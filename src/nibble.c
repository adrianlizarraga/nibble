#include "nibble.h"
#include "compiler.h"
#include "parser.h"
#include "bytecode.h"
#include "code_gen.h"
#include "path_utils.h"
#include "os_utils.h"

static NibbleCtx* nibble;

const char* os_names[NUM_OS] = {
    [OS_LINUX] = "linux",
    [OS_WIN32] = "win32",
    [OS_OSX] = "osx",
};

const char* arch_names[NUM_ARCH] = {
    [ARCH_X86] = "x86",
    [ARCH_X64] = "x64",
};

const char* keyword_names[KW_COUNT];
const char* annotation_names[ANNOTATION_COUNT];

Identifier* intrinsic_idents[INTRINSIC_COUNT];
Identifier* builtin_struct_fields[BUILTIN_STRUCT_FIELD_COUNT];
Identifier* main_proc_ident;

bool slurp_file(StringView* contents, Allocator* allocator, const char* filename)
{
    FILE* fd = fopen(filename, "r");
    if (!fd) {
        NIBBLE_FATAL_EXIT("Failed to open file %s", filename);
        return false;
    }

    if (fseek(fd, 0, SEEK_END) < 0) {
        NIBBLE_FATAL_EXIT("Failed to read file %s", filename);
        return false;
    }

    long int size = ftell(fd);
    if (size < 0) {
        NIBBLE_FATAL_EXIT("Failed to read file %s", filename);
        return false;
    }

    char* buf = mem_allocate(allocator, size + 1, DEFAULT_ALIGN, false);
    if (!buf) {
        NIBBLE_FATAL_EXIT("Out of memory: %s:%d", __FILE__, __LINE__);
        return false;
    }

    if (fseek(fd, 0, SEEK_SET) < 0) {
        NIBBLE_FATAL_EXIT("Failed to read file %s", filename);
        return false;
    }

    size_t n = fread(buf, 1, (size_t)size, fd);
    if (ferror(fd)) {
        NIBBLE_FATAL_EXIT("Failed to read file %s", filename);
        return false;
    }

    fclose(fd);
    buf[n] = '\0';

    contents->str = buf;
    contents->len = n;

    return true;
}

void report_error(ProgRange range, const char* format, ...)
{
    char buf[MAX_ERROR_LEN];
    va_list vargs;

    va_start(vargs, format);
    size_t size = vsnprintf(buf, MAX_ERROR_LEN, format, vargs) + 1;
    va_end(vargs);

    if (size >= MAX_ERROR_LEN) {
        buf[MAX_ERROR_LEN - 1] = '\0';
        size = MAX_ERROR_LEN;
    }

    error_stream_add(&nibble->errors, range, buf, size);
}

void error_stream_init(ErrorStream* stream, Allocator* allocator)
{
    memset(stream, 0, sizeof(ErrorStream));
    stream->allocator = allocator;
}

void error_stream_free(ErrorStream* stream)
{
    Error* err = stream->first;

    while (err) {
        Error* next = err->next;
        mem_free(stream->allocator, err);
        err = next;
    }

    stream->first = stream->last = NULL;
}

void error_stream_add(ErrorStream* stream, ProgRange range, const char* msg, size_t size)
{
    if (stream) {
        size_t err_size = offsetof(Error, msg) + size;
        Error* err = mem_allocate(stream->allocator, err_size, DEFAULT_ALIGN, false);

        if (err) {
            memcpy(err->msg, msg, size);
            err->size = size;
            err->next = NULL;
            err->range = range;

            if (!stream->first)
                stream->last = stream->first = err;
            else
                stream->last = stream->last->next = err;

            stream->count += 1;
        }
    }
}

typedef struct SourceFile {
    ProgRange range;
    const char* code;
    const char* cpath_str;
    size_t cpath_len;

    ProgPos* line_pos; // Stretchy array for now
} SourceFile;

static SourceFile* add_src_file(NibbleCtx* ctx, const char* cpath_str, size_t cpath_len, const char* code, size_t code_len)
{
    SourceFile* src_file = alloc_type(&ctx->gen_mem, SourceFile, true);
    src_file->range.start = ctx->src_pos;
    src_file->range.end = ctx->src_pos + (ProgPos)code_len;
    src_file->code = code;
    src_file->cpath_str = cpath_str;
    src_file->cpath_len = cpath_len;
    src_file->line_pos = array_create(&ctx->gen_mem, ProgPos, 32);

    ctx->src_pos = src_file->range.end;

    bucket_list_add_elem(&ctx->src_files, src_file);

    return src_file;
}

static SourceFile* get_src_file(NibbleCtx* ctx, ProgPos pos)
{
    size_t num_files = ctx->src_files.num_elems;

    for (size_t i = 0; i < num_files; i += 1) {
        void** elem_ptr = bucket_list_get_elem_packed(&ctx->src_files, i);
        assert(elem_ptr);

        SourceFile* src_file = (SourceFile*)*elem_ptr;

        if (pos < src_file->range.end) {
            return src_file;
        }
    }

    return NULL;
}

typedef struct LineCol {
    unsigned line;
    unsigned col;
} LineCol;

static LineCol get_src_linecol(SourceFile* src_file, ProgPos pos)
{
    assert(pos >= src_file->range.start);
    assert(pos <= src_file->range.end);

    LineCol result = {0};

    size_t num_line_pos = array_len(src_file->line_pos);
    size_t line_pos_i = 0;

    for (; line_pos_i < num_line_pos; line_pos_i += 1) {
        ProgPos line_end = src_file->line_pos[line_pos_i];
        assert(line_end <= src_file->range.end);

        if (pos < line_end) {
            break;
        }
    }

    ProgPos line_start = line_pos_i == 0 ? src_file->range.start : src_file->line_pos[line_pos_i - 1];

    result.line = line_pos_i + 1;
    result.col = pos - line_start + 1;

    return result;
}

#define RED_COLOR_CODE "\x1B[31m"
#define RESET_COLOR_CODE "\x1B[0m"

static void print_error(Error* error, bool use_colors)
{
    SourceFile* src_file = get_src_file(nibble, error->range.start);
    assert(src_file);

    LineCol linecol_s = get_src_linecol(src_file, error->range.start);

    Path src_ospath;
    cpath_str_to_ospath(&nibble->tmp_mem, &src_ospath, src_file->cpath_str, src_file->cpath_len, &nibble->base_ospath);

    ftprint_err("%.*s:%u:%u: [Error]: %s\n\n", src_ospath.len, src_ospath.str, linecol_s.line, linecol_s.col, error->msg);

    unsigned line = linecol_s.line;

    ftprint_err(" %5d | ", line);

    for (ProgPos p = (error->range.start - (linecol_s.col - 1)); p < error->range.start; p += 1) {
        size_t i = p - src_file->range.start;
        char ch = src_file->code[i];

        if (ch == '\n') {
            line += 1;
            ftprint_err("\n %5d | ", line);
        }
        else {
            ftprint_err("%c", src_file->code[i]);
        }
    }

    if (use_colors) {
        ftprint_err(RED_COLOR_CODE);
    }

    for (ProgPos p = error->range.start; p < error->range.end; p += 1) {
        size_t i = p - src_file->range.start;
        char ch = src_file->code[i];

        if (ch == '\n') {
            line += 1;

            if (use_colors) {
                ftprint_err(RESET_COLOR_CODE "\n %5d | " RED_COLOR_CODE, line);
            }
            else {
                ftprint_err("\n %5d | ", line);
            }
        }
        else {
            ftprint_err("%c", src_file->code[i]);
        }
    }

    if (use_colors) {
        ftprint_err(RESET_COLOR_CODE);
    }

    const char* p = src_file->code + (error->range.end - src_file->range.start);

    while (*p && (*p != '\n')) {
        ftprint_err("%c", *p);
        p += 1;
    }

    ftprint_err("\n\n");
}

static void print_errors(ErrorStream* errors)
{
    if (errors->count > 0) {
        bool use_colors = is_stderr_atty();

        ftprint_err("\n%u errors:\n\n", errors->count);
        Error* err = errors->first;

        while (err) {
            print_error(err, use_colors);
            err = err->next;
        }
    }
}

static bool init_annotations()
{
    static const StringView names[ANNOTATION_COUNT] = {
        [ANNOTATION_EXPORTED] = string_view_lit("exported"),
        [ANNOTATION_FOREIGN] = string_view_lit("foreign"),
        [ANNOTATION_PACKED] = string_view_lit("packed"),
    };

    for (int i = 0; i < ANNOTATION_COUNT; i += 1) {
        const StringView* str_view = names + i;
        Identifier* ident = intern_ident(str_view->str, str_view->len);

        if (!ident) {
            return false;
        }

        annotation_names[i] = ident->str;
    }

    return true;
}

static bool init_builtin_struct_fields()
{
    static const StringView names[BUILTIN_STRUCT_FIELD_COUNT] = {
        [BUILTIN_STRUCT_FIELD_LENGTH] = string_view_lit("length"),
        [BUILTIN_STRUCT_FIELD_DATA] = string_view_lit("data"),
        [BUILTIN_STRUCT_FIELD_TYPE] = string_view_lit("type"),
        [BUILTIN_STRUCT_FIELD_PTR] = string_view_lit("ptr"),
    };

    for (int i = 0; i < BUILTIN_STRUCT_FIELD_COUNT; i += 1) {
        const StringView* str_view = names + i;
        Identifier* ident = intern_ident(str_view->str, str_view->len);

        if (!ident) {
            return false;
        }

        builtin_struct_fields[i] = ident;
    }

    return true;
}

/*
// Code for the builtin module that is automatically imported everywhere:
// Generated by: xxd -i builtin.nib
// NOTE: A terminating null character has to be added to the output of xxd

proc #writeout(buf: ^char, size: usize) => ssize;
proc #readin(buf: ^char, size: usize) => ssize;
proc #memcpy(dst: ^void, src: ^void, size: usize);
proc #memset(dst: ^void, value: uchar, size: usize);
*/
static const char builtin_code[] = {
    0x70, 0x72, 0x6f, 0x63, 0x20, 0x23, 0x77, 0x72, 0x69, 0x74, 0x65, 0x6f, 0x75, 0x74, 0x28, 0x62, 0x75, 0x66, 0x3a, 0x20, 0x5e,
    0x63, 0x68, 0x61, 0x72, 0x2c, 0x20, 0x73, 0x69, 0x7a, 0x65, 0x3a, 0x20, 0x75, 0x73, 0x69, 0x7a, 0x65, 0x29, 0x20, 0x3d, 0x3e,
    0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x3b, 0x0a, 0x70, 0x72, 0x6f, 0x63, 0x20, 0x23, 0x72, 0x65, 0x61, 0x64, 0x69, 0x6e, 0x28,
    0x62, 0x75, 0x66, 0x3a, 0x20, 0x5e, 0x63, 0x68, 0x61, 0x72, 0x2c, 0x20, 0x73, 0x69, 0x7a, 0x65, 0x3a, 0x20, 0x75, 0x73, 0x69,
    0x7a, 0x65, 0x29, 0x20, 0x3d, 0x3e, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x3b, 0x0a, 0x70, 0x72, 0x6f, 0x63, 0x20, 0x23, 0x6d,
    0x65, 0x6d, 0x63, 0x70, 0x79, 0x28, 0x64, 0x73, 0x74, 0x3a, 0x20, 0x5e, 0x76, 0x6f, 0x69, 0x64, 0x2c, 0x20, 0x73, 0x72, 0x63,
    0x3a, 0x20, 0x5e, 0x76, 0x6f, 0x69, 0x64, 0x2c, 0x20, 0x73, 0x69, 0x7a, 0x65, 0x3a, 0x20, 0x75, 0x73, 0x69, 0x7a, 0x65, 0x29,
    0x3b, 0x0a, 0x70, 0x72, 0x6f, 0x63, 0x20, 0x23, 0x6d, 0x65, 0x6d, 0x73, 0x65, 0x74, 0x28, 0x64, 0x73, 0x74, 0x3a, 0x20, 0x5e,
    0x76, 0x6f, 0x69, 0x64, 0x2c, 0x20, 0x76, 0x61, 0x6c, 0x75, 0x65, 0x3a, 0x20, 0x75, 0x63, 0x68, 0x61, 0x72, 0x2c, 0x20, 0x73,
    0x69, 0x7a, 0x65, 0x3a, 0x20, 0x75, 0x73, 0x69, 0x7a, 0x65, 0x29, 0x3b, 0x0a, 0x00};

static bool init_intrinsics()
{
    static const StringView names[INTRINSIC_COUNT] = {
        [INTRINSIC_READIN] = string_view_lit("#readin"),
        [INTRINSIC_WRITEOUT] = string_view_lit("#writeout"),
        [INTRINSIC_MEMCPY] = string_view_lit("#memcpy"),
        [INTRINSIC_MEMSET] = string_view_lit("#memset"),
    };

    for (int i = 0; i < INTRINSIC_COUNT; i += 1) {
        const StringView* str_view = names + i;
        Identifier* ident = intern_ident(str_view->str, str_view->len);

        if (!ident) {
            return false;
        }

        ident->kind = IDENTIFIER_INTRINSIC;
        ident->intrinsic = (Intrinsic)i;

        intrinsic_idents[i] = ident;
    }

    return true;
}

static bool init_keywords()
{
    static const StringView names[KW_COUNT] = {
        [KW_VAR] = string_view_lit("var"),
        [KW_CONST] = string_view_lit("const"),
        [KW_ENUM] = string_view_lit("enum"),
        [KW_UNION] = string_view_lit("union"),
        [KW_STRUCT] = string_view_lit("struct"),
        [KW_PROC] = string_view_lit("proc"),
        [KW_TYPEDEF] = string_view_lit("typedef"),
        [KW_SIZEOF] = string_view_lit("#sizeof"),
        [KW_TYPEOF] = string_view_lit("#typeof"),
        [KW_TYPEID] = string_view_lit("#typeid"),
        [KW_INDEXOF] = string_view_lit("#indexof"),
        [KW_OFFSETOF] = string_view_lit("#offsetof"),
        [KW_LENGTH] = string_view_lit("#length"),
        [KW_STATIC_ASSERT] = string_view_lit("#static_assert"),
        [KW_RET_TYPE] = string_view_lit("#ret_type"),
        [KW_EXPORT] = string_view_lit("export"),
        [KW_IMPORT] = string_view_lit("import"),
        [KW_FROM] = string_view_lit("from"),
        [KW_AS] = string_view_lit("as"),
        [KW_INCLUDE] = string_view_lit("#include"),
        [KW_LABEL] = string_view_lit("label"),
        [KW_GOTO] = string_view_lit("goto"),
        [KW_BREAK] = string_view_lit("break"),
        [KW_CONTINUE] = string_view_lit("continue"),
        [KW_RETURN] = string_view_lit("return"),
        [KW_IF] = string_view_lit("if"),
        [KW_ELSE] = string_view_lit("else"),
        [KW_WHILE] = string_view_lit("while"),
        [KW_DO] = string_view_lit("do"),
        [KW_FOR] = string_view_lit("for"),
        [KW_SWITCH] = string_view_lit("switch"),
        [KW_CASE] = string_view_lit("case"),
        [KW_UNDERSCORE] = string_view_lit("_"),
    };

    for (int i = 0; i < KW_COUNT; i += 1) {
        const StringView* str_view = names + i;
        Identifier* ident = intern_ident(str_view->str, str_view->len);

        if (!ident) {
            return false;
        }

        ident->kind = IDENTIFIER_KEYWORD;
        ident->kw = (Keyword)i;

        keyword_names[i] = ident->str;
    }

    return true;
}

bool nibble_init(OS target_os, Arch target_arch)
{
    static const char main_name[] = "main";

    // First, check host/target archs
#ifdef NIBBLE_HOST_LINUX
    if (target_arch != ARCH_X64 || target_os != OS_LINUX) {
        ftprint_err("[ERROR]: Target OS (%s %s) not yet supported on Linux.\n", os_names[target_os], arch_names[target_arch]);
        return false;
    }
#elif defined(NIBBLE_HOST_WINDOWS)
    if (target_arch != ARCH_X64 || target_os != OS_WIN32) {
        ftprint_err("[ERROR]: Target OS (%s %s) not yet supported on Windows.\n", os_names[target_os], arch_names[target_arch]);
        return false;
    }
#else
    ftprint_err("[ERROR]: Target OS (%s %s) not supported on `Unknown` OS.\n", os_names[target_os], arch_names[target_arch]);
    exit(1);
#endif

    Allocator bootstrap = allocator_create(32768);
    nibble = alloc_type(&bootstrap, NibbleCtx, true);

    nibble->target_os = target_os;
    nibble->target_arch = target_arch;
    nibble->gen_mem = bootstrap;
    nibble->ast_mem = allocator_create(16384);
    nibble->tmp_mem = allocator_create(4096);
    nibble->str_lit_map = hmap(6, NULL);
    nibble->ident_map = hmap(8, NULL);
    nibble->mod_map = hmap(6, NULL);
    nibble->type_cache.ptrs = hmap(6, NULL);
    nibble->type_cache.arrays = hmap(6, NULL);
    nibble->type_cache.procs = hmap(6, NULL);
    nibble->type_cache.slices = hmap(6, NULL);
    nibble->type_cache.structs = hmap(6, NULL);
    nibble->type_cache.unions = hmap(6, NULL);

    if (!init_keywords())
        return false;

    if (!init_intrinsics())
        return false;

    if (!init_annotations())
        return false;

    if (!init_builtin_struct_fields())
        return false;

    main_proc_ident = intern_ident(main_name, sizeof(main_name) - 1);

    assert(nibble->ident_map.len == (KW_COUNT + ANNOTATION_COUNT + INTRINSIC_COUNT + BUILTIN_STRUCT_FIELD_COUNT + 1));

    bucket_list_init(&nibble->src_files, &nibble->gen_mem, 16);
    bucket_list_init(&nibble->vars, &nibble->ast_mem, 32);
    bucket_list_init(&nibble->procs, &nibble->ast_mem, 32);
    bucket_list_init(&nibble->aggregate_types, &nibble->ast_mem, 16);
    bucket_list_init(&nibble->str_lits, &nibble->ast_mem, 8);

    error_stream_init(&nibble->errors, &nibble->gen_mem);

    init_builtin_types(target_os, target_arch, &nibble->ast_mem, &nibble->type_cache);

    if (!init_code_gen(target_os, target_arch))
        return false;

    return true;
}

static Module* add_module(NibbleCtx* ctx, StrLit* cpath_lit)
{
    Module* mod = alloc_type(&ctx->ast_mem, Module, true);

    module_init(mod, cpath_lit);
    hmap_put(&ctx->mod_map, PTR_UINT(cpath_lit), PTR_UINT(mod));

    return mod;
}

static Module* get_module(NibbleCtx* ctx, StrLit* cpath_lit)
{
    uint64_t* pval = hmap_get(&ctx->mod_map, PTR_UINT(cpath_lit));
    Module* mod = pval ? (void*)*pval : NULL;

    return mod;
}

static bool add_builtin_type_symbol(NibbleCtx* ctx, const char* name, Type* type)
{
    Identifier* sym_name = intern_ident(name, cstr_len(name));
    Module* builtin_mod = ctx->builtin_mod;

    if (lookup_scope_symbol(&builtin_mod->scope, sym_name)) {
        ProgRange range = {0};
        report_error(range, "[INTERNAL ERROR] Duplicate definition of builtin `%s`", sym_name);
        return false;
    }

    Symbol* sym = new_symbol_builtin_type(&ctx->ast_mem, sym_name, type, builtin_mod);

    add_scope_symbol(&builtin_mod->scope, sym_name, sym, true);

    return true;
}

static void init_builtin_syms(NibbleCtx* ctx)
{
    size_t num_types = ARRAY_LEN(builtin_types);

    for (size_t i = 0; i < num_types; i += 1) {
        BuiltinType* builtin = builtin_types + i;

        add_builtin_type_symbol(ctx, builtin->name, builtin->type);
    }
}

#define NIBBLE_INCLUDE_LIMIT 50

typedef struct CachedInclude {
    struct CachedInclude* next;
    Path* includer_ospath;
    size_t len;
    char str[];
} CachedInclude;

static bool parse_code_recursive(NibbleCtx* ctx, Module* mod, const char* cpath_str, size_t cpath_len, const char* code,
                                 size_t code_len, HMap* seen_includes, int include_depth)
{
    ProgPos src_pos = ctx->src_pos;
    SourceFile* src_file = add_src_file(ctx, cpath_str, cpath_len, code, code_len);

    Parser parser = {0};
    parser_init(&parser, &ctx->ast_mem, &ctx->tmp_mem, code, src_pos, &ctx->errors, &src_file->line_pos);

    next_token(&parser);

    while (!is_token_kind(&parser, TKN_EOF)) {
        Stmt* stmt = parse_global_stmt(&parser);

        if (!stmt || nibble->errors.count)
            return false;

        if (stmt->kind == CST_StmtInclude) {
            StmtInclude* stmt_include = (StmtInclude*)stmt;

            Path file_ospath;
            cpath_str_to_ospath(&ctx->tmp_mem, &file_ospath, cpath_str, cpath_len, &ctx->base_ospath);

            if (include_depth > NIBBLE_INCLUDE_LIMIT) {
                report_error(stmt->range, "Include limit exceeded. File include chain exceeded the current threshold of `%d`.",
                             NIBBLE_INCLUDE_LIMIT);
                return false;
            }

            Path include_ospath;
            NibblePathErr ret =
                get_import_ospath(&include_ospath, stmt_include->file_pathname, &ctx->base_ospath, &file_ospath, &ctx->tmp_mem);

            // Check if included file's path exists somewhere.
            if (ret == NIB_PATH_INV_PATH) {
                report_error(stmt->range, "Invalid include file path \"%s\"", stmt_include->file_pathname->str);
                return false;
            }

            // Check for .nib extension.
            if (ret == NIB_PATH_INV_EXT) {
                report_error(stmt->range, "Included file \"%s\" does not end in `.%s`", stmt_include->file_pathname->str, nib_ext);
                return false;
            }

            assert(ret == NIB_PATH_OK);
            Path include_cpath;
            ret = ospath_to_cpath(&include_cpath, &include_ospath, &ctx->base_ospath, &ctx->tmp_mem);

            // Check that include path is inside the project's root directory.
            if (ret == NIB_PATH_OUTSIDE_ROOT) {
                report_error(stmt->range, "Relative include path \"%s\" is outside of project root dir \"%s\"",
                             stmt_include->file_pathname->str, ctx->base_ospath.str);
                return false;
            }

            assert(ret == NIB_PATH_OK);

            // Check that the include file is not the same as the current file.
            if (cstr_ncmp(cpath_str, include_cpath.str, include_cpath.len) == 0) {
                report_error(stmt->range, "Cyclic file inclusion detected at file `%s`. Cannot include self.", file_ospath.str);
                return false;
            }

            // Check if the file we're trying to include is in the seen_includes table.
            // If yes, fail. Otherwise, add.
            u64 key = hash_bytes(include_cpath.str, include_cpath.len, FNV_INIT);
            CachedInclude* cached_include = NULL;
            bool seen = false;

            // Look to see if this include file has already been seen.
            {
                u64* pval = hmap_get(seen_includes, key);

                if (pval) {
                    CachedInclude* cached = (void*)*pval;

                    for (CachedInclude* it = cached; it; it = it->next) {
                        if ((it->len == include_cpath.len) && (cstr_ncmp(it->str, include_cpath.str, include_cpath.len) == 0)) {
                            seen = true;
                            cached_include = it;
                            break;
                        }
                    }
                }
            }

            if (seen) {
                report_error(stmt->range, "Cyclic file inclusion detected.\nFile `%s` was first included by `%s`", include_ospath.str,
                             cached_include->includer_ospath->str);
                return false;
            }

            // Add to seen_includes
            {
                CachedInclude* new_cached_include =
                    mem_allocate(&ctx->tmp_mem, offsetof(CachedInclude, str) + include_cpath.len + 1, DEFAULT_ALIGN, true);

                if (!new_cached_include) {
                    NIBBLE_FATAL_EXIT("Out of memory: %s:%d", __FILE__, __LINE__);
                    return false;
                }

                new_cached_include->next = cached_include;
                new_cached_include->len = include_cpath.len;
                new_cached_include->includer_ospath = &file_ospath;

                memcpy(new_cached_include->str, include_cpath.str, include_cpath.len);
                new_cached_include->str[include_cpath.len] = '\0';

                hmap_put(seen_includes, key, (uintptr_t)new_cached_include);
            }

            StringView included_code;

            if (!slurp_file(&included_code, &ctx->gen_mem, include_ospath.str)) {
                return false;
            }

            if (!parse_code_recursive(ctx, mod, include_cpath.str, include_cpath.len, included_code.str, included_code.len,
                                      seen_includes, include_depth + 1)) {
                return false;
            }
        }
        else if (stmt->kind == CST_StmtImport) {
            StmtImport* stmt_import = (StmtImport*)stmt;

            // NOTE: A namespaced import only really imports one new symbol into a module.
            // NOTE: TODO: This is inaccurate for greedy imports (import all symbols raw)
            size_t num_imports = stmt_import->mod_namespace ? 1 : stmt_import->num_imports;

            mod->num_imports += num_imports;
            list_add_last(&mod->import_stmts, &stmt->lnode);
        }
        else if (stmt->kind == CST_StmtExport) {
            StmtExport* stmt_export = (StmtExport*)stmt;

            mod->num_exports += stmt_export->num_exports;
            list_add_last(&mod->export_stmts, &stmt->lnode);
        }
        else {
            if (stmt->kind == CST_StmtDecl) {
                StmtDecl* stmt_decl = (StmtDecl*)stmt;

                mod->num_decls += 1;
                if (stmt_decl->decl->flags & DECL_IS_EXPORTED)
                    mod->num_exports += 1;
            }

            list_add_last(&mod->stmts, &stmt->lnode);
        }

#ifdef NIBBLE_PRINT_IRS
        ftprint_out("%s\n", ftprint_stmt(&ctx->gen_mem, stmt));
#endif
    }

#ifdef NIBBLE_PRINT_IRS
    ftprint_out("\n");
#endif

    return true;
}

static bool parse_code(NibbleCtx* ctx, Module* mod, const char* code, size_t code_len)
{
    AllocatorState tmp_state = allocator_get_state(&ctx->tmp_mem);
    HMap seen_includes = hmap(3, &ctx->tmp_mem);

    mod->range.start = ctx->src_pos;

    bool ret = parse_code_recursive(ctx, mod, mod->cpath_lit->str, mod->cpath_lit->len, code, code_len, &seen_includes, 0);

    mod->range.end = ctx->src_pos;

    allocator_restore_state(tmp_state);

    return ret;
}

static bool parse_module(NibbleCtx* ctx, Module* mod);

static Module* parse_import_module(NibbleCtx* ctx, const char* path, size_t len)
{
    StrLit* cpath_lit = intern_str_lit(path, len);
    Module* mod = get_module(ctx, cpath_lit);

    if (!mod) {
        mod = add_module(ctx, cpath_lit);

        if (!parse_module(ctx, mod)) {
            return NULL;
        }
    }

    return mod;
}

bool import_builtin_syms(NibbleCtx* ctx, Module* mod)
{
    Module* src_mod = ctx->builtin_mod;
    List* head = &src_mod->scope.sym_list;
    List* it = head->next;

    while (it != head) {
        Symbol* sym = list_entry(it, Symbol, lnode);

        if (!module_add_global_sym(mod, sym->name, sym)) {
            return false;
        }

        it = it->next;
    }

    return true;
}

static bool parse_module(NibbleCtx* ctx, Module* mod)
{
    ftprint_out("[INFO]: Parsing module %s ...\n", mod->cpath_lit->str);

    mod->is_parsing = true;

    AllocatorState mem_state = allocator_get_state(&ctx->tmp_mem);

    // Parse the code text
    Path mod_ospath;
    cpath_str_to_ospath(&ctx->tmp_mem, &mod_ospath, mod->cpath_lit->str, mod->cpath_lit->len, &ctx->base_ospath);

    StringView code;

    if (!slurp_file(&code, &ctx->gen_mem, mod_ospath.str)) {
        return false;
    }

    if (!parse_code(ctx, mod, code.str, code.len)) {
        return false;
    }

    module_init_tables(mod, &ctx->ast_mem, ctx->num_builtins);

    // Import all builtin symbols.
    if (!import_builtin_syms(ctx, mod)) {
        return false;
    }

    // Install unresolved decls into the module's symbol table, and install exported decls into
    // the module's export table.
    if (!install_module_decls(&ctx->ast_mem, mod)) {
        return false;
    }

    // Process imports.
    {
        List* head = &mod->import_stmts;
        List* it = head->next;

        while (it != head) {
            Stmt* stmt = list_entry(it, Stmt, lnode);

            assert(stmt->kind == CST_StmtImport);
            StmtImport* simport = (StmtImport*)stmt;

            // TODO: IMPORTANT: Cache canonical import module paths to avoid expensive path construction
            // and validation!!!
            //
            // Key: Current canonical directory + raw import path
            // Val: A valid/existing canonical path for the imported module

            //
            // Create a canonical module path from import path.
            //
            // 1. Create an absolute OS path to the imported module and check if it exists.
            // 2. If it exists, subtract base_ospath to generate the canonical module path.

            Path import_mod_ospath;
            NibblePathErr ret_err =
                get_import_ospath(&import_mod_ospath, simport->mod_pathname, &ctx->base_ospath, &mod_ospath, &ctx->tmp_mem);
            // Check if imported module's path exists somewhere.
            if (ret_err == NIB_PATH_INV_PATH) {
                report_error(stmt->range, "Invalid module import path \"%s\"", simport->mod_pathname->str);
                return false;
            }

            // Check for .nib extension.
            if (ret_err == NIB_PATH_INV_EXT) {
                report_error(stmt->range, "Imported module file \"%s\" does not end in `.%s`", simport->mod_pathname->str, nib_ext);
                return false;
            }

            assert(ret_err == NIB_PATH_OK);

            // Try to create a canonical module path (where `/` corresponds to main's home directory).
            Path import_cpath;
            ret_err = ospath_to_cpath(&import_cpath, &import_mod_ospath, &ctx->base_ospath, &ctx->tmp_mem);

            // Check that import module path is inside the project's root directory.
            if (ret_err == NIB_PATH_OUTSIDE_ROOT) {
                report_error(stmt->range, "Relative module import path \"%s\" is outside of project root dir \"%s\"",
                             simport->mod_pathname->str, ctx->base_ospath.str);
                return false;
            }

            assert(ret_err == NIB_PATH_OK);

            //
            // Parse import module
            //

            Module* import_mod = parse_import_module(ctx, import_cpath.str, import_cpath.len);

            if (!import_mod) {
                return false;
            }

            if (import_mod->is_parsing) {
                report_error(stmt->range, "Cyclic import \"%s\" detected", simport->mod_pathname->str);
                return false;
            }

            bool have_import_syms = !list_empty(&simport->import_syms);
            bool have_import_ns = simport->mod_namespace != NULL;

            if (have_import_ns) {
                Symbol* import_mod_sym = new_symbol_mod(&ctx->ast_mem, simport, import_mod, mod);

                if (!module_add_global_sym(mod, import_mod_sym->name, import_mod_sym)) {
                    return false;
                }
            }
            else if (have_import_syms) {
                if (!import_mod_syms(mod, import_mod, simport))
                    return false;
            }
            else {
                if (!import_all_mod_syms(mod, import_mod))
                    return false;
            }

            it = it->next;
        }
    }

    // Process exports
    {
        List* head = &mod->export_stmts;
        List* it = head->next;

        while (it != head) {
            Stmt* stmt = list_entry(it, Stmt, lnode);

            assert(stmt->kind == CST_StmtExport);
            StmtExport* sexport = (StmtExport*)stmt;

            List* h = &sexport->export_syms;

            for (List* i = h->next; i != h; i = i->next) {
                ExportSymbol* esym = list_entry(i, ExportSymbol, lnode);

                // Lookup the symbol we're trying to export.
                Symbol* sym = NULL;
                {
                    List* e_head = &esym->ns_ident.idents;
                    List* e_it = e_head->next;

                    IdentNode* inode = list_entry(e_it, IdentNode, lnode);
                    sym = lookup_scope_symbol(&mod->scope, inode->ident);
                    e_it = e_it->next;

                    // Keep looking through module namespaces.
                    while (sym && (e_it != e_head)) {
                        if (sym->kind != SYMBOL_MODULE) {
                            report_error(esym->range, "Namespace `%s` in export statement is not a module", inode->ident->str);
                            return false;
                        }

                        inode = list_entry(e_it, IdentNode, lnode);

                        StmtImport* stmt = (StmtImport*)sym->as_mod.stmt;
                        Identifier* sym_name = get_import_sym_name(stmt, inode->ident);

                        sym = module_get_export_sym(sym->as_mod.mod, sym_name);
                        e_it = e_it->next;
                    }

                    if (!sym) {
                        report_error(esym->range, "Unknown export symbol `%s`", inode->ident->str);
                        return false;
                    }
                }

                // Namespaced symbols must be renamed. Ex: export {MyMod::foo as foo};
                if ((esym->ns_ident.num_idents > 1) && !esym->rename) {
                    report_error(esym->range, "Exported symbol `%s` must be renamed due to module namespace",
                                 ftprint_ns_ident(&ctx->tmp_mem, &esym->ns_ident));
                    return false;
                }

                // Prevent users from exporting builtin symbols.
                if (sym->home == ctx->builtin_mod) {
                    report_error(esym->range, "Cannot export builtin symbol `%s`", ftprint_ns_ident(&ctx->tmp_mem, &esym->ns_ident));
                    return false;
                }

                // Add symbol to the module's export table
                Identifier* exp_name = esym->rename;

                if (!esym->rename) {
                    IdentNode* last_inode = list_entry(esym->ns_ident.idents.prev, IdentNode, lnode);
                    exp_name = last_inode->ident;
                }

                if (!module_add_export_sym(mod, exp_name, sym)) {
                    report_error(esym->range, "Conflicting export symbol name `%s`", exp_name->str);
                    return false;
                }
            }

            it = it->next;
        }
    }

    allocator_restore_state(mem_state);
    mod->is_parsing = false; // IMPORTANT: This should be only successful exit point for this procedure.
    return true;
}

bool nibble_compile(const char* mainf_name, size_t mainf_len, const char* outf_name, size_t outf_len)
{
    AllocatorState mem_state = allocator_get_state(&nibble->tmp_mem);

    //////////////////////////////////////////
    //      Check output file name
    //////////////////////////////////////////
    Path outf_ospath;
    path_init(&outf_ospath, &nibble->tmp_mem);
    path_set(&outf_ospath, outf_name, outf_len);

    const char* outf_ext = path_ext(&outf_ospath);

    if ((nibble->target_os == OS_WIN32) && (cstr_cmp(outf_ext, exe_ext) != 0 || outf_ext == outf_ospath.str)) {
        path_append(&outf_ospath, dot_exe_ext, sizeof(dot_exe_ext) - 1);
    }

    // TODO: Validate output file name more extensively.

    //////////////////////////////////////////
    //      Check main file validity
    //////////////////////////////////////////
    static const char builtin_mod_name[] = "/_nibble_builtin";

    Path main_path;
    path_init(&main_path, &nibble->tmp_mem);
    path_set(&main_path, mainf_name, mainf_len);

    if (!path_abs(&main_path)) {
        ftprint_err("[ERROR]: Cannot find file `%s`\n", mainf_name);
        return false;
    }

    FileKind file_kind = path_kind(&main_path);

    if ((file_kind != FILE_REG) || cstr_cmp(path_ext(&main_path), nib_ext) != 0) {
        ftprint_err("[ERROR]: Program entry file `%s` is not a valid `.nib` source file.\n", main_path.str);
        return false;
    }

    /////////////////////////////////////////////////////////
    //      Get main file's module path (e.g., /main.nib)
    //      and extract the base OS path.
    /////////////////////////////////////////////////////////
    const char* filename_ptr = path_filename(&main_path);
    assert(filename_ptr != main_path.str);
    const char* base_path_end_ptr = filename_ptr - 1;

    Path* base_ospath = &nibble->base_ospath;
    path_init(base_ospath, &nibble->gen_mem);
    path_set(base_ospath, main_path.str, (base_path_end_ptr - main_path.str));

    ftprint_out("[INFO]: Base project OS path: %s\n", base_ospath->str);

    size_t cpath_cap = ((main_path.str + main_path.len) - base_path_end_ptr) + 1;
    char* entry_cpath_buf = array_create(&nibble->tmp_mem, char, cpath_cap);
    ftprint_char_array(&entry_cpath_buf, true, "%c%s", NIBBLE_PATH_SEP, filename_ptr);

    // Main module
    Module* main_mod = add_module(nibble, intern_str_lit(entry_cpath_buf, cstr_len(entry_cpath_buf)));

    // Builtin module
    Module* builtin_mod = add_module(nibble, intern_str_lit(builtin_mod_name, sizeof(builtin_mod_name) - 1));
    nibble->builtin_mod = builtin_mod;

    //////////////////////////////////////////
    //                Parse
    //////////////////////////////////////////
    const size_t num_builtin_types = ARRAY_LEN(builtin_types);
    const size_t builtin_code_len = cstr_len(builtin_code);
    bool parse_ok = parse_code(nibble, builtin_mod, builtin_code, builtin_code_len);

    if (!parse_ok) {
        ftprint_err("[ERROR]: Failed to parse builtin code\n");
        print_errors(&nibble->errors);
        return false;
    }

    nibble->num_builtins = builtin_mod->num_decls + num_builtin_types;
    builtin_mod->scope.sym_table = hmap(calc_hmap_size(nibble->num_builtins << 1), &nibble->ast_mem);

    init_builtin_syms(nibble);

    if (!install_module_decls(&nibble->ast_mem, builtin_mod)) {
        print_errors(&nibble->errors);
        return false;
    }

    // Parse main module.
    if (!parse_module(nibble, main_mod)) {
        print_errors(&nibble->errors);
        return false;
    }

    // Look for main to have been parsed and installed as an unresolved proc symbol.
    Symbol* main_sym = lookup_symbol(&main_mod->scope, main_proc_ident);

    if (!main_sym) {
        report_error(main_mod->range, "Program entry file must define a main() procedure.");
        print_errors(&nibble->errors);
        return false;
    }

    if (main_sym->kind != SYMBOL_PROC) {
        report_error(main_sym->decl->range, "Identifier `%s` must be a procedure, but found a %s.", main_proc_ident->str,
                     sym_kind_names[main_sym->kind]);
        print_errors(&nibble->errors);
        return false;
    }

    //////////////////////////////////////////
    //          Resolve/Typecheck
    //////////////////////////////////////////
    Resolver resolver = {.ctx = nibble};

    if (!resolve_module(&resolver, main_mod)) {
        print_errors(&nibble->errors);
        return false;
    }

    if (!resolve_reachable_sym_defs(&resolver)) {
        print_errors(&nibble->errors);
        return false;
    }

    // Ensure main has the expected type signature.
    Type* main_type = main_sym->type;
    assert(main_type->kind == TYPE_PROC);
    Type* main_ret_type = main_type->as_proc.ret;

    if (main_ret_type != builtin_types[BUILTIN_TYPE_INT].type) {
        DeclProc* main_decl = (DeclProc*)main_sym->decl;
        ProgRange err_range = main_decl->ret ? main_decl->ret->range : main_decl->super.range;

        report_error(err_range, "Main procedure must return an `int` (`%s`) type, but found `%s`.",
                     type_name(builtin_types[BUILTIN_TYPE_INT].type), type_name(main_ret_type));
        print_errors(&nibble->errors);
        return false;
    }

    size_t main_num_params = main_type->as_proc.num_params;

    // Check that params are either main(argc: int) or main(argc: int, argv: ^^char)
    if (main_num_params > 0) {
        DeclProc* main_decl = (DeclProc*)main_sym->decl;
        Type** param_types = main_type->as_proc.params;

        if (param_types[0] != builtin_types[BUILTIN_TYPE_INT].type) {
            DeclVar* param = (DeclVar*)list_entry(main_decl->params.next, Decl, lnode);

            report_error(param->typespec->range, "Main procedure's first paramater must be an `int` (`%s`) type, but found `%s`.",
                         type_name(builtin_types[BUILTIN_TYPE_INT].type), type_name(param_types[0]));
            print_errors(&nibble->errors);
            return false;
        }

        // TODO: Allow argv : []^char
        if ((main_num_params == 2) && (param_types[1] != type_ptr_ptr_char)) {
            DeclVar* param = (DeclVar*)list_entry(main_decl->params.next->next, Decl, lnode);

            report_error(param->typespec->range, "Main procedure's second paramater must be a `^^char` type, but found `%s`.",
                         type_name(param_types[1]));
            print_errors(&nibble->errors);
            return false;
        }

        // TODO: Allow/check for envp param
    }

    //////////////////////////////////////////
    //          Gen IR bytecode
    //////////////////////////////////////////
    ftprint_out("[INFO]: Generating IR ...\n");
    IR_gen_bytecode(&nibble->ast_mem, &nibble->tmp_mem, &nibble->vars, &nibble->procs, &nibble->str_lits, &nibble->type_cache);

    //////////////////////////////////////////
    //          Gen NASM output
    //////////////////////////////////////////
    const char nasm_ext[] = ".s";

    Path nasm_fname;
    path_init(&nasm_fname, &nibble->tmp_mem);
    path_set(&nasm_fname, outf_ospath.str, outf_ospath.len);
    path_append(&nasm_fname, nasm_ext, sizeof(nasm_ext) - 1);

    ftprint_out("[INFO]: Generating NASM assembly output: %s ...\n", nasm_fname.str);
    gen_module(&nibble->gen_mem, &nibble->tmp_mem, &nibble->vars, &nibble->procs, &nibble->str_lits, nasm_fname.str);

    //////////////////////////////////////////
    //          Run NASM assembler
    //////////////////////////////////////////
    assert(nibble->target_arch == ARCH_X64);

    char obj_ext_linux[] = ".o";
    char obj_ext_windows[] = ".obj";
    char nasm_fformat_linux[] = "elf64";
    char nasm_fformat_windows[] = "win64";

    char* obj_ext;
    char* nasm_fformat;
    size_t obj_ext_len;

    if (nibble->target_os == OS_LINUX) {
        obj_ext = obj_ext_linux;
        obj_ext_len = sizeof(obj_ext_linux) - 1;
        nasm_fformat = nasm_fformat_linux;
    }
    else {
        assert(nibble->target_os == OS_WIN32);
        obj_ext = obj_ext_windows;
        obj_ext_len = sizeof(obj_ext_windows) - 1;
        nasm_fformat = nasm_fformat_windows;
    }

    Path obj_fname;
    path_init(&obj_fname, &nibble->tmp_mem);
    path_set(&obj_fname, outf_ospath.str, outf_ospath.len);
    path_append(&obj_fname, obj_ext, obj_ext_len);

    char* nasm_cmd[] = {"nasm", "-f", nasm_fformat, nasm_fname.str, "-o", obj_fname.str, NULL};

    if (run_cmd(&nibble->tmp_mem, nasm_cmd, ARRAY_LEN(nasm_cmd) - 1) < 0) {
        ftprint_err("[ERROR]: NASM command failed\n");
        return false;
    }

    //////////////////////////////////////////
    //          Run linker
    //////////////////////////////////////////
    char* outf_name_dup = cstr_dup(&nibble->tmp_mem, outf_ospath.str);
    char* win_linker_out = array_create(&nibble->tmp_mem, char, 16);

    ftprint_char_array(&win_linker_out, true, "/out:%s", outf_name_dup);

    char* ld_cmd_linux[] = {"ld", "-o", outf_name_dup, obj_fname.str, NULL};
    // link /entry:_start /nodefaultlib /subsystem:console .\out.obj kernel32.lib user32.lib Shell32.lib
    char* ld_cmd_windows[] = {"link.exe",     obj_fname.str,  "/entry:_start", "/nodefaultlib", "/subsystem:console",
                              win_linker_out, "kernel32.lib", "user32.lib",    "Shell32.lib",   NULL};

    char** ld_cmd;
    int ld_cmd_argc;

    if (nibble->target_os == OS_LINUX) {
        ld_cmd = ld_cmd_linux;
        ld_cmd_argc = ARRAY_LEN(ld_cmd_linux) - 1;
    }
    else {
        assert(nibble->target_os == OS_WIN32);
        ld_cmd = ld_cmd_windows;
        ld_cmd_argc = ARRAY_LEN(ld_cmd_windows) - 1;
    }

    if (run_cmd(&nibble->tmp_mem, ld_cmd, ld_cmd_argc) < 0) {
        ftprint_err("[ERROR]: Linker command failed\n");
        return false;
    }

    allocator_restore_state(mem_state);
    return true;
}

void nibble_cleanup(void)
{
#ifdef NIBBLE_PRINT_MEM_USAGE
    print_allocator_stats(&nibble->gen_mem, "GEN mem stats");
    print_allocator_stats(&nibble->ast_mem, "AST mem stats");
    print_allocator_stats(&nibble->tmp_mem, "TMP mem stats");
    ftprint_out("Ident map: len = %lu, cap = %lu, total_size (malloc) = %lu\n", nibble->ident_map.len, nibble->ident_map.cap,
                nibble->ident_map.cap * sizeof(HMapEntry));
    ftprint_out("StrLit map: len = %lu, cap = %lu, total_size (malloc) = %lu\n", nibble->str_lit_map.len, nibble->str_lit_map.cap,
                nibble->str_lit_map.cap * sizeof(HMapEntry));
    ftprint_out("Module map: len = %lu, cap = %lu, total_size (malloc) = %lu\n", nibble->mod_map.len, nibble->mod_map.cap,
                nibble->mod_map.cap * sizeof(HMapEntry));
    ftprint_out("type_ptr cache: len = %lu, cap = %lu, total_size (malloc) = %lu\n", nibble->type_cache.ptrs.len,
                nibble->type_cache.ptrs.cap, nibble->type_cache.ptrs.cap * sizeof(HMapEntry));
    ftprint_out("type_array cache: len = %lu, cap = %lu, total_size (malloc) = %lu\n", nibble->type_cache.arrays.len,
                nibble->type_cache.arrays.cap, nibble->type_cache.arrays.cap * sizeof(HMapEntry));
    ftprint_out("type_proc cache: len = %lu, cap = %lu, total_size (malloc) = %lu\n", nibble->type_cache.procs.len,
                nibble->type_cache.procs.cap, nibble->type_cache.procs.cap * sizeof(HMapEntry));
    ftprint_out("type_slices cache: len = %lu, cap = %lu, total_size (malloc) = %lu\n", nibble->type_cache.slices.len,
                nibble->type_cache.slices.cap, nibble->type_cache.slices.cap * sizeof(HMapEntry));
    ftprint_out("type_structs cache: len = %lu, cap = %lu, total_size (malloc) = %lu\n", nibble->type_cache.structs.len,
                nibble->type_cache.structs.cap, nibble->type_cache.structs.cap * sizeof(HMapEntry));
    ftprint_out("type_unions cache: len = %lu, cap = %lu, total_size (malloc) = %lu\n", nibble->type_cache.unions.len,
                nibble->type_cache.unions.cap, nibble->type_cache.unions.cap * sizeof(HMapEntry));
#endif

    hmap_destroy(&nibble->str_lit_map);
    hmap_destroy(&nibble->ident_map);
    hmap_destroy(&nibble->mod_map);
    hmap_destroy(&nibble->type_cache.ptrs);
    hmap_destroy(&nibble->type_cache.procs);
    hmap_destroy(&nibble->type_cache.arrays);
    hmap_destroy(&nibble->type_cache.slices);
    hmap_destroy(&nibble->type_cache.structs);
    hmap_destroy(&nibble->type_cache.unions);
    allocator_destroy(&nibble->tmp_mem);
    allocator_destroy(&nibble->ast_mem);

    Allocator bootstrap = nibble->gen_mem;
    allocator_destroy(&bootstrap);
}

StrLit* intern_str_lit(const char* str, size_t len)
{
    Allocator* allocator = &nibble->gen_mem;
    HMap* strmap = &nibble->str_lit_map;

    uint64_t key = hash_bytes(str, len, FNV_INIT);
    uint64_t* pval = hmap_get(strmap, key);
    StrLit* intern = pval ? (void*)*pval : NULL;

    // Collisions will only occur if identical hash values are produced. Collisions due to
    // contention for hash map slots will not occur (open addressing).
    //
    // Walk the linked list in case of collision.
    for (StrLit* it = intern; it; it = it->next) {
        if ((it->len == len) && (cstr_ncmp(it->str, str, len) == 0))
            return it;
    }

    // If we got here, need to add this string to the intern table.
    StrLit* new_intern = mem_allocate(allocator, offsetof(StrLit, str) + len + 1, DEFAULT_ALIGN, true);

    if (!new_intern) {
        // TODO: Handle in a better way.
        ftprint_err("[INTERNAL ERROR]: Out of memory.\n%s:%d\n", __FILE__, __LINE__);
        exit(1);
        return NULL;
    }

    new_intern->next = intern; // Record collision. If a collision did _NOT_ occur, this will be null.
    new_intern->id = strmap->len;
    new_intern->len = len;

    memcpy(new_intern->str, str, len);
    new_intern->str[len] = '\0';

    hmap_put(strmap, key, (uintptr_t)new_intern);

    return new_intern;
}

Identifier* intern_ident(const char* str, size_t len)
{
    Allocator* allocator = &nibble->gen_mem;
    HMap* strmap = &nibble->ident_map;
    uint64_t key = hash_bytes(str, len, FNV_INIT);
    uint64_t* pval = hmap_get(strmap, key);
    Identifier* intern = pval ? (void*)*pval : NULL;

    // Collisions will only occur if identical hash values are produced. Collisions due to
    // contention for hash map slots will not occur (open addressing).
    //
    // Walk the linked list in case of collision.
    for (Identifier* it = intern; it; it = it->next) {
        if ((it->len == len) && (cstr_ncmp(it->str, str, len) == 0)) {
            return it;
        }
    }

    // If we got here, need to add this string to the intern table.
    Identifier* new_intern = mem_allocate(allocator, offsetof(Identifier, str) + len + 1, DEFAULT_ALIGN, true);

    if (!new_intern) {
        // TODO: Handle in a better way.
        ftprint_err("[INTERNAL ERROR]: Out of memory.\n%s:%d\n", __FILE__, __LINE__);
        exit(1);
        return NULL;
    }

    new_intern->next = intern; // Record collision. If a collision did _NOT_ occur, this will be null.
    new_intern->len = len;

    memcpy(new_intern->str, str, len);
    new_intern->str[len] = '\0';

    hmap_put(strmap, key, (uintptr_t)new_intern);

    return new_intern;
}

void nibble_fatal_exit(const char* format, ...)
{
    va_list vargs;

    ftprint_err("[FATAL ERROR]: ");

    va_start(vargs, format);
    ftprintv_err(format, vargs);
    va_end(vargs);

    ftprint_err("\n");

    exit(1);
}
