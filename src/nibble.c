#include "nibble.h"
#include "cstring.h"
#include "hash_map.h"
#include "parser.h"
#include "bytecode.h"
#include "code_gen.h"

typedef struct NibbleCtx {
    Allocator gen_mem;
    Allocator ast_mem;
    Allocator tmp_mem;

    HMap ident_map;
    HMap str_lit_map;
    HMap pkg_map;

    ByteStream errors;

    TypeCache type_cache;
    BucketList symbols;

    OS target_os;
    Arch target_arch;
} NibbleCtx;

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
const char* intrinsic_names[INTRINSIC_COUNT];

char* slurp_file(Allocator* allocator, const char* filename)
{
    FILE* fd = fopen(filename, "r");
    if (!fd) {
        NIBBLE_FATAL_EXIT("Failed to open file %s", filename);
        return NULL;
    }

    if (fseek(fd, 0, SEEK_END) < 0) {
        NIBBLE_FATAL_EXIT("Failed to read file %s", filename);
        return NULL;
    }

    long int size = ftell(fd);
    if (size < 0) {
        NIBBLE_FATAL_EXIT("Failed to read file %s", filename);
        return NULL;
    }

    char* buf = mem_allocate(allocator, size + 1, DEFAULT_ALIGN, false);
    if (!buf) {
        NIBBLE_FATAL_EXIT("Out of memory: %s:%d", __FILE__, __LINE__);
        return NULL;
    }

    if (fseek(fd, 0, SEEK_SET) < 0) {
        NIBBLE_FATAL_EXIT("Failed to read file %s", filename);
        return NULL;
    }

    size_t n = fread(buf, 1, (size_t)size, fd);
    if (ferror(fd)) {
        NIBBLE_FATAL_EXIT("Failed to read file %s", filename);
        return NULL;
    }

    fclose(fd);

    buf[n] = '\0';

    return buf;
}

static void print_errors(ByteStream* errors)
{
    if (errors->count > 0) {
        ftprint_out("\nErrors: %lu\n", errors->count);

        ByteStreamChunk* chunk = errors->first;

        while (chunk) {
            ftprint_out("%s\n", chunk->buf);

            chunk = chunk->next;
        }
    }
}

static bool init_annotations()
{
    static const StringView names[ANNOTATION_COUNT] = {
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

static const char* builtin_decls =
    "proc #writeout(buf: ^char, size: usize) => ssize;\n"
    "proc #readin(buf: ^char, size: usize) => ssize;\n";

static bool init_intrinsics()
{
    static const StringView names[INTRINSIC_COUNT] = {
        [INTRINSIC_READIN] = string_view_lit("#readin"),
        [INTRINSIC_WRITEOUT] = string_view_lit("#writeout"),
    };

    for (int i = 0; i < INTRINSIC_COUNT; i += 1) {
        const StringView* str_view = names + i;
        Identifier* ident = intern_ident(str_view->str, str_view->len);

        if (!ident) {
            return false;
        }

        ident->kind = IDENTIFIER_INTRINSIC;
        ident->intrinsic = (Intrinsic)i;

        intrinsic_names[i] = ident->str;
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
        [KW_STATIC_ASSERT] = string_view_lit("#static_assert"),
        [KW_IMPORT] = string_view_lit("import"),
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
    Allocator bootstrap = allocator_create(32768);
    nibble = alloc_type(&bootstrap, NibbleCtx, true);

    nibble->target_os = target_os;
    nibble->target_arch = target_arch;
    nibble->gen_mem = bootstrap;
    nibble->ast_mem = allocator_create(16384);
    nibble->tmp_mem = allocator_create(4096);
    nibble->errors = byte_stream_create(&nibble->gen_mem);
    nibble->str_lit_map = hmap(6, NULL);
    nibble->ident_map = hmap(8, NULL);
    nibble->pkg_map = hmap(6, NULL);
    nibble->type_cache.ptrs = hmap(6, NULL);
    nibble->type_cache.arrays = hmap(6, NULL);
    nibble->type_cache.procs = hmap(6, NULL);

    if (!init_keywords())
        return false;

    if (!init_intrinsics())
        return false;

    if (!init_annotations())
        return false;

    assert(nibble->ident_map.len == (KW_COUNT + ANNOTATION_COUNT + INTRINSIC_COUNT));

    bucket_list_init(&nibble->symbols, &nibble->ast_mem, 64);
    init_builtin_types(target_os, target_arch, &nibble->ast_mem, &nibble->type_cache);

    if (!init_code_gen(target_os, target_arch))
        return false;

    return true;
}

static Package* get_pkg(NibbleCtx* ctx, StrLit* name)
{
    uint64_t* pval = hmap_get(ctx->pkg_map, PTR_UINT(name));
    Package* pkg = pval ? (void*)*pval : NULL;

    return pkg;
}

static bool set_pkg_path(NibbleCtx* ctx, Package* pkg)
{
    for (int i = 0; i < ctx->num_search_paths; i += 1) {
        Path* search_path = ctx->search_paths + i;

        path_set(&pkg->path, search_path->str);
        //path_join(&pkg->path, 
    }
}

static Package* import_pkg(NibbleCtx* ctx, const char* pkg_path)
{
    StrLit* name = intern_str_lit(pkg_path, cstr_len(pkg_path));
    Package* pkg = get_package(ctx, name);

    if (!pkg) {
        pkg = alloc_type(&ctx->ast_mem, Package, false);
        pkg->name = name;
        pkg->curr_scope = NULL;

        scope_init(&pkg->global_scope);
        path_init(&pkg->path, &ctx->ast_mem);

        if (!set_pkg_path(ctx, pkg)) {
            return NULL;
        }
    }

    return pkg;
}

static int64_t parse_code(List* stmts, const char* code)
{
    Parser parser = {0};
    int64_t num_decls = 0;

    parser_init(&parser, &nibble->ast_mem, &nibble->tmp_mem, code, 0, &nibble->errors);
    next_token(&parser);

    while (!is_token_kind(&parser, TKN_EOF)) {
        Stmt* stmt = parse_global_stmt(&parser);

        if (!stmt || nibble->errors.count)
            return -1;

        if (stmt->kind == CST_StmtDecl) num_decls += 1;
        list_add_last(stmts, &stmt->lnode);

#ifdef NIBBLE_PRINT_DECLS
        ftprint_out("%s\n", ftprint_stmt(&nibble->gen_mem, stmt));
#endif
    }

#ifdef NIBBLE_PRINT_DECLS
    ftprint_out("\n");
#endif

    return num_decls;
}

void nibble_compile(const char* input_file, const char* output_file)
{
    //////////////////////////////////////////
    //                Parse
    //////////////////////////////////////////
    ftprint_out("1. Parsing %s ...\n", input_file);

    const char* path = intern_str_lit(input_file, cstr_len(input_file))->str;
    const char* code = slurp_file(&nibble->gen_mem, path);
    List stmts = list_head_create(stmts);

    int64_t num_builtin_decls = parse_code(&stmts, builtin_decls);

    if (num_builtin_decls <= 0) {
        ftprint_err("[ERROR]: Failed to parse builtin code\n");
        print_errors(&nibble->errors);
        return;
    }

    int64_t num_decls = parse_code(&stmts, code);

    if (num_decls <= 0) {
        print_errors(&nibble->errors);
        return;
    }

    //////////////////////////////////////////
    //          Resolve/Typecheck
    //////////////////////////////////////////
    ftprint_out("2. Type-checking ...\n");

    Resolver resolver = {0};
    size_t num_global_syms = num_decls + num_builtin_decls + 17; // TODO: Update magic 17 to num of builtin types.

    init_scope_sym_table(&nibble->global_scope, &nibble->ast_mem, num_global_syms * 2);
    init_resolver(&resolver, &nibble->ast_mem, &nibble->tmp_mem, &nibble->errors, &nibble->type_cache,
                  &nibble->global_scope);

    if (!resolve_global_stmts(&resolver, &stmts)) {
        print_errors(&nibble->errors);
        return;
    }

    ftprint_out("\tglobal vars: %u\n\tglobal procs: %u\n", nibble->global_scope.sym_kind_counts[SYMBOL_VAR],
                nibble->global_scope.sym_kind_counts[SYMBOL_PROC]);

    //////////////////////////////////////////
    //          Gen NASM output
    //////////////////////////////////////////
    ftprint_out("4. Generating IR bytecode\n");
    IR_Module* module = IR_build_module(&nibble->ast_mem, &nibble->tmp_mem, &nibble->global_scope, &nibble->type_cache);

    //////////////////////////////////////////
    //          Gen NASM output
    //////////////////////////////////////////
    ftprint_out("4. Generating NASM assembly output: %s ...\n", output_file);
    gen_module(&nibble->gen_mem, &nibble->tmp_mem, module, &nibble->str_lit_map, output_file);
}

void nibble_cleanup(void)
{
#ifndef NDEBUG
    print_allocator_stats(&nibble->gen_mem, "GEN mem stats");
    print_allocator_stats(&nibble->ast_mem, "AST mem stats");
    print_allocator_stats(&nibble->tmp_mem, "TMP mem stats");
    ftprint_out("Ident map: len = %lu, cap = %lu, total_size (malloc) = %lu\n", nibble->ident_map.len,
                nibble->ident_map.cap, nibble->ident_map.cap * sizeof(HMapEntry));
    ftprint_out("StrLit map: len = %lu, cap = %lu, total_size (malloc) = %lu\n", nibble->str_lit_map.len,
                nibble->str_lit_map.cap, nibble->str_lit_map.cap * sizeof(HMapEntry));
    ftprint_out("type_ptr cache: len = %lu, cap = %lu, total_size (malloc) = %lu\n", nibble->type_cache.ptrs.len,
                nibble->type_cache.ptrs.cap, nibble->type_cache.ptrs.cap * sizeof(HMapEntry));
    ftprint_out("type_array cache: len = %lu, cap = %lu, total_size (malloc) = %lu\n", nibble->type_cache.arrays.len,
                nibble->type_cache.arrays.cap, nibble->type_cache.arrays.cap * sizeof(HMapEntry));
    ftprint_out("type_proc cache: len = %lu, cap = %lu, total_size (malloc) = %lu\n", nibble->type_cache.procs.len,
                nibble->type_cache.procs.cap, nibble->type_cache.procs.cap * sizeof(HMapEntry));
#endif

    hmap_destroy(&nibble->str_lit_map);
    hmap_destroy(&nibble->ident_map);
    hmap_destroy(&nibble->pkg_map);
    hmap_destroy(&nibble->type_cache.ptrs);
    hmap_destroy(&nibble->type_cache.procs);
    hmap_destroy(&nibble->type_cache.arrays);
    allocator_destroy(&nibble->tmp_mem);
    allocator_destroy(&nibble->ast_mem);

    Allocator bootstrap = nibble->gen_mem;
    allocator_destroy(&bootstrap);
}

StrLit* intern_str_lit(const char* str, size_t len)
{
    Allocator* allocator = &nibble->gen_mem;
    HMap* strmap = &nibble->str_lit_map;

    uint64_t key = hash_bytes(str, len);
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
    uint64_t key = hash_bytes(str, len);
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
