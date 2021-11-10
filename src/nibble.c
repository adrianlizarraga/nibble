#include "nibble.h"
#include "compiler.h"
#include "parser.h"
#include "bytecode.h"
#include "code_gen.h"

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

Identifier* main_proc_ident;

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

void report_error(const char* filename, ProgRange range, const char* format, ...)
{
    // TODO: range should just have line and column numbers instead of byte offsets.
    (void)range;

    // TODO: Use an allocator + string writer.
    char buf[MAX_ERROR_LEN];
    va_list vargs;

    size_t size = snprintf(buf, MAX_ERROR_LEN, "%s:0:0 ", filename);

    if (size < MAX_ERROR_LEN) {
        size_t rem = MAX_ERROR_LEN - size;
        char* ptr = buf + size;

        va_start(vargs, format);
        size += vsnprintf(ptr, rem, format, vargs) + 1;
        va_end(vargs);
    }

    if (size >= MAX_ERROR_LEN) {
        buf[MAX_ERROR_LEN - 1] = '\0';
        size = MAX_ERROR_LEN;
    }

    add_byte_stream_chunk(&nibble->errors, buf, size);
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

static const char nib_ext[] = "nib";

/*
// Code for the builtin module that is automatically imported everywhere:
// Generated by: xxd -i builtin.nib

@exported
proc #writeout(buf: ^char, size: usize) => ssize;

@exported
proc #readin(buf: ^char, size: usize) => ssize;
*/
static const char builtin_code[] = {
  0x40, 0x65, 0x78, 0x70, 0x6f, 0x72, 0x74, 0x65, 0x64, 0x0a, 0x70, 0x72,
  0x6f, 0x63, 0x20, 0x23, 0x77, 0x72, 0x69, 0x74, 0x65, 0x6f, 0x75, 0x74,
  0x28, 0x62, 0x75, 0x66, 0x3a, 0x20, 0x5e, 0x63, 0x68, 0x61, 0x72, 0x2c,
  0x20, 0x73, 0x69, 0x7a, 0x65, 0x3a, 0x20, 0x75, 0x73, 0x69, 0x7a, 0x65,
  0x29, 0x20, 0x3d, 0x3e, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x3b, 0x0a,
  0x0a, 0x40, 0x65, 0x78, 0x70, 0x6f, 0x72, 0x74, 0x65, 0x64, 0x0a, 0x70,
  0x72, 0x6f, 0x63, 0x20, 0x23, 0x72, 0x65, 0x61, 0x64, 0x69, 0x6e, 0x28,
  0x62, 0x75, 0x66, 0x3a, 0x20, 0x5e, 0x63, 0x68, 0x61, 0x72, 0x2c, 0x20,
  0x73, 0x69, 0x7a, 0x65, 0x3a, 0x20, 0x75, 0x73, 0x69, 0x7a, 0x65, 0x29,
  0x20, 0x3d, 0x3e, 0x20, 0x73, 0x73, 0x69, 0x7a, 0x65, 0x3b, 0x0a, 0x00
};


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
        [KW_FROM] = string_view_lit("from"),
        [KW_AS] = string_view_lit("as"),
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
    nibble->mod_map = hmap(6, NULL);
    nibble->type_cache.ptrs = hmap(6, NULL);
    nibble->type_cache.arrays = hmap(6, NULL);
    nibble->type_cache.procs = hmap(6, NULL);

    if (!init_keywords())
        return false;

    if (!init_intrinsics())
        return false;

    if (!init_annotations())
        return false;

    main_proc_ident = intern_ident(main_name, sizeof(main_name) - 1);

    assert(nibble->ident_map.len == (KW_COUNT + ANNOTATION_COUNT + INTRINSIC_COUNT + 1));

    bucket_list_init(&nibble->vars, &nibble->ast_mem, 32);
    bucket_list_init(&nibble->procs, &nibble->ast_mem, 32);

    init_builtin_types(target_os, target_arch, &nibble->ast_mem, &nibble->type_cache);

    if (!init_code_gen(target_os, target_arch))
        return false;

    return true;
}

static void module_get_ospath(Allocator* alloc, Path* dst, Module* mod, const Path* base_ospath)
{
    Path modpath;
    path_init(&modpath, alloc);
    path_set(&modpath, mod->mod_path->str, mod->mod_path->len);

    path_init(dst, alloc);
    path_set(dst, base_ospath->str, base_ospath->len);
    path_join(dst, &modpath);
}

static Module* get_module(NibbleCtx* ctx, StrLit* mod_path)
{
    uint64_t* pval = hmap_get(&ctx->mod_map, PTR_UINT(mod_path));
    Module* mod = pval ? (void*)*pval : NULL;

    return mod;
}

static bool add_builtin_type_symbol(NibbleCtx* ctx, const char* name, Type* type)
{
    Identifier* sym_name = intern_ident(name, cstr_len(name));
    Module* builtin_mod = &ctx->builtin_mod;

    if (lookup_scope_symbol(&builtin_mod->scope, sym_name)) {
        ProgRange range = {0};
        report_error(builtin_mod->mod_path->str, range, "[INTERNAL ERROR] Duplicate definition of builtin `%s`", sym_name);
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

static bool parse_code(size_t* num_decls, List* stmts, const char* code)
{
    Parser parser = {0};
    *num_decls = 0;

    parser_init(&parser, &nibble->ast_mem, &nibble->tmp_mem, code, 0, &nibble->errors);
    next_token(&parser);

    while (!is_token_kind(&parser, TKN_EOF)) {
        Stmt* stmt = parse_global_stmt(&parser);

        if (!stmt || nibble->errors.count)
            return false;

        if (stmt->kind == CST_StmtDecl) *num_decls += 1;
        list_add_last(stmts, &stmt->lnode);

#ifdef NIBBLE_PRINT_DECLS
        ftprint_out("%s\n", ftprint_stmt(&nibble->gen_mem, stmt));
#endif
    }

#ifdef NIBBLE_PRINT_DECLS
    ftprint_out("\n");
#endif

    return true;
}

static bool parse_module(NibbleCtx* ctx, Module* mod);

static Module* import_module(NibbleCtx* ctx, const char* path, size_t len)
{
    StrLit* mod_path = intern_str_lit(path, len);
    Module* mod = get_module(ctx, mod_path);

    if (!mod) {
        mod = alloc_type(&ctx->ast_mem, Module, true);
        mod->mod_path = mod_path;

        scope_init(&mod->scope);
        list_head_init(&mod->stmts);

        hmap_put(&ctx->mod_map, PTR_UINT(mod->mod_path), PTR_UINT(mod));

        if (!parse_module(ctx, mod)) {
            return NULL;
        }
    }

    return mod;
}

static bool parse_module(NibbleCtx* ctx, Module* mod)
{
    ftprint_out("[INFO] Parsing module %s ...\n", mod->mod_path->str);

    const char* code = NULL;
    size_t num_decls = 0;

    AllocatorState mem_state = allocator_get_state(&ctx->tmp_mem);

    // Parse the code text
    Path mod_ospath;
    module_get_ospath(&ctx->tmp_mem, &mod_ospath, mod, &ctx->base_ospath);

    ftprint_out("Module OS path: %s\n", mod_ospath.str);

    code = slurp_file(&ctx->tmp_mem, mod_ospath.str);

    if (!parse_code(&num_decls, &mod->stmts, code)) {
        return false;
    }

    init_scope_sym_table(&mod->scope, &ctx->ast_mem, num_decls << 1);

    ftprint_out("Trying to import builtins...\n");

    // Import all builtin symbols.
    if (!import_all_mod_syms(mod, &ctx->builtin_mod, true)) {
        return false;
    }

    ftprint_out("Trying install decls into sym table...\n");
    // Install unresolved decls into the module's symbol table.
    if (!install_module_decls(&ctx->ast_mem, mod)) {
        return false;
    }

    ftprint_out("Trying process imports...\n");
    // Process imports.
    {
        List* head = &mod->stmts;
        List* it = head->next;

        while (it != head) {
            Stmt* stmt = list_entry(it, Stmt, lnode);

            if (stmt->kind == CST_StmtImport) {
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
                ftprint_out("Trying to import %s\n", simport->mod_pathname->str);

                Path import_mod_ospath;
                path_init(&import_mod_ospath, &ctx->tmp_mem);

                Path import_rel_path;
                path_init(&import_rel_path, &ctx->tmp_mem);
                path_set(&import_rel_path, simport->mod_pathname->str, simport->mod_pathname->len);

                bool starts_root = simport->mod_pathname->str[0] == NIBBLE_PATH_SEP;

                if (starts_root) {
                    path_set(&import_mod_ospath, ctx->base_ospath.str, ctx->base_ospath.len);
                }
                else {
                    const char* dir_begp = mod_ospath.str;
                    const char* dir_endp = path_filename(&mod_ospath) - 1;

                    path_set(&import_mod_ospath, dir_begp, dir_endp - dir_begp);
                }

                path_join(&import_mod_ospath, &import_rel_path);
                ftprint_out("Hmmm import os path before abs: %s\n", import_mod_ospath.str);

                // Check if imported module's path exists somewhere.
                if (!path_abs(&import_mod_ospath)) {
                    report_error(mod_ospath.str, stmt->range, "Invalid module import path \"%s\"", simport->mod_pathname->str);
                    return false;
                }

                // Check for .nib extension.
                if (cstr_cmp(path_ext(&import_mod_ospath), nib_ext) != 0) {
                    report_error(mod_ospath.str, stmt->range, "Imported file \"%s\" does not end in `.%s`",
                                 simport->mod_pathname->str, nib_ext);
                    return false;
                }

                ftprint_out("Import OS path: %s\n", import_mod_ospath.str);
                ftprint_out("Base OS path: %s\n", ctx->base_ospath.str);

                // Try to create a canonical module path (where `/` corresponds to main's home directory).
                Path import_mod_path;
                path_init(&import_mod_path, &ctx->tmp_mem);

                {
                    // NOTE: Does not handle case where base_ospath is literally `/`.
                    assert(ctx->base_ospath.len);

                    const char* bp = ctx->base_ospath.str;
                    const char* mp = import_mod_ospath.str;

                    while (*bp && *mp && (*bp == *mp)) {
                        bp += 1;
                        mp += 1;
                    }

                    if (*bp) {
                        report_error(mod_ospath.str, stmt->range,
                                     "Relative module import path \"%s\" is outside of project root dir \"%s\"", 
                                     simport->mod_pathname->str, ctx->base_ospath.str);
                        return false;
                    }

                    assert(*mp == OS_PATH_SEP);

                    path_set(&import_mod_path, mp, (import_mod_ospath.str + import_mod_ospath.len) - mp);
                    path_norm(&import_mod_path, OS_PATH_SEP, NIBBLE_PATH_SEP);

                    ftprint_out("Import canonical path: %s\n", import_mod_path.str);
                }

                //
                // Import module
                //

                Module* import_mod = import_module(ctx, import_mod_path.str, import_mod_path.len);

                if (!import_mod) {
                    return false;
                }

                // TODO: Support import entities
                assert(list_empty(&simport->import_syms));

                if (simport->mod_namespace) {
                    // TODO: Support module namespace
                    // TODO: Create module symbol
                    assert(0);
                }
                else {
                    if (!import_all_mod_syms(mod, import_mod, false)) {
                        return false;
                    }
                }
            }

            it = it->next;
        }
    }

    allocator_restore_state(mem_state);
    ftprint_out("[INFO] DONE parsing module %s ...\n", mod->mod_path->str);
    return true;
}

void nibble_compile(const char* main_file, const char* output_file)
{
    //////////////////////////////////////////
    //      Check main file validity
    //////////////////////////////////////////
    static const char builtin_mod_name[] = "/_nibble_builtin";

    AllocatorState mem_state = allocator_get_state(&nibble->tmp_mem);

    Path main_path;
    path_init(&main_path, &nibble->tmp_mem);
    path_set(&main_path, main_file, cstr_len(main_file));
    
    if (!path_abs(&main_path)) {
        ftprint_err("[ERROR]: Cannot find file `%s`\n", main_file);
        return;
    }

    FileKind file_kind = path_kind(&main_path);

    if ((file_kind != FILE_REG) || cstr_cmp(path_ext(&main_path), nib_ext) != 0) {
        ftprint_err("[ERROR]: Program main file must end in `.nib`\n");
        return;
    }

    /////////////////////////////////////////////////////////
    //      Get main file's module path (e.g., /main.nib)
    //      and extract the base OS path.
    /////////////////////////////////////////////////////////
    const char* filename_ptr = path_filename(&main_path); assert(filename_ptr != main_path.str);
    const char* base_path_end_ptr = filename_ptr - 1;

    Path* base_ospath = &nibble->base_ospath;
    path_init(base_ospath, &nibble->gen_mem);
    path_set(base_ospath, main_path.str, (base_path_end_ptr - main_path.str));

    ftprint_out("[INFO] Base project OS path: %s\n", base_ospath->str);

    size_t mod_path_cap = ((main_path.str + main_path.len) - base_path_end_ptr) + 1;
    char* entry_mod_path_buf = array_create(&nibble->tmp_mem, char, mod_path_cap);
    ftprint_char_array(&entry_mod_path_buf, true, "%c%s", NIBBLE_PATH_SEP, filename_ptr);

    // Main module
    Module* main_mod = alloc_type(&nibble->ast_mem, Module, true);
    main_mod->mod_path = intern_str_lit(entry_mod_path_buf, cstr_len(entry_mod_path_buf));
    scope_init(&main_mod->scope);
    list_head_init(&main_mod->stmts);
    hmap_put(&nibble->mod_map, PTR_UINT(main_mod->mod_path), PTR_UINT(main_mod));

    /// Builtin module
    Module* builtin_mod = &nibble->builtin_mod;
    builtin_mod->mod_path = intern_str_lit(builtin_mod_name, sizeof(builtin_mod_name) - 1);
    scope_init(&builtin_mod->scope);
    list_head_init(&builtin_mod->stmts);
    hmap_put(&nibble->mod_map, PTR_UINT(builtin_mod->mod_path), PTR_UINT(builtin_mod)); // TODO: Necessary?

    //////////////////////////////////////////
    //                Parse
    //////////////////////////////////////////
    ftprint_out("[INFO] Parsing builtin module\n");

    const size_t num_builtin_types = ARRAY_LEN(builtin_types);
    size_t num_builtin_decls = 0;
    bool parse_ok = parse_code(&num_builtin_decls, &builtin_mod->stmts, builtin_code);

    if (!parse_ok) {
        ftprint_err("[ERROR]: Failed to parse builtin code\n");
        print_errors(&nibble->errors);
        return;
    }

    init_scope_sym_table(&builtin_mod->scope, &nibble->ast_mem, (num_builtin_decls + num_builtin_types) << 1);
    init_builtin_syms(nibble);

    if (!install_module_decls(&nibble->ast_mem, builtin_mod)) {
        print_errors(&nibble->errors);
        return;
    }

    // Parse main module.
    if (!parse_module(nibble, main_mod)) {
        print_errors(&nibble->errors);
        return;
    }

    // Look for main to have been parsed and installed as an unresolved proc symbol.
    Symbol* main_sym = lookup_symbol(&main_mod->scope, main_proc_ident);

    if (!main_sym || (main_sym->kind != SYMBOL_PROC)) {
        ftprint_err("[ERROR]: Program entry file must define a main() procedure.\n");
        return;
    }

    //////////////////////////////////////////
    //          Resolve/Typecheck
    //////////////////////////////////////////
    ftprint_out("2. Type-checking ...\n");

    Resolver resolver = {.ctx = nibble};

    if (!resolve_module(&resolver, main_mod)) {
        print_errors(&nibble->errors);
        return;
    }

    if (!resolve_reachable_sym_defs(&resolver)) {
        print_errors(&nibble->errors);
        return;
    }

    // Ensure main has the expected type signature.
    Type* main_type = main_sym->type; assert(main_type->kind == TYPE_PROC);
    Type* main_ret_type = main_type->as_proc.ret;

    if (main_ret_type != builtin_types[BUILTIN_TYPE_INT].type) {
        ftprint_err("[ERROR]: Main procedure must return an `int` (`%s`) type, but found `%s`.\n",
                    type_name(builtin_types[BUILTIN_TYPE_INT].type),
                    type_name(main_ret_type));
        return;
    }

    size_t main_num_params = main_type->as_proc.num_params;

    // Check that params are either main(argc: int) or main(argc: int, argv: ^^char)
    if (main_num_params > 0) {
        Type** param_types = main_type->as_proc.params;

        if (param_types[0] != builtin_types[BUILTIN_TYPE_INT].type) {
            ftprint_err("[ERROR]: Main procedure's first paramater must be an `int` (`%s`) type, but found `%s`.\n",
                        type_name(builtin_types[BUILTIN_TYPE_INT].type),
                        type_name(param_types[0]));
            return;
        }

        // TODO: Allow argv : []^char
        if ((main_num_params == 2) && (param_types[1] != type_ptr_ptr_char)) {
            ftprint_err("[ERROR]: Main procedure's second paramater must be a `^^char` type, but found `%s`.\n",
                        type_name(param_types[0]));
            return;
        }

        // TODO: Allow/check for envp param
    }

    ftprint_out("\tglobal vars: %u\n\tglobal procs: %u\n", nibble->vars.num_elems, nibble->procs.num_elems);

    //////////////////////////////////////////
    //          Gen NASM output
    //////////////////////////////////////////
    ftprint_out("4. Generating IR bytecode\n");
    IR_gen_bytecode(&nibble->ast_mem, &nibble->tmp_mem, &nibble->procs, &nibble->type_cache);

    //////////////////////////////////////////
    //          Gen NASM output
    //////////////////////////////////////////
    ftprint_out("4. Generating NASM assembly output: %s ...\n", output_file);
    gen_module(&nibble->gen_mem, &nibble->tmp_mem, &nibble->vars, &nibble->procs, &nibble->str_lit_map, output_file);

    allocator_restore_state(mem_state);
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
    ftprint_out("Module map: len = %lu, cap = %lu, total_size (malloc) = %lu\n", nibble->mod_map.len,
                nibble->mod_map.cap, nibble->mod_map.cap * sizeof(HMapEntry));
    ftprint_out("type_ptr cache: len = %lu, cap = %lu, total_size (malloc) = %lu\n", nibble->type_cache.ptrs.len,
                nibble->type_cache.ptrs.cap, nibble->type_cache.ptrs.cap * sizeof(HMapEntry));
    ftprint_out("type_array cache: len = %lu, cap = %lu, total_size (malloc) = %lu\n", nibble->type_cache.arrays.len,
                nibble->type_cache.arrays.cap, nibble->type_cache.arrays.cap * sizeof(HMapEntry));
    ftprint_out("type_proc cache: len = %lu, cap = %lu, total_size (malloc) = %lu\n", nibble->type_cache.procs.len,
                nibble->type_cache.procs.cap, nibble->type_cache.procs.cap * sizeof(HMapEntry));
#endif

    hmap_destroy(&nibble->str_lit_map);
    hmap_destroy(&nibble->ident_map);
    hmap_destroy(&nibble->mod_map);
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
