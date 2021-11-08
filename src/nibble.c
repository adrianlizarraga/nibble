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

    assert(nibble->ident_map.len == (KW_COUNT + ANNOTATION_COUNT + INTRINSIC_COUNT));

    bucket_list_init(&nibble->vars, &nibble->ast_mem, 32);
    bucket_list_init(&nibble->procs, &nibble->ast_mem, 32);

    init_builtin_types(target_os, target_arch, &nibble->ast_mem, &nibble->type_cache);

    if (!init_code_gen(target_os, target_arch))
        return false;

    return true;
}

/*
static const char* module_get_ospath(NibbleCtx* ctx, Path* ospath, Module* mod)
{
    path_set(ospath, ctx->base_ospath.str, ctx->base_ospath.len);

    Path modpath;
    path_init(&modpath, &ctx->tmp_mem);
    path_set(&modpath, mod->mod_path->str, mod->mod_path->len);

    path_join(ospath, &modpath);
}

static Package* get_pkg(NibbleCtx* ctx, StrLit* name)
{
    uint64_t* pval = hmap_get(&ctx->pkg_map, PTR_UINT(name));
    Package* pkg = pval ? (void*)*pval : NULL;

    return pkg;
}

static bool set_pkg_path(NibbleCtx* ctx, Package* pkg)
{
    AllocatorState mem_state = allocator_get_state(&ctx->tmp_mem);
    bool found = false;

    for (int i = 0; i < ctx->num_search_paths; i += 1) {
        Path* search_path = ctx->search_paths + i;

        Path full_path;
        path_init(&full_path, &ctx->tmp_mem);
        path_set(&full_path, search_path->str);

        Path name_path;
        path_init(&name_path, &ctx->tmp_mem);
        path_set(&name_path, pkg->name->str);

        path_join(&full_path, &name_path);

        if (path_abs(&full_path)) {
            path_set(&pkg->path, full_path.str);
            found = true;
            break;
        }
    }

    allocator_restore_state(mem_state);
    return found;
}

static bool parse_pkg(NibbleCtx* ctx, Package* pkg)
{
    return true;
}

static Package* import_pkg(NibbleCtx* ctx, const char* pkg_path)
{
    StrLit* name = intern_str_lit(pkg_path, cstr_len(pkg_path));
    Package* pkg = get_pkg(ctx, name);

    if (!pkg) {
        pkg = alloc_type(&ctx->ast_mem, Package, false);
        pkg->name = name;
        pkg->curr_scope = NULL;

        scope_init(&pkg->global_scope);
        path_init(&pkg->path, &ctx->ast_mem);

        if (!set_pkg_path(ctx, pkg)) {
            return NULL;
        }

        add_pkg(ctx, pkg);
        
        if (!parse_pkg(ctx, pkg)) {
            return NULL;
        }
    }

    return pkg;
}
*/

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

void nibble_compile(const char* main_file, const char* output_file)
{
    //////////////////////////////////////////
    //      Check main file validity
    //////////////////////////////////////////
    static const char nib_ext[] = "nib";
    static const char main_name[] = "main";

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

    size_t mod_path_cap = ((main_path.str + main_path.len) - base_path_end_ptr) + 1;
    char* entry_mod_path_buf = array_create(&nibble->tmp_mem, char, mod_path_cap);
    ftprint_char_array(&entry_mod_path_buf, true, "%c%s", NIBBLE_PATH_SEP, filename_ptr);

    Module* main_mod = alloc_type(&nibble->ast_mem, Module, true);
    main_mod->mod_path = intern_str_lit(entry_mod_path_buf, cstr_len(entry_mod_path_buf));

    scope_init(&main_mod->scope);
    hmap_put(&nibble->mod_map, PTR_UINT(main_mod->mod_path), PTR_UINT(main_mod));

    //////////////////////////////////////////
    //                Parse
    //////////////////////////////////////////
    ftprint_out("1. Parsing module %s ...\n", main_mod->mod_path->str);

    const char* code = slurp_file(&nibble->gen_mem, main_path.str);
    list_head_init(&main_mod->stmts);

    int64_t num_builtin_decls = parse_code(&main_mod->stmts, builtin_decls);

    if (num_builtin_decls <= 0) {
        ftprint_err("[ERROR]: Failed to parse builtin code\n");
        print_errors(&nibble->errors);
        return;
    }

    int64_t num_decls = parse_code(&main_mod->stmts, code);

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

    init_scope_sym_table(&main_mod->scope, &nibble->ast_mem, num_global_syms * 2);
    init_resolver(&resolver, nibble, main_mod);

    // Look for main to have been parsed and installed as an unresolved proc symbol.
    Identifier* main_ident = intern_ident(main_name, sizeof(main_name) - 1);
    Symbol* main_sym = lookup_symbol(&main_mod->scope, main_ident);

    if (!main_sym || (main_sym->kind != SYMBOL_PROC)) {
        ftprint_err("[ERROR]: Program entry file must define a main() procedure.\n");
        return;
    }

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

    if (main_ret_type != type_int) {
        ftprint_err("[ERROR]: Main procedure must return an `int` (`%s`) type, but found `%s`.\n",
                    type_name(type_int),
                    type_name(main_ret_type));
        return;
    }

    size_t main_num_params = main_type->as_proc.num_params;

    // Check that params are either main(argc: int) or main(argc: int, argv: ^^char)
    if (main_num_params > 0) {
        Type** param_types = main_type->as_proc.params;

        if (param_types[0] != type_int) {
            ftprint_err("[ERROR]: Main procedure's first paramater must be an `int` (`%s`) type, but found `%s`.\n",
                        type_name(type_int),
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
