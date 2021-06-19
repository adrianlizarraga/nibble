#include "nibble.h"
#include "cstring.h"
#include "hash_map.h"
#include "parser.h"

typedef struct NibbleCtx {
    Allocator gen_mem;
    Allocator ast_mem;
    Allocator tmp_mem;

    HMap ident_map;
    HMap str_lit_map;

    ByteStream errors;

    TypeCache type_cache;
    Scope global_scope;

    OS target_os;
    Arch target_arch;
} NibbleCtx;

typedef struct InternedIdent {
    struct InternedIdent* next;
    size_t len;
    Keyword kw;
    bool is_kw;
    char str[];
} InternedIdent;

static NibbleCtx* nibble;

const char* os_names[NUM_OS] = {
    [OS_LINUX]   = "linux",
    [OS_WIN32]   = "win32",
    [OS_OSX]     = "osx",
};

const char* arch_names[NUM_ARCH] = {
    [ARCH_X86]     = "x86",
    [ARCH_X64]     = "x64",
};

const char* keywords[KW_COUNT];

static StringView keyword_names[KW_COUNT] = {
    [KW_VAR] = string_view_lit("var"),
    [KW_CONST] = string_view_lit("const"),
    [KW_ENUM] = string_view_lit("enum"),
    [KW_UNION] = string_view_lit("union"),
    [KW_STRUCT] = string_view_lit("struct"),
    [KW_PROC] = string_view_lit("proc"),
    [KW_TYPEDEF] = string_view_lit("typedef"),
    [KW_SIZEOF] = string_view_lit("sizeof"),
    [KW_TYPEOF] = string_view_lit("typeof"),
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

static char* slurp_file(Allocator* allocator, const char* filename)
{
    FILE* fd = fopen(filename, "r");
    if (!fd)
    {
        NIBBLE_FATAL_EXIT("Failed to open file %s", filename);
        return NULL;
    }

    if (fseek(fd, 0, SEEK_END) < 0)
    {
        NIBBLE_FATAL_EXIT("Failed to read file %s", filename);
        return NULL;
    }

    long int size = ftell(fd);
    if (size < 0)
    {
        NIBBLE_FATAL_EXIT("Failed to read file %s", filename);
        return NULL;
    }

    char* buf = mem_allocate(allocator, size + 1, DEFAULT_ALIGN, false);
    if (!buf)
    {
        NIBBLE_FATAL_EXIT("Out of memory: %s:%d", __FILE__, __LINE__);
        return NULL;
    }

    if (fseek(fd, 0, SEEK_SET) < 0)
    {
        NIBBLE_FATAL_EXIT("Failed to read file %s", filename);
        return NULL;
    }

    size_t n = fread(buf, 1, (size_t)size, fd);
    if (ferror(fd))
    {
        NIBBLE_FATAL_EXIT("Failed to read file %s", filename);
        return NULL;
    }

    fclose(fd);

    buf[n] = '\0';

    return buf;
}

static void print_errors(ByteStream* errors)
{
    if (errors->count > 0)
    {
        ftprint_out("\nErrors: %lu\n", errors->count);

        ByteStreamChunk* chunk = errors->first;

        while (chunk)
        {
            ftprint_out("%s\n", chunk->buf);

            chunk = chunk->next;
        }
    }
}

static bool init_keywords()
{
    // Compute the total amount of memory needed to store all interned keywords.
    // Why? Program needs all keywords to reside in a contigous block of memory to facilitate
    // determining whether a string is a keyword using simple pointer comparisons.
    size_t kws_size = 0;
    for (int i = 0; i < KW_COUNT; ++i)
    {
        size_t size = offsetof(InternedIdent, str) + keyword_names[i].len + 1;

        kws_size += ALIGN_UP(size, DEFAULT_ALIGN);
    }

    char* kws_mem = mem_allocate(&nibble->gen_mem, kws_size, DEFAULT_ALIGN, false);

    if (!kws_mem)
        return false;

    char* kws_mem_ptr = kws_mem;

    for (int i = 0; i < KW_COUNT; ++i)
    {
        const char* str = keyword_names[i].str;
        size_t len = keyword_names[i].len;
        size_t size = offsetof(InternedIdent, str) + len + 1;
        InternedIdent* kw = (void*)kws_mem_ptr;

        kw->next = NULL;
        kw->len = len;
        kw->is_kw = true;
        kw->kw = (Keyword)i;

        memcpy(kw->str, str, len);
        kw->str[len] = '\0';

        hmap_put(&nibble->ident_map, hash_bytes(str, len), (uintptr_t)kw);
        keywords[i] = kw->str;

        kws_mem_ptr += ALIGN_UP(size, DEFAULT_ALIGN);
    }
    assert((size_t)(kws_mem_ptr - kws_mem) == kws_size);
    assert(nibble->ident_map.len == KW_COUNT);

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
    nibble->tmp_mem = allocator_create(512);
    nibble->errors = byte_stream_create(&nibble->gen_mem);
    nibble->str_lit_map = hmap(6, NULL);
    nibble->ident_map = hmap(8, NULL);
    nibble->type_cache.ptrs = hmap(6, NULL);
    nibble->type_cache.procs = hmap(6, NULL);

    if (!init_keywords())
        return false;

    init_scope_lists(&nibble->global_scope);
    init_builtin_types(target_os, target_arch);

    return true;
}

static int64_t parse_code(List* decls, const char* code)
{
    Parser parser = {0};
    int64_t num_decls = 0;

    parser_init(&parser, &nibble->ast_mem, &nibble->tmp_mem, code, 0, &nibble->errors);
    next_token(&parser);

    while (!is_token_kind(&parser, TKN_EOF))
    {
        Decl* decl = parse_decl(&parser);

        if (!decl)
            return -1;

        num_decls += 1;
        list_add_last(decls, &decl->lnode);

#ifdef NIBBLE_PRINT_DECLS
        ftprint_out("%s\n", ftprint_decl(&nibble->gen_mem, decl));
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

    const char* path = intern_str_lit(input_file, cstr_len(input_file));
    const char* code = slurp_file(&nibble->gen_mem, path);
    List decls = list_head_create(decls);

    int64_t num_decls = parse_code(&decls, code);

    if (num_decls <= 0)
    {
        print_errors(&nibble->errors);
        return;
    }

    //////////////////////////////////////////
    //          Resolve/Typecheck
    //////////////////////////////////////////
    ftprint_out("2. Type-checking ...\n");

    Resolver resolver = {0};
    size_t num_global_syms = num_decls + 17; // TODO: Update magic 17 to num of builtin types.

    init_scope_sym_table(&nibble->global_scope, &nibble->ast_mem, num_global_syms * 2);
    init_resolver(&resolver, &nibble->ast_mem, &nibble->gen_mem, &nibble->tmp_mem, &nibble->errors,
                  &nibble->type_cache, &nibble->global_scope);

    if (!resolve_global_decls(&resolver, &decls))
    {
        print_errors(&nibble->errors);
        return;
    }

    //////////////////////////////////////////
    //          Resolve/Typecheck
    //////////////////////////////////////////
    ftprint_out("3. Generating IR ...\n");
    gen_gasm(&nibble->gen_mem, &nibble->tmp_mem, &nibble->global_scope, output_file);

    ftprint_out("4. Generating output ...\n");
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
    ftprint_out("type_proc cache: len = %lu, cap = %lu, total_size (malloc) = %lu\n", nibble->type_cache.procs.len,
                nibble->type_cache.procs.cap, nibble->type_cache.procs.cap * sizeof(HMapEntry));
#endif

    hmap_destroy(&nibble->str_lit_map);
    hmap_destroy(&nibble->ident_map);
    hmap_destroy(&nibble->type_cache.ptrs);
    hmap_destroy(&nibble->type_cache.procs);
    allocator_destroy(&nibble->tmp_mem);
    allocator_destroy(&nibble->ast_mem);

    Allocator bootstrap = nibble->gen_mem;
    allocator_destroy(&bootstrap);
}

const char* intern_str_lit(const char* str, size_t len)
{
    Allocator* allocator = &nibble->gen_mem;
    HMap* strmap = &nibble->str_lit_map;
    const char* interned_str = intern_str(allocator, strmap, str, len);

    if (!interned_str)
    {
        // TODO: Handle in a better way.
        ftprint_err("[INTERNAL ERROR]: Out of memory.\n%s:%d\n", __FILE__, __LINE__);
        exit(1);
    }

    return interned_str;
}

// TODO: Reuse duplicated interning code!
const char* intern_ident(const char* str, size_t len, bool* is_kw, Keyword* kw)
{
    Allocator* allocator = &nibble->gen_mem;
    HMap* strmap = &nibble->ident_map;
    uint64_t key = hash_bytes(str, len);
    uint64_t* pval = hmap_get(strmap, key);
    InternedIdent* intern = pval ? (void*)*pval : NULL;

    // Collisions will only occur if identical hash values are produced. Collisions due to
    // contention for hash map slots will not occur (open addressing).
    //
    // Walk the linked list in case of collision.
    for (InternedIdent* it = intern; it; it = it->next)
    {
        if ((it->len == len) && (cstr_ncmp(it->str, str, len) == 0))
        {
            if (is_kw)
            {
                *is_kw = it->is_kw;

                if (kw)
                    *kw = it->kw;
            }

            return it->str;
        }
    }

    // If we got here, need to add this string to the intern table.
    InternedIdent* new_intern = mem_allocate(allocator, offsetof(InternedIdent, str) + len + 1, DEFAULT_ALIGN, false);
    if (new_intern)
    {
        new_intern->next = intern; // Record collision. If a collision did _NOT_ occur, this will be null.
        new_intern->len = len;
        new_intern->is_kw = false;
        new_intern->kw = (Keyword)0; // TODO: Have an invalid or none keyword type. Just clean this up in some way!

        memcpy(new_intern->str, str, len);
        new_intern->str[len] = '\0';

        hmap_put(strmap, key, (uintptr_t)new_intern);
    }
    else
    {
        NIBBLE_FATAL_EXIT("Out of memory: %s:%d", __FILE__, __LINE__);
        return NULL;
    }

    if (is_kw)
    {
        *is_kw = new_intern->is_kw;

        if (kw)
            *kw = new_intern->kw;
    }

    return new_intern->str;
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
