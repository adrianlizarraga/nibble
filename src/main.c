//#define NDEBUG 1
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nibble.h"

#include "allocator.c"
#include "cstring.c"
#include "print.c"
#include "array.c"
#include "hash_map.c"
#include "stream.c"
#include "lexer.c"
#include "ast.c"
#include "parser.c"

typedef struct NibbleCtx {
    HashMap ident_map;
    HashMap str_lit_map;
    Allocator allocator;
} NibbleCtx;

typedef struct CompiledModule {
    Allocator allocator;
    Allocator ast_arena;
    ByteStream errors;
} CompiledModule;

typedef struct InternedStr {
    struct InternedStr* next;
    size_t len;
    char str[];
} InternedStr;

typedef struct InternedIdent {
    struct InternedIdent* next;
    size_t len;
    Keyword kw;
    bool is_kw;
    char str[];
} InternedIdent;

static NibbleCtx nibble;

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
    [KW_DEFAULT] = string_view_lit("default"),
    [KW_CAST] = string_view_lit("cast"),
    [KW_UNDERSCORE] = string_view_lit("_"),
};

static bool nibble_init(void)
{
    nibble.allocator = allocator_create(4096);
    nibble.str_lit_map = hash_map(6, NULL);
    nibble.ident_map = hash_map(6, NULL);

    // Compute the total amount of memory needed to store all interned keywords.
    // Why? Program needs all keywords to reside in a contigous block of memory to facilitate
    // determining whether a string is a keyword using simple pointer comparisons.
    size_t kws_size = 0;
    for (int i = 0; i < KW_COUNT; ++i) {
        size_t size = offsetof(InternedIdent, str) + keyword_names[i].len + 1; 

        kws_size += ALIGN_UP(size, DEFAULT_ALIGN);
    }

    char* kws_mem = mem_allocate(&nibble.allocator, kws_size, DEFAULT_ALIGN, false);
    if (!kws_mem) {
        return false;
    }

    char* kws_mem_ptr = kws_mem;

    for (int i = 0; i < KW_COUNT; ++i) {
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

        hash_map_put(&nibble.ident_map, hash_bytes(str, len), (uintptr_t)kw);
        keywords[i] = kw->str;

        kws_mem_ptr += ALIGN_UP(size, DEFAULT_ALIGN);
    }
    assert((size_t)(kws_mem_ptr - kws_mem) == kws_size);
    assert(nibble.ident_map.len == KW_COUNT);

    return true;
}

static void nibble_cleanup(void)
{
    hash_map_destroy(&nibble.str_lit_map);
    hash_map_destroy(&nibble.ident_map);
    allocator_destroy(&nibble.allocator);
}

const char* intern_str_lit(const char* str, size_t len)
{
    Allocator* allocator = &nibble.allocator;
    HashMap* strmap = &nibble.str_lit_map;
    uint64_t key = hash_bytes(str, len);
    uint64_t* pval = hash_map_get(strmap, key);
    InternedStr* intern = pval ? (void*)*pval : NULL;

    // Collisions will only occur if identical hash values are produced. Collisions due to
    // contention for hash map slots will not occur (open addressing).
    //
    // Walk the linked list in case of collision.
    for (InternedStr* it = intern; it; it = it->next) {
        if ((it->len == len) && (cstr_ncmp(it->str, str, len) == 0)) {
            return it->str;
        }
    }

    // If we got here, need to add this string to the intern table.
    InternedStr* new_intern = mem_allocate(allocator, offsetof(InternedStr, str) + len + 1, DEFAULT_ALIGN, false);
    if (new_intern) {
        new_intern->next = intern; // Record collision. If a collision did _NOT_ occur, this will be null.
        new_intern->len = len;

        memcpy(new_intern->str, str, len);
        new_intern->str[len] = '\0';

        hash_map_put(strmap, key, (uintptr_t)new_intern);
    } else {
        // TODO: Handle in a better way.
        fprintf(stderr, "[INTERNAL ERROR]: Out of memory.\n%s:%d\n", __FILE__, __LINE__);
        exit(1);
    }

    return new_intern->str;
}

const char* intern_ident(const char* str, size_t len, bool* is_kw, Keyword* kw)
{
    Allocator* allocator = &nibble.allocator;
    HashMap* strmap = &nibble.ident_map;
    uint64_t key = hash_bytes(str, len);
    uint64_t* pval = hash_map_get(strmap, key);
    InternedIdent* intern = pval ? (void*)*pval : NULL;

    // Collisions will only occur if identical hash values are produced. Collisions due to
    // contention for hash map slots will not occur (open addressing).
    //
    // Walk the linked list in case of collision.
    for (InternedIdent* it = intern; it; it = it->next) {
        if ((it->len == len) && (cstr_ncmp(it->str, str, len) == 0)) {
            if (is_kw) { *is_kw = it->is_kw; }
            if (kw) { *kw = it->kw; }
            return it->str;
        }
    }

    // If we got here, need to add this string to the intern table.
    InternedIdent* new_intern = mem_allocate(allocator, offsetof(InternedIdent, str) + len + 1, DEFAULT_ALIGN, false);
    if (new_intern) {
        new_intern->next = intern; // Record collision. If a collision did _NOT_ occur, this will be null.
        new_intern->len = len;
        new_intern->is_kw = false;
        new_intern->kw = (Keyword)0; // TODO: Have an invalid or none keyword type. Just clean this up in some way!

        memcpy(new_intern->str, str, len);
        new_intern->str[len] = '\0';

        hash_map_put(strmap, key, (uintptr_t)new_intern);
    } else {
        // TODO: Handle in a better way.
        fprintf(stderr, "[INTERNAL ERROR]: Out of memory.\n%s:%d\n", __FILE__, __LINE__);
        exit(1);
    }

    if (is_kw) { *is_kw = new_intern->is_kw; }
    if (kw) { *kw = new_intern->kw; }

    return new_intern->str;
}

static CompiledModule* compile_module(const char* str, ProgPos pos)
{
    Allocator bootstrap = allocator_create(256);
    CompiledModule* module = new_type(&bootstrap, CompiledModule, true);
    module->allocator = bootstrap;
    module->errors = byte_stream_create(&module->allocator);
    module->ast_arena = allocator_create(1024);

    Parser parser = parser_create(&module->ast_arena, str, pos, &module->errors);

#if 0
    char tkn_buf[64];
    while (next_token(&parser)) {
        print_token(&parser.token, tkn_buf, sizeof(tkn_buf));
        ftprint_out("%s\n", tkn_buf);
    }
#elif 0
    next_token(&parser);
    Expr* expr = parse_expr(&parser);

    ftprint_out("Done parsing expr\n");
    AllocatorState state = allocator_get_state(&module->allocator);
    {
        ftprint_out("%s\n", ftprint_expr(&module->allocator, expr));
    }
    allocator_restore_state(state);
#elif 1
    next_token(&parser);
    Decl* decl = parse_decl(&parser);

    ftprint_out("Done parsing decl\n");
    AllocatorState state = allocator_get_state(&module->allocator);
    {
        ftprint_out("%s\n", ftprint_decl(&module->allocator, decl));
    }
    allocator_restore_state(state);
#else
    next_token(&parser);
    Stmt* stmt = parse_stmt(&parser);

    ftprint_out("Done parsing stmt\n");
    AllocatorState state = allocator_get_state(&module->allocator);
    {
        ftprint_out("%s\n", ftprint_stmt(&module->allocator, stmt));
    }
    allocator_restore_state(state);
#endif

    if (module->errors.num_chunks > 0) {
        ftprint_out("Num errors: %lu\n", module->errors.num_chunks);
        ByteStreamChunk* chunk = module->errors.first;

        while (chunk) {
            ftprint_out("%s\n", chunk->buf);
            chunk = chunk->next;
        }
    }

    parser_destroy(&parser);

    return module;
}

static void free_compiled_module(CompiledModule* module)
{
    Allocator bootstrap = module->allocator;

    allocator_destroy(&module->ast_arena);
    allocator_destroy(&bootstrap);
}

int main(void)
{
    if (!nibble_init()) {
        ftprint_err("Failed to initialize\n");
        exit(1);
    }

    /*
    ftprint_out("sizeof(TypeSpec) = %lu, sizeof(Expr) = %lu, sizeof(Stmt) = %lu, sizeof(Decl) = %lu, sizeof(StmtIf) =
    %lu, offsetof(Stmt, as_if) = %lu\n", sizeof(TypeSpec), sizeof(Expr), sizeof(Stmt), sizeof(Decl), sizeof(StmtIf),
    offsetof(Stmt, as_if));
    */

    // CompiledModule* module = compile_module("var a : int = 1 + 2;", 0);
    // CompiledModule* module = compile_module("var a : int = sizeof(int32);", 0);
    // CompiledModule* module = compile_module("var a := 1 + 2;", 0);
    // CompiledModule* module = compile_module("var a : int;", 0);
    // CompiledModule* module = compile_module("var a : int = f(x=a);", 0);
    // CompiledModule* module = compile_module("var a : Vector2 = {x = 10, y = 20 :Vector2};", 0);
    // CompiledModule* module = compile_module("var a : int32= x > 3 ? -2*x : f(1,b=2) - (3.14 + y.val) / z[2];", 0);
    // CompiledModule* module = compile_module("var a: proc([]int32, ^^int32)=>int32;", 0);
    // CompiledModule* module = compile_module("var a: (^int32);", 0);
    // CompiledModule* module = compile_module("var a: struct {a:int32;};", 0);
    CompiledModule* module = compile_module("var a: union {a:int32; b:float32;};", 0);
    //
    // CompiledModule* module = compile_module("var a :;", 0);
    // CompiledModule* module = compile_module("var a;", 0);
    //
    // CompiledModule* module = compile_module("const a : int = 1 + 2;", 0);
    // CompiledModule* module = compile_module("const a := 1 + 2;", 0);
    //
    // CompiledModule* module = compile_module("typedef i8 = int8;", 0);
    //
    // CompiledModule* module = compile_module("enum Kind {A}", 0);
    // CompiledModule* module = compile_module("enum Kind {}", 0);
    // CompiledModule* module = compile_module("enum Kind :int8 { A = 0, B, C }", 0);
    // CompiledModule* module = compile_module("enum Kind :uint32 { A = 1 << 0, B = 1 << 1, C = 1 << 2, }", 0);
    //
    // CompiledModule* module = compile_module("struct Vector2 {x: float32; y:float32;}", 0);
    // CompiledModule* module = compile_module("struct Vector2 {}", 0);
    // CompiledModule* module = compile_module("union Vector2 {}", 0);
    // CompiledModule* module = compile_module("struct Vector2 {u:union{a:int;b:int;};}", 0);
    // CompiledModule* module = compile_module("struct Vector2 {s:struct {a:int;b:int;};}", 0);
    // CompiledModule* module = compile_module("union Vector2 {s:struct {a:int;b:int;}; z:^int;}", 0);
    // CompiledModule* module = compile_module("struct Vector2 {s:struct {};}", 0);
    // CompiledModule* module = compile_module("union Vector2 {data:[2]float32; s: Vec2;}", 0);
    //
    // CompiledModule* module = compile_module("proc add(a:int32, b:int32) =>int32 {}", 0);
    //

    // CompiledModule* module = compile_module("{}", 0);

    // CompiledModule* module = compile_module("x > 3 ? -2*x : f(1,b=2) - (3.14 + y.val) / z[2]", 0);
    // CompiledModule* module = compile_module("a ^ ^b", 0);
    // CompiledModule* module = compile_module("\"abc\"[0]", 0);
    // CompiledModule* module = compile_module("-x:>int*2", 0);
    // CompiledModule* module = compile_module("(-x):>int*2", 0);
    // CompiledModule* module = compile_module("sizeof[1)", 0);
    // CompiledModule* module = compile_module("(a :> (int)) + 2", 0);
    // CompiledModule* module = compile_module("(a:>^int)", 0);
    // CompiledModule* module = compile_module("a:>^int", 0);
    // CompiledModule* module = compile_module("(a :> ^^int)[0]", 0);
    // CompiledModule* module = compile_module("a:>^^int[0]", 0);
    // CompiledModule* module = compile_module("a:>proc(int,)=>int", 0);
    // CompiledModule* module = compile_module("(a:>proc(int)=>int)(10)", 0);
    // CompiledModule* module = compile_module("a:>proc(int)=>int(10)", 0);
    // CompiledModule* module = compile_module("a:>proc(int)=>int:>uint", 0);
    // CompiledModule* module = compile_module("a:>proc(int, float32)=>int:>uint + 2", 0);
    // CompiledModule* module = compile_module("sizeof(int)", 0);
    // CompiledModule* module = compile_module("sizeof(const int)", 0);
    // CompiledModule* module = compile_module("sizeof(^ const char)", 0);
    // CompiledModule* module = compile_module("sizeof([16] ^int)", 0);
    // CompiledModule* module = compile_module("sizeof([16] (proc(int)=>int))", 0);
    // CompiledModule* module = compile_module("sizeof(proc(^^int)=>^int)", 0);
    // CompiledModule* module = compile_module("sizeof(struct {x:int; z:int;})", 0);
    // CompiledModule* module = compile_module("sizeof(union {x:int; z:int;})", 0);
    // CompiledModule* module = compile_module("typeof(1 + 2)", 0);
    // CompiledModule* module = compile_module("typeof(-x:>int*2)", 0);
    // CompiledModule* module = compile_module("f(1,2)", 0);
    // CompiledModule* module = compile_module("3 + f(1+3, a*3, -4.3, x ? a : b)", 0);
    // CompiledModule* module = compile_module("a[1 * 3", 0);
    // CompiledModule* module = compile_module("{x = 1, y = 2 :Vector2}", 0);
    // CompiledModule* module = compile_module("{1, y = 2.0:>int :Vector2}", 0);
    // CompiledModule* module = compile_module("{[0] = 1, [1] = 2 :[]int}", 0);
    // CompiledModule* module = compile_module("{[0] = 1, [1] = 2 :[1]int}", 0);
    // CompiledModule* module = compile_module("1", 0);

    free_compiled_module(module);
    nibble_cleanup();
}
