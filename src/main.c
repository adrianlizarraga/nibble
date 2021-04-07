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
        kws_size += offsetof(InternedStr, str) + keyword_names[i].len + 1;
    }

    char* kws_mem = mem_allocate(&nibble.allocator, kws_size, DEFAULT_ALIGN, false);
    if (!kws_mem) {
        return false;
    }

    for (int i = 0; i < KW_COUNT; ++i) {
        const char* str = keyword_names[i].str;
        size_t len = keyword_names[i].len;
        size_t size = offsetof(InternedStr, str) + len + 1;
        InternedStr* kw = (void*)kws_mem;

        kw->next = NULL;
        kw->len = len;

        memcpy(kw->str, str, len);
        kw->str[len] = '\0';

        hash_map_put(&nibble.ident_map, hash_bytes(str, len), (uintptr_t)kw);
        keywords[i] = kw->str;

        kws_mem += size;
    }
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
    return str_intern(&nibble.allocator, &nibble.str_lit_map, str, len);
}

const char* intern_ident(const char* str, size_t len)
{
    return str_intern(&nibble.allocator, &nibble.ident_map, str, len);
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

    CompiledModule* module = compile_module("var a : int = 1 + 2;", 0);
    // CompiledModule* module = compile_module("var a := 1 + 2;", 0);
    // CompiledModule* module = compile_module("var a : int;", 0);
    // CompiledModule* module = compile_module("var a : int = f(x=a);", 0);
    // CompiledModule* module = compile_module("var a : Vector2 = {x = 10, y = 20 :Vector2};", 0);
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
    // CompiledModule* module = compile_module("#sizeof[1)", 0);
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
    // CompiledModule* module = compile_module("#sizeof(int)", 0);
    // CompiledModule* module = compile_module("#sizeof(const int)", 0);
    // CompiledModule* module = compile_module("#sizeof(^ const char)", 0);
    // CompiledModule* module = compile_module("#sizeof([16] ^int)", 0);
    // CompiledModule* module = compile_module("#sizeof([16] (proc(int)=>int))", 0);
    // CompiledModule* module = compile_module("#sizeof(proc(^^int)=>^int)", 0);
    // CompiledModule* module = compile_module("#sizeof(struct {x:int; z:int;})", 0);
    // CompiledModule* module = compile_module("#sizeof(union {x:int; z:int;})", 0);
    // CompiledModule* module = compile_module("#typeof(1 + 2)", 0);
    // CompiledModule* module = compile_module("#typeof(-x:>int*2)", 0);
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
