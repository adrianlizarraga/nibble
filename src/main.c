//#define NDEBUG 1
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "allocator.c"
#include "cstring.c"
#include "print.c"
#include "array.c"
#include "hash_map.c"
#include "stream.c"
#include "nibble.c"
#include "lexer.c"
#include "ast.c"
#include "parser.c"

typedef struct CompiledModule {
    Allocator allocator;
    Allocator ast_arena;
    ByteStream errors;
} CompiledModule;

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

    ftprint_out("%s\n", ftprint_expr(&module->allocator, expr));
    ftprint_out("\n\n");
#elif 1
    next_token(&parser);
    Decl* decl = parse_decl(&parser);

    ftprint_out("%s\n", ftprint_decl(&module->allocator, decl));
    ftprint_out("\n\n");
#else
    next_token(&parser);
    Stmt* stmt = parse_stmt(&parser);

    ftprint_out("%s\n", ftprint_stmt(&module->allocator, stmt));
    ftprint_out("\n\n");
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

#ifndef NDEBUG
    print_allocator_stats(&module->ast_arena, "AST mem stats");
    print_allocator_stats(&module->allocator, "Module mem stats");
#endif

    allocator_destroy(&module->ast_arena);
    allocator_destroy(&bootstrap);
}

int main(void)
{
    if (!nibble_init()) {
        ftprint_err("Failed to initialize\n");
        exit(1);
    }

    // sizeof(TypeSpec) = 48, sizeof(Expr) = 48, sizeof(Stmt) = 32, sizeof(Decl) = 80
    // sizeof(TypeSpec) = 48, sizeof(Expr) = 48, sizeof(Stmt) = 32, sizeof(Decl) = 24
    // sizeof(TypeSpec) = 12, sizeof(Expr) = 48, sizeof(Stmt) = 32, sizeof(Decl) = 24
    // sizeof(TypeSpec) = 12, sizeof(Expr) = 12, sizeof(Stmt) = 32, sizeof(Decl) = 24
    // ftprint_out("sizeof(TypeSpec) = %lu, sizeof(Expr) = %lu, sizeof(Stmt) = %lu, sizeof(Decl) = %lu\n\n",
    // sizeof(TypeSpec), sizeof(Expr), sizeof(Stmt), sizeof(Decl));

    // CompiledModule* module = compile_module("var a : int = 1 + 2;", 0);
    // CompiledModule* module = compile_module("var a : int = sizeof(int32);", 0);
    // CompiledModule* module = compile_module("var a : int = typeof(int);", 0);
    // CompiledModule* module = compile_module("var a := 1 + 2;", 0);
    // CompiledModule* module = compile_module("var a : int;", 0);
    // CompiledModule* module = compile_module("var a : int = f(x=a);", 0);
    // CompiledModule* module = compile_module("var a : Vector2 = {x = 10, y = 20 :Vector2};", 0);
    // CompiledModule* module = compile_module("var a : int32= x > 3 ? -2*x : f(1,b=2) - (3.14 + y.val) / z[2];", 0);
    // CompiledModule* module = compile_module("var a: proc([]int32, ^^int32)=>int32;", 0);
    // CompiledModule* module = compile_module("var a: proc(x:[]int32, ^^int32)=>int32;", 0);
    // CompiledModule* module = compile_module("var a: (^int32);", 0);
    // CompiledModule* module = compile_module("var a: struct {a:int32;};", 0);
    // CompiledModule* module = compile_module("var a: union {a:int32; b:float32;};", 0);
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
    // AST usage: 848 bytes, Nibble usage: 951 bytes
    // AST usage: 840 bytes, Nibble usage: 951 bytes
    // AST usage: 760 bytes, Nibble usage: 951 bytes
    // AST usage: 600 bytes, Nibble usage: 951 bytes
    // AST usage: 624 bytes, Nibble usage: 951 bytes (dont require {} as body for loops etc)
    // CompiledModule* module = compile_module("proc add(a:int32, b:int32) =>int32 {if(a == 2) {g = 2*a;}}", 0);
    // CompiledModule* module = compile_module("proc add(a:int32, b:int32) =>int32 {if(a == 2) {g = 2*a;} else g=0;}", 0);
    // CompiledModule* module = compile_module("proc add(a:int32, b:int32) =>int32 {if(a == 2) {g = 2*a; f(g);}}", 0);
    // CompiledModule* module = compile_module("proc add(a:int32, b:int32) =>void {for(var i:=0;i<10;i+=1){f(i);}}", 0);
    // CompiledModule* module = compile_module("proc add(a:int32, b:int32) =>int32 {return 10;}", 0);
    // CompiledModule* module = compile_module("proc add(a:int32, b:int32) =>void {return;}", 0);
    // CompiledModule* module = compile_module("proc add(a:int32, b:int32) =>void {for(;i<10;i+=1){f(i);}}", 0);
    // CompiledModule* module = compile_module("proc add(a:int32, b:int32) =>void {while(a > b){f(i);}}", 0);
    // CompiledModule* module = compile_module("proc add(a:int32, b:int32) =>void {do{f(i);}while(a > b);}", 0);
    // CompiledModule* module = compile_module("proc add(a:int32, b:int32) =>void {do f(i); while(a > b);}", 0);
    // CompiledModule* module = compile_module("proc add(a:int32, b:int32) =>void {for(;;i+=1){f(i);}}", 0);
    // CompiledModule* module = compile_module("proc add(a:int32, b:int32) =>void {for(;i != 0;){f(i);}}", 0);
    // CompiledModule* module = compile_module("proc add(a:int32, b:int32) =>void {for(;;){f(i);}}", 0);
    // CompiledModule* module = compile_module("proc add(a:int32, b:int32) =>void {for(;;){break;}}", 0);
    // CompiledModule* module = compile_module("proc add(a:int32, b:int32) =>void {for(;;){continue;}}", 0);
    // CompiledModule* module = compile_module("proc add(a:int32, b:int32) =>void {label top: ; goto top;}", 0);
    CompiledModule* module = compile_module("proc add(a:int32, b:int32) =>void {switch(a){case 1 .. 2: a += 1; case: a = 0;}}", 0);
    // CompiledModule* module = compile_module("proc add(a:int32, b:int32) =>void {for(i=0;i<10;i+=1){f(i);}}", 0);
    // CompiledModule* module = compile_module("proc add(a:int32, b:int32) =>void {for(g(^i);i<10;i+=1){f(i);}}", 0);
    //
    // CompiledModule* module = compile_module("proc add(a:int32, b:int32) =>void {;}", 0);

    // CompiledModule* module = compile_module("{var a:int32 = 0;}", 0);
    // CompiledModule* module = compile_module("{var a:int32 = 0; var b:float32 = 1.0;}", 0);
    // CompiledModule* module = compile_module("{var a:int32 = 0; f(a);}", 0);
    // CompiledModule* module = compile_module("{var a:int32 = 1 << 8; a *= 2;}", 0);
    // CompiledModule* module = compile_module("{var a:int32 = 3; while(a > 0){var b:int32 = 2; f(a); a -= 1;}}", 0);
    // CompiledModule* module = compile_module("{var a:int32 = 3; do{var b:int32 = 2; f(a); a -= 1;} while(a > 0);}",
    // 0); CompiledModule* module = compile_module("{if(a) {a = 3;}}", 0); CompiledModule* module =
    // compile_module("{if(a) {a = 3;} else{b = 4;}}", 0); CompiledModule* module = compile_module("{if(a) {a = 3;}
    // elif(b == 2) {c = 10;} else{b = 4;}}", 0); CompiledModule* module = compile_module("{if(a) {a = 3;} elif(b == 2)
    // {b = 10;} elif(c == 3) {c = 1;} else{b = 4;}}", 0);

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
