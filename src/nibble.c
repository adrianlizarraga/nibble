//#define NDEBUG 1
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nibble.h"

#include "allocator.c"
#include "array.c"
#include "hash_map.c"
#include "stream.c"
#include "lexer.c"
#include "ast.c"
#include "parser.c"

#define MAX_ERRORS 20

typedef struct NibbleCtx {
    HashMap ident_map;
    HashMap str_lit_map;
    Allocator allocator;
} NibbleCtx;

typedef struct CompiledModule {
    Allocator allocator;
    ByteStream errors;
} CompiledModule;

static NibbleCtx nibble;

const char* keywords[KW_COUNT];

static StringView keyword_names[KW_COUNT] = {
    [KW_VAR] = string_view_lit("var"),         [KW_CONST] = string_view_lit("const"),
    [KW_ENUM] = string_view_lit("enum"),       [KW_UNION] = string_view_lit("union"),
    [KW_STRUCT] = string_view_lit("struct"),   [KW_FUNC] = string_view_lit("func"),
    [KW_TYPEDEF] = string_view_lit("typedef"), [KW_SIZEOF] = string_view_lit("sizeof"),
    [KW_TYPEOF] = string_view_lit("typeof"),   [KW_GOTO] = string_view_lit("goto"),
    [KW_BREAK] = string_view_lit("break"),     [KW_CONTINUE] = string_view_lit("continue"),
    [KW_RETURN] = string_view_lit("return"),   [KW_IF] = string_view_lit("if"),
    [KW_ELSE] = string_view_lit("else"),       [KW_WHILE] = string_view_lit("while"),
    [KW_DO] = string_view_lit("do"),           [KW_FOR] = string_view_lit("for"),
    [KW_SWITCH] = string_view_lit("switch"),   [KW_CASE] = string_view_lit("case"),
    [KW_DEFAULT] = string_view_lit("default"),
};

static bool nibble_init(void)
{
    nibble.allocator = allocator_create(4096);
    nibble.str_lit_map = hash_map(9, NULL);
    nibble.ident_map = hash_map(9, NULL);

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
    Allocator bootstrap = allocator_create(4096);
    CompiledModule* module = new_type(&bootstrap, CompiledModule, true);
    module->allocator = bootstrap;
    module->errors = byte_stream_create(&module->allocator);

    Parser parser = parser_create(&module->allocator, str, pos, &module->errors);

#if 0
    char tkn_buf[64];
    while (next_token(&parser)) {
        print_token(&parser.token, tkn_buf, sizeof(tkn_buf));
        printf("%s\n", tkn_buf);
    }
#else
    next_token(&parser);
    Expr* expr = parse_expr(&parser);

    print_expr(expr);
    printf("\n");
#endif

    printf("Num errors: %lu\n", module->errors.num_chunks);
    if (module->errors.num_chunks > 0) {
        ByteStreamChunk* chunk = module->errors.first;

        while (chunk) {
            printf("%s\n", chunk->buf);
            chunk = chunk->next;
        }
    }

    parser_destroy(&parser);

    return module;
}

static void free_compiled_module(CompiledModule* module)
{
    Allocator bootstrap = module->allocator;

    allocator_destroy(&bootstrap);
}

int main(void)
{
    if (!nibble_init()) {
        fprintf(stderr, "Failed to initialize\n");
        exit(1);
    }

    // CompiledModule* module = compile_module("int main() { int a = 0; /* comment */ if (a != 1) print(\"hi\"); }", 0);
    // CompiledModule* module = compile_module("x > 3 ? -2*x : x - (3.14 + y.val) / z[2]", 0);
    // CompiledModule* module = compile_module("\"abc\"[0]", 0);
    // CompiledModule* module = compile_module("(:int)-x*2", 0);
    // CompiledModule* module = compile_module("sizeof(1)", 0);
    // CompiledModule* module = compile_module("sizeof(:int)", 0);
    CompiledModule* module = compile_module("3 + f(1+3, a*3, -4.3, x ? a : b)", 0);

    free_compiled_module(module);
    nibble_cleanup();
}
