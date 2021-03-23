//#define NDEBUG 1
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

void nibble_init(void)
{
    nibble.allocator = allocator_create(4096);
    nibble.str_lit_map = hash_map(9, NULL);
    nibble.ident_map = hash_map(9, NULL);
}

void nibble_cleanup(void)
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

    char tkn_buf[64];
    while (next_token(&parser)) {
        print_token(&parser.token, tkn_buf, sizeof(tkn_buf));
        printf("%s\n", tkn_buf); 
    }

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
    nibble_init();

    CompiledModule* module = compile_module("int main() { int a = 0; /* comment */ if (a == 1) print(\"hi\"); }", 0);

    free_compiled_module(module);
    nibble_cleanup();
}
