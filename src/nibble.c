//#define NDEBUG 1
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "allocator.c"
#include "array.c"
#include "hash_map.c"
#include "lexer.c"
#include "ast.c"

typedef struct NibbleCtx {
    int num_errors;
    Allocator allocator;
    HashMap strmap;
} NibbleCtx;

static void nibble_on_error(void* data, ProgPos pos, const char* msg)
{
    if (data) {
        NibbleCtx* c = data;

        c->num_errors += 1;
        printf("[ERROR]:%u: %s\n", pos, msg);
    }
}

static void nibble_on_line(void* data, ProgPos pos)
{
    (void)data;
    (void)pos;
}

static const char* nibble_on_str(void* data, ProgPos pos, const char* str, size_t len)
{
    (void)pos;

    if (data) {
        NibbleCtx* c = data;

        char* dup = new_array(&c->allocator, char, len + 1, false);

        for (size_t i = 0; i < len; ++i) {
            dup[i] = str[i];
        }
        dup[len] = '\0';

        return dup;
    }

    return NULL;
}

static const char* nibble_on_identifier(void* data, ProgPos pos, const char* str, size_t len)
{
    (void)pos;

    if (data) {
        NibbleCtx* c = data;

        return str_intern(&c->allocator, &c->strmap, str, len);
    }

    return NULL;
}

int main(void)
{
    NibbleCtx ctx;
    ctx.allocator = allocator_create(4096);
    ctx.strmap = hash_map(9, NULL);

    Lexer lexer = lexer_from_str("int main() { int a = 0; if (a == 1) print(\"hi\"); }", 0);

    lexer_set_client(&lexer, &ctx, nibble_on_error, nibble_on_line, nibble_on_identifier, nibble_on_str);

    char tkn_buf[64];
    while (next_token(&lexer)) {
        print_token(&lexer.token, tkn_buf, sizeof(tkn_buf));
        printf("%s\n", tkn_buf); 
    }

    free_lexer(&lexer);
    hash_map_destroy(&ctx.strmap);
    allocator_destroy(&ctx.allocator);
}
