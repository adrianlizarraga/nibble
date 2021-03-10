//#define NDEBUG 1
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "allocator.c"
#include "array.c"
#include "hash_map.c"
#include "lexer.c"

typedef struct TestProgContext {
    int num_errors;
    Allocator allocator;
} TestProgContext;

static TestProgContext g_ctx;

static void test_on_error(void* data, ProgPos pos, const char* msg)
{
    if (data) {
        TestProgContext* c = data;

        c->num_errors += 1;
        printf("[ERROR]:%u: %s\n", pos, msg);
    }
}

static const char* test_on_str(void* data, ProgPos pos, const char* str, size_t len)
{
    (void)pos;

    if (data) {
        TestProgContext* c = data;

        char* dup = new_array(&c->allocator, char, len + 1, false);

        for (size_t i = 0; i < len; ++i) {
            dup[i] = str[i];
        }
        dup[len] = '\0';

        return dup;
    }

    return NULL;
}

static const char* test_on_identifier(void* data, ProgPos pos, const char* str, size_t len)
{
    (void)pos;

    if (data) {
        TestProgContext* c = data;

        char* dup = new_array(&c->allocator, char, len + 1, false);

        for (size_t i = 0; i < len; ++i) {
            dup[i] = str[i];
        }
        dup[len] = '\0';

        return dup;
    }

    return NULL;
}

#define TKN_TEST_POS(tk, tp, a, b)                                                                                     \
    do {                                                                                                               \
        assert((tk.kind == tp));                                                                                       \
        assert((tk.start == a));                                                                                       \
        assert((tk.end == b));                                                                                         \
    } while (0)
#define TKN_TEST_INT(tk, b, v)                                                                                         \
    do {                                                                                                               \
        assert((tk.kind == TKN_INT));                                                                                  \
        assert((tk.tint.rep == b));                                                                                    \
        assert((tk.tint.value == v));                                                                                  \
    } while (0)
#define TKN_TEST_FLOAT(tk, v)                                                                                          \
    do {                                                                                                               \
        assert((tk.kind == TKN_FLOAT));                                                                                \
        assert((tk.tfloat.value == v));                                                                                \
    } while (0)
#define TKN_TEST_CHAR(tk, v)                                                                                           \
    do {                                                                                                               \
        assert((tk.kind == TKN_INT));                                                                                  \
        assert((tk.tint.rep == TKN_INT_CHAR));                                                                         \
        assert((tk.tint.value == v));                                                                                  \
    } while (0)
#define TKN_TEST_STR(tk, v)                                                                                            \
    do {                                                                                                               \
        assert((tk.kind == TKN_STR));                                                                                  \
        assert(strcmp(tk.tstr.value, v) == 0);                                                                         \
    } while (0)
#define TKN_TEST_IDEN(tk, v)                                                                                           \
    do {                                                                                                               \
        assert((tk.kind == TKN_IDENTIFIER));                                                                           \
        assert(strcmp(tk.tidentifier.value, v) == 0);                                                                  \
    } while (0)
static void test_init_lexer(Lexer* lexer, const char* str, ProgPos start)
{
    free_lexer(lexer);
    init_lexer(lexer, str, start);
    lexer->client.data = &g_ctx;
    lexer->client.on_error = test_on_error;
    lexer->client.on_str = test_on_str;
    lexer->client.on_identifier = test_on_identifier;

    g_ctx.num_errors = 0;
}

static void test_lexer(void)
{
    Lexer lexer = {0};

    // Test basic 1 character tokens, newlines, and c++ comments.
    unsigned int i = 10;
    test_init_lexer(&lexer, "(+[]-)  \n  //++--\n{;:,./}", i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_LPAREN, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_PLUS, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_LBRACE, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_RBRACE, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_MINUS, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_RPAREN, i, ++i);

    i += 12;
    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_LBRACKET, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_SEMICOLON, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_COLON, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_COMMA, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_DOT, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_DIV, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_RBRACKET, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_EOF, i, ++i);

    // Test nested c-style comments
    test_init_lexer(&lexer, "/**** 1 /* 2 */ \n***/+-", 0);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_PLUS, 21, 22);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_MINUS, 22, 23);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_EOF, 23, 24);

    // Test error when have unclosed c-style comments
    test_init_lexer(&lexer, "/* An unclosed comment", 0);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_EOF, 22, 23);
    assert(g_ctx.num_errors == 1);

    test_init_lexer(&lexer, "/* An unclosed comment\n", 0);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_EOF, strlen(lexer.str), strlen(lexer.str) + 1);
    assert(g_ctx.num_errors == 1);

    // Test integer literals
    test_init_lexer(&lexer, "123 333\n0xFF 0b0111 011", 0);

    next_token(&lexer);
    TKN_TEST_INT(lexer.token, TKN_INT_DEC, 123);

    next_token(&lexer);
    TKN_TEST_INT(lexer.token, TKN_INT_DEC, 333);

    next_token(&lexer);
    TKN_TEST_INT(lexer.token, TKN_INT_HEX, 0xFF);

    next_token(&lexer);
    TKN_TEST_INT(lexer.token, TKN_INT_BIN, 7);

    next_token(&lexer);
    TKN_TEST_INT(lexer.token, TKN_INT_OCT, 9);

    next_token(&lexer);
    assert(lexer.token.kind == TKN_EOF);

    test_init_lexer(&lexer, "0Z 0b3 09 1A\n999999999999999999999999", 0);

    next_token(&lexer);
    assert(g_ctx.num_errors == 1);

    next_token(&lexer);
    assert(g_ctx.num_errors == 2);

    next_token(&lexer);
    assert(g_ctx.num_errors == 3);

    next_token(&lexer);
    assert(g_ctx.num_errors == 4);

    next_token(&lexer);
    assert(g_ctx.num_errors == 5);

    next_token(&lexer);
    assert(lexer.token.kind == TKN_EOF);

    // Test floating point literals
    test_init_lexer(&lexer, "1.23 .23 1.33E2", 0);

    next_token(&lexer);
    TKN_TEST_FLOAT(lexer.token, 1.23);

    next_token(&lexer);
    TKN_TEST_FLOAT(lexer.token, .23);

    next_token(&lexer);
    TKN_TEST_FLOAT(lexer.token, 1.33E2);

    test_init_lexer(&lexer, "1.33ea 1.33e100000000000", 0);

    next_token(&lexer);
    assert(g_ctx.num_errors == 1);

    next_token(&lexer);
    assert(g_ctx.num_errors == 2);

    // Test character literals
    test_init_lexer(&lexer,
                    "'a' '1' ' ' '\\0' '\\a' '\\b' '\\f' '\\n' '\\r' '\\t' '\\v' "
                    "'\\\\' '\\'' '\\\"' '\\?'",
                    0);

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, 'a');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '1');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, ' ');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\0');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\a');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\b');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\f');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\n');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\r');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\t');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\v');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\\');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\'');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '"');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '?');

    next_token(&lexer);
    assert(lexer.token.kind == TKN_EOF);
    assert(g_ctx.num_errors == 0);

    test_init_lexer(&lexer, "'\\x12'  '\\x3'", 0);

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\x12');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\x3');

    test_init_lexer(&lexer, "'' 'a '\n' '\\z' '\\0'", 0);

    next_token(&lexer);
    assert(g_ctx.num_errors == 1);

    next_token(&lexer);
    assert(g_ctx.num_errors == 2);

    next_token(&lexer);
    assert(g_ctx.num_errors == 3);

    next_token(&lexer);
    assert(g_ctx.num_errors == 4);

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\0');

    next_token(&lexer);
    assert(lexer.token.kind == TKN_EOF);

    // Test basic string literals
    {
        const char* str = "\"hello world\" \"a\\nb\" \n \"\\x50 a \\x51\" \"\" \"\\\"nested\\\"\"";
        test_init_lexer(&lexer, str, 0);

        next_token(&lexer);
        TKN_TEST_STR(lexer.token, "hello world");

        next_token(&lexer);
        TKN_TEST_STR(lexer.token, "a\nb");

        next_token(&lexer);
        TKN_TEST_STR(lexer.token, "\x50 a \x51");

        next_token(&lexer);
        TKN_TEST_STR(lexer.token, "");

        next_token(&lexer);
        TKN_TEST_STR(lexer.token, "\"nested\"");
    }

    // Test errors when scanning string literals
    {
        const char* str = "\"\n\" \"\\xTF\" \"\\W\" \"unclosed";
        test_init_lexer(&lexer, str, 0);

        next_token(&lexer);
        assert(g_ctx.num_errors == 1);

        next_token(&lexer);
        assert(g_ctx.num_errors == 2);

        next_token(&lexer);
        assert(g_ctx.num_errors == 3);

        next_token(&lexer);
        assert(g_ctx.num_errors == 4);
    }

    // Test basic identifiers
    {
        const char* str = "var x1a x11 _abc abc_ _ab_c_ i";
        test_init_lexer(&lexer, str, 0);

        next_token(&lexer);
        TKN_TEST_IDEN(lexer.token, "var");

        next_token(&lexer);
        TKN_TEST_IDEN(lexer.token, "x1a");

        next_token(&lexer);
        TKN_TEST_IDEN(lexer.token, "x11");

        next_token(&lexer);
        TKN_TEST_IDEN(lexer.token, "_abc");

        next_token(&lexer);
        TKN_TEST_IDEN(lexer.token, "abc_");

        next_token(&lexer);
        TKN_TEST_IDEN(lexer.token, "_ab_c_");

        next_token(&lexer);
        TKN_TEST_IDEN(lexer.token, "i");
    }

    // Test invalid identifier combinations.
    {
        const char* str = "1var";
        test_init_lexer(&lexer, str, 0);

        next_token(&lexer);
        TKN_TEST_INT(lexer.token, TKN_INT_DEC, 1);

        next_token(&lexer);
        TKN_TEST_IDEN(lexer.token, "var");
    }
    free_lexer(&lexer);
}

int main(void)
{
    g_ctx.allocator = allocator_create(4096);
    printf("Nibble!\n");
    test_lexer();
    allocator_destroy(&g_ctx.allocator);
}
