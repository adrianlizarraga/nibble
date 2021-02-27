#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lexer.c"

#define TKN_TEST_POS(tk, tp, a, b)                                                                                     \
    do {                                                                                                               \
        assert((tk.type == tp));                                                                                       \
        assert((tk.start == a));                                                                                       \
        assert((tk.end == b));                                                                                         \
    } while (0)
#define TKN_TEST_INT(tk, b, v)                                                                                         \
    do {                                                                                                               \
        assert((tk.type == TKN_INT));                                                                                  \
        assert((tk.tint.base = b));                                                                                    \
        assert((tk.tint.value == v));                                                                                  \
    } while (0)
#define TKN_TEST_FLOAT(tk, v)                                                                                          \
    do {                                                                                                               \
        assert((tk.type == TKN_FLOAT));                                                                                \
        assert((tk.tfloat.value == v));                                                                                \
    } while (0)
#define TKN_TEST_CHAR(tk, v)                                                                                           \
    do {                                                                                                               \
        assert((tk.type == TKN_CHAR));                                                                                 \
        assert((tk.tchar.value == v));                                                                                 \
    } while (0)

void test_lexer()
{
    Lexer lexer = {0};
    int ret = 0;

    // Test basic 1 character tokens, newlines, and c++ comments.
    unsigned int i = 10;
    init_lexer(&lexer, "(+[]-)  \n  //++--\n{;:,./}", i);

    ret = next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_LPAREN, i, ++i);

    ret = next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_PLUS, i, ++i);

    ret = next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_LBRACE, i, ++i);

    ret = next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_RBRACE, i, ++i);

    ret = next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_MINUS, i, ++i);

    ret = next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_RPAREN, i, ++i);

    i += 12;
    ret = next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_LBRACKET, i, ++i);

    ret = next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_SEMICOLON, i, ++i);

    ret = next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_COLON, i, ++i);

    ret = next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_COMMA, i, ++i);

    ret = next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_DOT, i, ++i);

    ret = next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_DIV, i, ++i);

    ret = next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_RBRACKET, i, ++i);

    ret = next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_EOF, i, ++i);

    // Test nested c-style comments
    init_lexer(&lexer, "/**** 1 /* 2 */ \n***/+-", 0);

    ret = next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_PLUS, 21, 22);

    ret = next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_MINUS, 22, 23);

    ret = next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_EOF, 23, 24);

    // Test error when have unclosed c-style comments
    init_lexer(&lexer, "/* An unclosed comment", 0);

    ret = next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_EOF, 22, 23);
    assert(ret >= 1);

    init_lexer(&lexer, "/* An unclosed comment\n", 0);

    ret = next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_EOF, strlen(lexer.str), strlen(lexer.str) + 1);
    assert(ret >= 1);

    // Test integer literals
    init_lexer(&lexer, "123 333\n0xFF 0b0111 011", 0);

    ret = next_token(&lexer);
    TKN_TEST_INT(lexer.token, 10, 123);

    ret = next_token(&lexer);
    TKN_TEST_INT(lexer.token, 10, 333);

    ret = next_token(&lexer);
    TKN_TEST_INT(lexer.token, 16, 0xFF);

    ret = next_token(&lexer);
    TKN_TEST_INT(lexer.token, 2, 7);

    ret = next_token(&lexer);
    TKN_TEST_INT(lexer.token, 8, 9);

    ret = next_token(&lexer);
    assert(lexer.token.type == TKN_EOF);

    init_lexer(&lexer, "0Z 0b3 09 1A\n999999999999999999999999", 0);

    ret = next_token(&lexer);
    assert(ret >= 1);

    ret = next_token(&lexer);
    assert(ret >= 1);

    ret = next_token(&lexer);
    assert(ret >= 1);

    ret = next_token(&lexer);
    assert(ret >= 1);

    ret = next_token(&lexer);
    assert(ret >= 1);

    ret = next_token(&lexer);
    assert(lexer.token.type == TKN_EOF);

    // Test floating point literals
    init_lexer(&lexer, "1.23 .23 1.33E2", 0);

    ret = next_token(&lexer);
    TKN_TEST_FLOAT(lexer.token, 1.23);

    ret = next_token(&lexer);
    TKN_TEST_FLOAT(lexer.token, .23);

    ret = next_token(&lexer);
    TKN_TEST_FLOAT(lexer.token, 1.33E2);

    init_lexer(&lexer, "1.33ea 1.33e100000000000", 0);

    ret = next_token(&lexer);
    assert(ret >= 1);

    ret = next_token(&lexer);
    assert(ret >= 1);

    // Test character literals
    init_lexer(&lexer,
               "'a' '1' ' ' '\\0' '\\a' '\\b' '\\f' '\\n' '\\r' '\\t' '\\v' "
               "'\\\\' '\\'' '\\\"' '\\?'",
               0);

    ret = next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, 'a');

    ret = next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '1');

    ret = next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, ' ');

    ret = next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\0');

    ret = next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\a');

    ret = next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\b');

    ret = next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\f');

    ret = next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\n');

    ret = next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\r');

    ret = next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\t');

    ret = next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\v');

    ret = next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\\');

    ret = next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\'');

    ret = next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '"');

    ret = next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '?');

    ret = next_token(&lexer);
    assert(lexer.token.type == TKN_EOF);
    assert(ret == 0);

    init_lexer(&lexer, "'\\x12'  '\\x3'", 0);

    ret = next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\x12');

    ret = next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\x3');

    init_lexer(&lexer, "'' 'a '\n' '\\z' '\\0'", 0);

    ret = next_token(&lexer);
    assert(ret >= 1);

    ret = next_token(&lexer);
    assert(ret >= 1);

    ret = next_token(&lexer);
    assert(ret >= 1);

    ret = next_token(&lexer);
    assert(ret >= 1);

    ret = next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\0');

    ret = next_token(&lexer);
    assert(lexer.token.type == TKN_EOF);
}

int main(void)
{
    printf("Nibble!\n");
    test_lexer();
}
