#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lexer.c"


#define TKN_TEST_POS(tk, tp, l, c) do { assert((tk.type == tp)); assert((tk.pos.line == l)); assert((tk.pos.column == c)); } while(0)
#define TKN_TEST_INT(tk, b, v) do { assert((tk.type == TKN_INT)); assert((tk.int_.base = b)); assert((tk.int_.value == v)); } while(0)
#define TKN_TEST_FLOAT(tk, v) do { assert((tk.type == TKN_FLOAT)); assert((tk.float_.value == v)); } while(0)
#define TKN_TEST_CHAR(tk, v) do { assert((tk.type == TKN_CHAR)); assert((tk.char_.value == v)); } while(0)

void test_lexer()
{
    Lexer lexer = {0};

    // Test basic 1 character tokens, newlines, and c++ comments.
    lexer.at = "(+[]-)  \n  //++--\n{;:,./}";
    lexer.line_start = lexer.at;

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_LPAREN, 0, 0);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_PLUS, 0, 1);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_LBRACE, 0, 2);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_RBRACE, 0, 3);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_MINUS, 0, 4);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_RPAREN, 0, 5);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_LBRACKET, 2, 0);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_SEMICOLON, 2, 1);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_COLON, 2, 2);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_COMMA, 2, 3);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_DOT, 2, 4);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_DIV, 2, 5);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_RBRACKET, 2, 6);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_EOF, 2, 7);

    // Test nested c-style comments
    memset(&lexer, 0, sizeof(Lexer));
    lexer.at = "/**** 1 /* 2 */ \n***/+-";
    lexer.line_start = lexer.at;

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_PLUS, 1, 4);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_MINUS, 1, 5);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_EOF, 1, 6);

    // Test error when have unclosed c-style comments
    memset(&lexer, 0, sizeof(Lexer));
    lexer.at = "/* An unclosed comment";
    lexer.line_start = lexer.at;

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_EOF, 0, 22);
    assert(lexer.num_errors == 1);

    memset(&lexer, 0, sizeof(Lexer));
    lexer.at = "/* An unclosed comment\n";
    lexer.line_start = lexer.at;

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_EOF, 1, 0);
    assert(lexer.num_errors == 1);

    // Test integer literals
    memset(&lexer, 0, sizeof(Lexer));
    lexer.at = "123 333\n0xFF 0b0111 011";
    lexer.line_start = lexer.at;

    next_token(&lexer);
    TKN_TEST_INT(lexer.token, 10, 123);

    next_token(&lexer);
    TKN_TEST_INT(lexer.token, 10, 333);

    next_token(&lexer);
    TKN_TEST_INT(lexer.token, 16, 0xFF);

    next_token(&lexer);
    TKN_TEST_INT(lexer.token, 2, 7);

    next_token(&lexer);
    TKN_TEST_INT(lexer.token, 8, 9);

    next_token(&lexer);
    assert(lexer.token.type == TKN_EOF);

    memset(&lexer, 0, sizeof(Lexer));
    lexer.at = "0Z 0b3 09 1A\n999999999999999999999999";
    lexer.line_start = lexer.at;

    next_token(&lexer);
    assert(lexer.num_errors == 1);

    next_token(&lexer);
    assert(lexer.num_errors == 2);

    next_token(&lexer);
    assert(lexer.num_errors == 3);

    next_token(&lexer);
    assert(lexer.num_errors == 4);

    next_token(&lexer);
    assert(lexer.num_errors == 5);

    next_token(&lexer);
    assert(lexer.token.type == TKN_EOF);

    // Test floating point literals
    memset(&lexer, 0, sizeof(Lexer));
    lexer.at = "1.23 .23 1.33E2";
    lexer.line_start = lexer.at;

    next_token(&lexer);
    TKN_TEST_FLOAT(lexer.token, 1.23);

    next_token(&lexer);
    TKN_TEST_FLOAT(lexer.token, .23);

    next_token(&lexer);
    TKN_TEST_FLOAT(lexer.token, 1.33E2);

    memset(&lexer, 0, sizeof(Lexer));
    lexer.at = "1.33ea 1.33e100000000000";
    lexer.line_start = lexer.at;

    next_token(&lexer);
    assert(lexer.num_errors == 1);

    next_token(&lexer);
    assert(lexer.num_errors == 2);

    // Test character literals
    memset(&lexer, 0, sizeof(Lexer));
    lexer.at = "'a' '1' ' ' '\\0' '\\a' '\\b' '\\f' '\\n' '\\r' '\\t' '\\v' '\\\\' '\\'' '\\\"' '\\?'";
    lexer.line_start = lexer.at;

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
    assert(lexer.token.type == TKN_EOF);
    assert(lexer.num_errors == 0);

    memset(&lexer, 0, sizeof(Lexer));
    lexer.at = "'\\x12'  '\\x3'";
    lexer.line_start = lexer.at;

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\x12');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\x3');

    memset(&lexer, 0, sizeof(Lexer));
    lexer.at = "'' 'a '\n' '\\z' '\\0'";
    lexer.line_start = lexer.at;

    next_token(&lexer);
    assert(lexer.num_errors == 1);

    next_token(&lexer);
    assert(lexer.num_errors == 2);

    next_token(&lexer);
    assert(lexer.num_errors == 3);

    next_token(&lexer);
    assert(lexer.num_errors == 4);

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\0');

    next_token(&lexer);
    assert(lexer.token.type == TKN_EOF);
}

int main(void)
{
    printf("Nibble!\n");
    test_lexer();
}
