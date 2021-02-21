#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lexer.c"


#define TKN_TEST_POS(tk, tp, l, c) do { assert(tk.type == tp); assert(tk.line == l); assert(tk.column == c); } while(0)

void test_lexer()
{
    Lexer lexer = {0};
    Token token = {0};

    // Test basic 1 character tokens, newlines, and c++ comments.
    lexer.at = "(++--)  \n  //++--\n()";

    token = next_token(&lexer);
    TKN_TEST_POS(token, TKN_LPAREN, 0, 0);

    token = next_token(&lexer);
    TKN_TEST_POS(token, TKN_PLUS, 0, 1);

    token = next_token(&lexer);
    TKN_TEST_POS(token, TKN_PLUS, 0, 2);

    token = next_token(&lexer);
    TKN_TEST_POS(token, TKN_MINUS, 0, 3);

    token = next_token(&lexer);
    TKN_TEST_POS(token, TKN_MINUS, 0, 4);

    token = next_token(&lexer);
    TKN_TEST_POS(token, TKN_RPAREN, 0, 5);

    token = next_token(&lexer);
    TKN_TEST_POS(token, TKN_LPAREN, 2, 0);

    token = next_token(&lexer);
    TKN_TEST_POS(token, TKN_RPAREN, 2, 1);

    token = next_token(&lexer);
    TKN_TEST_POS(token, TKN_EOF, 2, 2);

    // Test nested c-style comments
    memset(&lexer, 0, sizeof(Lexer));
    lexer.at = "/**** 1 /* 2 */ \n***/+-";

    token = next_token(&lexer);
    TKN_TEST_POS(token, TKN_PLUS, 1, 4);

    token = next_token(&lexer);
    TKN_TEST_POS(token, TKN_MINUS, 1, 5);

    token = next_token(&lexer);
    TKN_TEST_POS(token, TKN_EOF, 1, 6);

    // Test error when have unclosed c-style comments
    memset(&lexer, 0, sizeof(Lexer));
    lexer.at = "/* An unclosed comment";
    token = next_token(&lexer);
    TKN_TEST_POS(token, TKN_EOF, 0, 22);
    assert(lexer.num_errors == 1);

    memset(&lexer, 0, sizeof(Lexer));
    lexer.at = "/* An unclosed comment\n";
    token = next_token(&lexer);
    TKN_TEST_POS(token, TKN_EOF, 1, 0);
    assert(lexer.num_errors == 1);

    memset(&lexer, 0, sizeof(Lexer));
    lexer.at = "123 333\n0xFF 0b0111 011";
    token = next_token(&lexer);
    assert(token.type == TKN_INT);
    assert(token.int_.value == 123);

    token = next_token(&lexer);
    assert(token.type == TKN_INT);
    assert(token.int_.base == 10);
    assert(token.int_.value == 333);

    token = next_token(&lexer);
    assert(token.type == TKN_INT);
    assert(token.int_.base == 16);
    assert(token.int_.value == 0xFF);

    token = next_token(&lexer);
    assert(token.type == TKN_INT);
    assert(token.int_.base == 2);
    assert(token.int_.value == 7);

    token = next_token(&lexer);
    assert(token.type == TKN_INT);
    assert(token.int_.base == 8);
    assert(token.int_.value == 9);

    memset(&lexer, 0, sizeof(Lexer));
    lexer.at = "0Z";
    token = next_token(&lexer);
    assert(lexer.num_errors == 1);

    memset(&lexer, 0, sizeof(Lexer));
    lexer.at = "0b3";
    token = next_token(&lexer);
    assert(lexer.num_errors == 1);

    memset(&lexer, 0, sizeof(Lexer));
    lexer.at = "09";
    token = next_token(&lexer);
    assert(lexer.num_errors == 1);

    memset(&lexer, 0, sizeof(Lexer));
    lexer.at = "1A";
    token = next_token(&lexer);
    assert(lexer.num_errors == 1);

    memset(&lexer, 0, sizeof(Lexer));
    lexer.at = "9999999999999999999999";
    token = next_token(&lexer);
    assert(lexer.num_errors == 1);

    memset(&lexer, 0, sizeof(Lexer));
    lexer.at = "1.23 .23 1.33E2";
    token = next_token(&lexer);
    assert(token.type == TKN_FLOAT);
    assert(token.float_.value == 1.23);

    token = next_token(&lexer);
    assert(token.type == TKN_FLOAT);
    assert(token.float_.value == .23);

    token = next_token(&lexer);
    assert(token.type == TKN_FLOAT);
    assert(token.float_.value == 1.33E2);

    memset(&lexer, 0, sizeof(Lexer));
    lexer.at = "1.33ea";
    token = next_token(&lexer);
    assert(lexer.num_errors == 1);

    memset(&lexer, 0, sizeof(Lexer));
    lexer.at = "1.33e1000000000";
    token = next_token(&lexer);
    assert(lexer.num_errors == 1);
}

int main(void)
{
    printf("Nibble!\n");
    test_lexer();
}
