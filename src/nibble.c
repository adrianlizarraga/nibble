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
    lexer.at = "(+[]-)  \n  //++--\n{;:,./}";

    token = next_token(&lexer);
    TKN_TEST_POS(token, TKN_LPAREN, 0, 0);

    token = next_token(&lexer);
    TKN_TEST_POS(token, TKN_PLUS, 0, 1);

    token = next_token(&lexer);
    TKN_TEST_POS(token, TKN_LBRACE, 0, 2);

    token = next_token(&lexer);
    TKN_TEST_POS(token, TKN_RBRACE, 0, 3);

    token = next_token(&lexer);
    TKN_TEST_POS(token, TKN_MINUS, 0, 4);

    token = next_token(&lexer);
    TKN_TEST_POS(token, TKN_RPAREN, 0, 5);

    token = next_token(&lexer);
    TKN_TEST_POS(token, TKN_LBRACKET, 2, 0);

    token = next_token(&lexer);
    TKN_TEST_POS(token, TKN_SEMICOLON, 2, 1);

    token = next_token(&lexer);
    TKN_TEST_POS(token, TKN_COLON, 2, 2);

    token = next_token(&lexer);
    TKN_TEST_POS(token, TKN_COMMA, 2, 3);

    token = next_token(&lexer);
    TKN_TEST_POS(token, TKN_DOT, 2, 4);

    token = next_token(&lexer);
    TKN_TEST_POS(token, TKN_DIV, 2, 5);

    token = next_token(&lexer);
    TKN_TEST_POS(token, TKN_RBRACKET, 2, 6);

    token = next_token(&lexer);
    TKN_TEST_POS(token, TKN_EOF, 2, 7);

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

    // Test integer literals
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
    lexer.at = "0Z 0b3 09 1A\n999999999999999999999999";
    token = next_token(&lexer);
    assert(lexer.num_errors == 1);

    token = next_token(&lexer);
    assert(lexer.num_errors == 2);

    token = next_token(&lexer);
    assert(lexer.num_errors == 3);

    token = next_token(&lexer);
    assert(lexer.num_errors == 4);

    token = next_token(&lexer);
    assert(lexer.num_errors == 5);

    token = next_token(&lexer);
    assert(token.type == TKN_EOF);

    // Test floating point literals
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
    lexer.at = "1.33ea 1.33e100000000000";
    token = next_token(&lexer);
    assert(lexer.num_errors == 1);

    token = next_token(&lexer);
    assert(lexer.num_errors == 2);

    // Test character literals
    memset(&lexer, 0, sizeof(Lexer));
    lexer.at = "'a' '1' ' ' '\\0' '\\a' '\\b' '\\e' '\\f' '\\n' '\\r' '\\t' '\\v' '\\\\' '\\'' '\\\"' '\\?'";
    token = next_token(&lexer);
    assert(token.type == TKN_CHAR);
    assert(token.char_.value == 'a');

    token = next_token(&lexer);
    assert(token.type == TKN_CHAR);
    assert(token.char_.value == '1');

    token = next_token(&lexer);
    assert(token.type == TKN_CHAR);
    assert(token.char_.value == ' ');

    token = next_token(&lexer);
    assert(token.type == TKN_CHAR);
    assert(token.char_.value == '\0');

    token = next_token(&lexer);
    assert(token.type == TKN_CHAR);
    assert(token.char_.value == '\a');

    token = next_token(&lexer);
    assert(token.type == TKN_CHAR);
    assert(token.char_.value == '\b');

    token = next_token(&lexer);
    assert(token.type == TKN_CHAR);
    assert(token.char_.value == '\e');

    token = next_token(&lexer);
    assert(token.type == TKN_CHAR);
    assert(token.char_.value == '\f');    

    token = next_token(&lexer);
    assert(token.type == TKN_CHAR);
    assert(token.char_.value == '\n');

    token = next_token(&lexer);
    assert(token.type == TKN_CHAR);
    assert(token.char_.value == '\r');

    token = next_token(&lexer);
    assert(token.type == TKN_CHAR);
    assert(token.char_.value == '\t');

    token = next_token(&lexer);
    assert(token.type == TKN_CHAR);
    assert(token.char_.value == '\v');

    token = next_token(&lexer);
    assert(token.type == TKN_CHAR);
    assert(token.char_.value == '\\');

    token = next_token(&lexer);
    assert(token.type == TKN_CHAR);
    assert(token.char_.value == '\'');

    token = next_token(&lexer);
    assert(token.type == TKN_CHAR);
    assert(token.char_.value == '"');

    token = next_token(&lexer);
    assert(token.type == TKN_CHAR);
    assert(token.char_.value == '?');

    token = next_token(&lexer);
    assert(token.type == TKN_EOF);
    assert(lexer.num_errors == 0);

    memset(&lexer, 0, sizeof(Lexer));
    lexer.at = "'\\x12'  '\\x3'";
    token = next_token(&lexer);
    assert(token.type == TKN_CHAR);
    assert(token.char_.value == '\x12');

    token = next_token(&lexer);
    assert(token.type == TKN_CHAR);
    assert(token.char_.value == '\x3');

    memset(&lexer, 0, sizeof(Lexer));
    lexer.at = "'' 'a '\n' '\\z' '\\0'";
    token = next_token(&lexer);
    assert(lexer.num_errors == 1);

    token = next_token(&lexer);
    assert(lexer.num_errors == 2);

    token = next_token(&lexer);
    assert(lexer.num_errors == 3);

    token = next_token(&lexer);
    assert(lexer.num_errors == 4);

    token = next_token(&lexer);
    assert(token.type == TKN_CHAR);
    assert(token.char_.value == '\0');

    token = next_token(&lexer);
    assert(token.type == TKN_EOF);
}

int main(void)
{
    printf("Nibble!\n");
    test_lexer();
}
