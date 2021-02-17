#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lexer.c"

void test_token(Lexer* lexer, TokenType type, uint32_t line, uint32_t column)
{
    Token token = next_token(lexer);

    if ((token.type != type) || (token.line != line) || (token.column != column)) {
	fprintf(stderr, "[ERROR] Bad token (line: %u, col: %u, type: %d). Expected (line: %u, col: %u, type: %d)\n",
		token.line, token.column, token.type,
	        line, column, type);
	exit(1);
    }
}

void test_lexer()
{
    Lexer lexer = {};

    // Test basic 1 character tokens, newlines, and c++ comments.
    lexer.at = "(++--)  \n  //++--\n()";

    test_token(&lexer, TKN_LPAREN, 0, 0);
    test_token(&lexer, TKN_PLUS, 0, 1);
    test_token(&lexer, TKN_PLUS, 0, 2);
    test_token(&lexer, TKN_MINUS, 0, 3);
    test_token(&lexer, TKN_MINUS, 0, 4);
    test_token(&lexer, TKN_RPAREN, 0, 5);
    test_token(&lexer, TKN_LPAREN, 2, 0);
    test_token(&lexer, TKN_RPAREN, 2, 1);
    test_token(&lexer, TKN_EOF, 2, 2);

    // Test nested c-style comments
    memset(&lexer, 0, sizeof(Lexer));
    lexer.at = "/**** 1 /* 2 */ \n***/+-";

    test_token(&lexer, TKN_PLUS, 1, 4);
    test_token(&lexer, TKN_MINUS, 1, 5);
    test_token(&lexer, TKN_EOF, 1, 6);
}

int main(void)
{
    printf("Nibble!\n");
    test_lexer();
}
