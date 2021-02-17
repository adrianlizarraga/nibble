#include <stdio.h>
#include <string.h>

#include "lexer.c"
void test_lexer()
{
    Lexer lexer = {};
    lexer.at = "(++--)  \n  //++--\n()";

    for (int i = 0; i < 8; ++i) {
	Token token = next_token(&lexer);

	printf("line: %u, col: %u, type: %d\n", token.line, token.column, token.type);
    }

    Token token = next_token(&lexer);
    assert(token.type == TKN_EOF);
    printf("EOF\n");

    memset(&lexer, sizeof(Lexer), 0);
    lexer.at = "/**** 1 /* 2 */ ***/+-";
    token = next_token(&lexer);
    assert(token.type == TKN_PLUS);
    token = next_token(&lexer);
    assert(token.type == TKN_MINUS);
    token = next_token(&lexer);
    assert(token.type == TKN_EOF);
}

int main(void)
{
    printf("Nibble!\n");
    test_lexer();
}
