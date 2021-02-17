#include "lexer.h"
#include <assert.h>
#include <stdbool.h>

static bool is_whitespace(char c)
{
    return (c == ' ') || (c == '\t') || (c == '\n') || (c == '\r') || (c == '\v');
}

void skip_c_comment(Lexer* lexer)
{
    if ((lexer->at[0] == '/') && (lexer->at[1] == '*')) {
	lexer->at += 2;

	while (lexer->at[0] && !((lexer->at[0] == '*') && (lexer->at[1] == '/'))) {

	    // Nested c-style comment
	    if ((lexer->at[0] == '/') && (lexer->at[1] == '*')) {
	        skip_c_comment(lexer);
	    }
	    else {
		lexer->at++;
	    }
	}

	if (lexer->at[0]) {
	    lexer->at += 2;
	}
    }
}

Token next_token(Lexer* lexer)
{
    assert(lexer && lexer->at);

    Token token = {};
    bool parsing = true;

    while (parsing) {
	const char* start = lexer->at;

	token.line = lexer->line;
	token.column = lexer->column;

	switch (*lexer->at) {
	case ' ': case '\t': case '\n': case '\r': case '\v': {
	    while (is_whitespace(*lexer->at)) {
		if (*lexer->at == '\n') {
		    lexer->line += 1;
		    lexer->column = 0;
		    lexer->at += 1;

		    start = lexer->at;
		}
		else {
		    lexer->at++;
		}
	    }

	    parsing = true;
	} break;
	case '/': {
	    if (lexer->at[1] == '/') {
		while (lexer->at[0] && (lexer->at[0] != '\n') && (lexer->at[0] != '\r')) {
		    lexer->at++;
		}

		parsing = true;
	    }
	    else if (lexer->at[1] == '*') {
		skip_c_comment(lexer);

		parsing = true;
	    }
	    else {
		token.type = TKN_DIV;
		lexer->at++;

		parsing = false;
	    }
	} break;
	case '(': {
	    token.type = TKN_LPAREN;
	    lexer->at++;

	    parsing = false;
	} break;
	case ')': {
	    token.type = TKN_RPAREN;
	    lexer->at++;

	    parsing = false;
	} break;
	case '+': {
	    token.type = TKN_PLUS;
	    lexer->at++;

	    parsing = false;
	} break;
	case '-': {
	    token.type = TKN_MINUS;
	    lexer->at++;

	    parsing = false;
	} break;
	case '\0': {
	    token.type = TKN_EOF;
	    lexer->at++;

	    parsing = false;
	} break;
	default: {
	    fprintf(stderr, "Unexpected first char %c\n", *lexer->at);
	    assert(!"Invalid lexing default case");
	    parsing = false;
	} break;
	}

	lexer->column += lexer->at - start;
    }

    return token;
}

