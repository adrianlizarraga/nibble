#include "lexer.h"
#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>


static bool is_whitespace(char c)
{
    return (c == ' ') || (c == '\t') || (c == '\n') || (c == '\r') || (c == '\v');
}

static void lexer_error(Lexer* lexer, const char* format, ...)
{
    va_list vargs;

    va_start(vargs, format);
    vsnprintf(lexer->errors[lexer->num_errors++], LEXER_MAX_ERROR_LEN, format, vargs);
    va_end(vargs);
}

static void process_newline(Lexer* lexer)
{
    assert(lexer->at[0] == '\n');
    lexer->line += 1;
    lexer->column = 0;
    lexer->at += 1;
    lexer->token_start = lexer->at;
}

static void skip_c_comment(Lexer* lexer)
{
    assert(lexer->at[0] == '/');
    assert(lexer->at[1] == '*');
    lexer->at += 2;

    int level = 1;
    while (lexer->at[0] && (level > 0)) {

	// Nested c-style comment
	if ((lexer->at[0] == '/') && (lexer->at[1] == '*')) {
	    level += 1;
	    lexer->at += 2;
	}
	// End of one level of c-style comments
	else if ((lexer->at[0] == '*') && (lexer->at[1] == '/')) {
	    level -= 1;
	    lexer->at += 2;
	}
	else if (lexer->at[0] == '\n') {
	    process_newline(lexer);
	}
	else {
	    lexer->at++;
	}
    }

    if (level > 0) {
	lexer_error(lexer, "[ERROR] Missing closing '*/' for c-style comment.");
    }
}

Token next_token(Lexer* lexer)
{
    assert(lexer && lexer->at);

    Token token = {};
    bool repeat = false;

    do {
	repeat = false;
        lexer->token_start = lexer->at;

	token.line = lexer->line;
	token.column = lexer->column;

	switch (*lexer->at) {
	case ' ': case '\t': case '\n': case '\r': case '\v': {
	    while (is_whitespace(*lexer->at)) {
		if (*lexer->at == '\n') {
		    process_newline(lexer);
		}
		else {
		    lexer->at++;
		}
	    }

	    repeat = true;
	} break;
	case '/': {
	    if (lexer->at[1] == '/') {
		while (lexer->at[0] && (lexer->at[0] != '\n') && (lexer->at[0] != '\r')) {
		    lexer->at++;
		}

	        repeat = true;
	    }
	    else if (lexer->at[1] == '*') {
		skip_c_comment(lexer);

	        repeat = true;
	    }
	    else {
		token.type = TKN_DIV;
		lexer->at++;
	    }
	} break;
	case '(': {
	    token.type = TKN_LPAREN;
	    lexer->at++;
	} break;
	case ')': {
	    token.type = TKN_RPAREN;
	    lexer->at++;
	} break;
	case '+': {
	    token.type = TKN_PLUS;
	    lexer->at++;
	} break;
	case '-': {
	    token.type = TKN_MINUS;
	    lexer->at++;
	} break;
	case '\0': {
	    token.type = TKN_EOF;
	    lexer->at++;
	} break;
	default: {
	    lexer_error(lexer, "[INTERNAL ERROR] Unexpected token character: %c", *lexer->at);
	    lexer->at++;
	    repeat = true;
	} break;
	}

	lexer->column += lexer->at - lexer->token_start;
    } while (repeat);

    return token;
}

