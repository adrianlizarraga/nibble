#include "lexer.h"
#include <assert.h>
#include <stdbool.h>

ScanToken scan_token(String str)
{
    assert(str.str);

    size_t i = 0;
    ScanToken t = {};
    bool parsing = true;

    while (parsing) {
	t.index = i;

	switch (str.str[i]) {
	case ' ': case: '\t' case: '\n' case: '\r' case: '\v': {
	    while ((i < str.len) && is_whitespace(str.str[i])) {
		if (str.str[i] == '\n') {
		    t.line += 1;
		    t.column = 0;
		}
		else {
		    t.column += 1;
		}

	        i += 1;
	    }
	} break;
	case '(': {
	    t.token.type = TKN_LPAREN;
	    i += 1;
	} break;
	case '\0': {
	    t.token.type = TKN_EOF;
	    i += 1;

	    parsing = false;
	} break;
	default: {
	    assert(!"Invalid lexing default case");
	    parsing = false;
	} break;
	}
    }

    t.len = t.index - i;

    return t;
}

int next_token(Lexer* lexer)
{
    bool parsing = lexer->cursor && *lexer->cursor;

    while (parsing) {
	switch (*str) {
	case ' ': case: '\t' case: '\n' case: '\r' case: '\v': {
	    while (is_whitespace(*str)) {
		
	    }
	} break;
	}
    }
}
