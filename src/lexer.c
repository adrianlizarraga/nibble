#include "lexer.h"
#include <stdbool.h>

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
