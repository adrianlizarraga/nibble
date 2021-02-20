#include "lexer.h"
#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <limits.h>


static bool is_whitespace(char c)
{
    return (c == ' ') || (c == '\t') || (c == '\n') || (c == '\r') || (c == '\v');
}

static bool is_number(char c)
{
    return (c >= '0') && (c <= '9');
}

static void lexer_error(Lexer* lexer, const char* format, ...)
{
    if (lexer->num_errors < LEXER_MAX_NUM_ERRORS) {
        char* buf = lexer->errors[lexer->num_errors];
	size_t len = LEXER_MAX_ERROR_LEN;
	int n = snprintf(buf, len, "%s:%u:%u: error: ", "<filename>", lexer->line + 1, lexer->column + 1);

	buf += n;
	len -= n;

	va_list vargs;
	va_start(vargs, format);
	vsnprintf(buf, len, format, vargs);
	va_end(vargs);

	lexer->num_errors++;
    }
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
	lexer_error(lexer, "Missing closing '*/' for c-style comment.");
    }
}

// Converts a numeric character to an integer value. Values are biased by +1
// so that a result of 0 is known to be invalid.
static const unsigned int char_to_biased_digit[256] = {
    ['0'] = 1,
    ['1'] = 2,
    ['2'] = 3,
    ['3'] = 4,
    ['4'] = 5,
    ['5'] = 6,
    ['6'] = 7,
    ['7'] = 8,
    ['8'] = 9,
    ['9'] = 10,
    ['a'] = 11,
    ['b'] = 12,
    ['c'] = 13,
    ['d'] = 14,
    ['e'] = 15,
    ['f'] = 16,
    ['A'] = 11,
    ['B'] = 12,
    ['C'] = 13,
    ['D'] = 14,
    ['E'] = 15,
    ['F'] = 16,
};

TokenInt scan_uint(Lexer* lexer)
{
    assert(is_number(lexer->at[0]));
    // TODO: Support length suffixes e.g., U, UL, ULL, L, LL

    TokenInt token = {.value = 0, .base = 10};

    if (lexer->at[0] == '0') {
	lexer->at++;
	if ((lexer->at[0] == 'x') || (lexer->at[0] == 'X')) {
	    lexer->at++;
	    token.base = 16;
	}
	else if ((lexer->at[0] == 'b') || (lexer->at[0] == 'B')) {
	    lexer->at++;
	    token.base = 2;
	}
	else {
	    token.base = 8;
	}
    }

    unsigned int biased = char_to_biased_digit[(int)lexer->at[0]];

    if (biased == 0) {
	lexer_error(lexer, "Invalid integer literal character '%c' after base specifier",
	            *lexer->at);
	return token;
    }


    do {
	unsigned int digit = biased - 1;

	if (digit >= token.base) {
	    lexer_error(lexer, "Integer literal digit (%c) is outside of base (%u) range", *lexer->at, token.base);
	    token.value = 0;
	    while (char_to_biased_digit[(int)lexer->at[0]]) {
		lexer->at++;
	    }
	    break;
	}

	// Detect overflow if 10*val + digt > MAX
	if (token.value > (UINT64_MAX - digit) / token.base) {
	    lexer_error(lexer, "Integer literal is too large for its type");
	    token.value = 0;
	    while (char_to_biased_digit[(int)lexer->at[0]]) {
		lexer->at++;
	    }
	    break;
	}

	token.value *= token.base;
	token.value += digit;

	lexer->at++;
	biased = char_to_biased_digit[(int)lexer->at[0]];
    } while (biased != 0);

    return token;
}

Token next_token(Lexer* lexer)
{
    assert(lexer && lexer->at);

    Token token = {0};
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
	case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9': {
	    token.type = TKN_INT;
	    token.int_ = scan_uint(lexer);
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

