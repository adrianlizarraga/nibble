#include "lexer.h"
#include <assert.h>
#include <math.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>
#include <limits.h>


static bool is_whitespace(char c)
{
    return (c == ' ') || (c == '\t') || (c == '\n') || (c == '\r') || (c == '\v');
}

static bool is_escaped_space(char c)
{
    return (c == '\t') || (c == '\n') || (c == '\r') || (c == '\v');
}

static bool is_digit(char c)
{
    return (c >= '0') && (c <= '9');
}

static bool is_letter(char c)
{
    return ((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z'));
}

static bool is_alphanum(char c)
{
    return is_digit(c) || is_letter(c) || (c == '_');
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

static void skip_word_end(Lexer* lexer)
{
    while (lexer->at[0] && is_alphanum(lexer->at[0])) {
	lexer->at++;
    }
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

static void skip_char(Lexer* lexer, char c)
{
    if (lexer->at[0] == c) {
	lexer->at++;
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

static TokenInt scan_uint(Lexer* lexer)
{
    // TODO: Support length suffixes e.g., U, UL, ULL, L, LL

    TokenInt token = {.value = 0, .base = INT_DEC_BASE};

    if (lexer->at[0] == '0') {
	lexer->at++;
	if ((lexer->at[0] == 'x') || (lexer->at[0] == 'X')) {
	    lexer->at++;
	    token.base = INT_HEX_BASE;
	}
	else if ((lexer->at[0] == 'b') || (lexer->at[0] == 'B')) {
	    lexer->at++;
	    token.base = INT_BIN_BASE;
	}
	else {
	    token.base = INT_OCT_BASE;
	}
    }

    unsigned int biased = char_to_biased_digit[(unsigned char)lexer->at[0]];

    if (biased == 0) {
	lexer_error(lexer, "Invalid integer literal character '%c' after base specifier",
	            *lexer->at);
	skip_word_end(lexer);
	return token;
    }


    const char* start = lexer->at;

    do {
	unsigned int digit = biased - 1;

	if (digit >= token.base) {
	    lexer_error(lexer, "Integer literal digit (%c) is outside of base (%u) range", *lexer->at, token.base);
	    token.value = 0;
	    skip_word_end(lexer);
	    break;
	}

	// Detect overflow if 10*val + digt > MAX
	if (token.value > (UINT64_MAX - digit) / token.base) {
	    lexer_error(lexer, "Integer literal %.*s is too large for its type", (size_t)(lexer->at - start), start);
	    token.value = 0;
	    skip_word_end(lexer);
	    break;
	}

	token.value *= token.base;
	token.value += digit;

	lexer->at++;
	biased = char_to_biased_digit[(unsigned char)lexer->at[0]];
    } while (biased != 0);

    return token;
}

static TokenFloat scan_float(Lexer* lexer)
{
    assert(is_digit(*lexer->at) || (*lexer->at == '.'));
    TokenFloat token = {0};
    const char* start = lexer->at;

    // \d*\.?\d*(e[+-]?\d*)?

    while (is_digit(lexer->at[0])) {
	lexer->at++;
    }

    if (lexer->at[0] == '.') {
	lexer->at++;
    }

    while (is_digit(lexer->at[0])) {
	lexer->at++;
    }

    if ((lexer->at[0] == 'e') || (lexer->at[0] == 'E')) {
	lexer->at++;

	if ((lexer->at[0] == '-') || (lexer->at[0] == '+')) {
	    lexer->at++;
	}

	if (!is_digit(lexer->at[0])) {
	    lexer_error(lexer, "Unexpected character '%c' after floating point literal's exponent", lexer->at[0]);
	    skip_word_end(lexer);
	    return token;
	}

	while (is_digit(lexer->at[0])) {
	    lexer->at++;
	}
    }

    // If we reached this point, use libc's strtod to get the floating point value.
    // TODO: Make a custom atof implementation (not trivial!).
    char* end = NULL;
    double value = strtod(start, &end);

    assert(end == lexer->at);

    if (value == HUGE_VAL) {
	lexer_error(lexer, "Floating point literal is too large");
	return token;
    }

    token.value = value;
    return token;
}

static const char escaped_to_char[256] = {
    ['0'] = '\0',
    ['a'] = '\a',
    ['b'] = '\b',
    ['e'] = '\e',
    ['f'] = '\f',
    ['n'] = '\n',
    ['r'] = '\r',
    ['t'] = '\t',
    ['v'] = '\v',
    ['\\'] = '\\',
    ['\''] = '\'',
    ['"'] = '"',
    ['?'] = '?',
};

static TokenChar scan_char(Lexer* lexer)
{
    assert(lexer->at[0] == '\'');
    TokenChar token = {0};

    lexer->at++;

    // Check for empty character literal.
    if (lexer->at[0] == '\'') {
	lexer_error(lexer, "Character literal cannot be empty");
	lexer->at++;
	return token;
    }

    // Check for invalid characters (e.g., newline).
    if (is_escaped_space(lexer->at[0])) {
	lexer_error(lexer, "Invalid character literal with value 0x%X", lexer->at[0]);

	if (lexer->at[0] == '\n') {
	    process_newline(lexer);
	}
	else {
	    skip_word_end(lexer);
	}

	skip_char(lexer, '\'');

	return token;
    }

    // Scan escaped sequence
    if (lexer->at[0] == '\\') {
	lexer->at++;

	if ((lexer->at[0] == 'x') || (lexer->at[0] == 'X')) {
	    lexer->at++;

	    // Scan the first of two hex digits.
	    unsigned int biased = char_to_biased_digit[(unsigned char)lexer->at[0]];
	    unsigned int digit = biased - 1;

	    if (!biased || (digit >= INT_HEX_BASE)) {
		lexer_error(lexer, "Invalid hex character digit '0x%X'", lexer->at[0]);
		skip_word_end(lexer);
		skip_char(lexer, '\'');
		return token;
	    }

	    token.value = digit;
	    lexer->at++;

	    // Scan the second (optional) digit.
	    biased = char_to_biased_digit[(unsigned char)lexer->at[0]];

	    if (biased) {
	        digit = biased - 1;

		if (digit >= INT_HEX_BASE) {
		    lexer_error(lexer, "Invalid hex character digit '0x%X' is larger than 0x%X",
				lexer->at[0], INT_HEX_BASE - 1);
		    skip_word_end(lexer);
		    skip_char(lexer, '\'');
		    return token;
		}

		token.value *= INT_HEX_BASE;
		token.value += digit;

		lexer->at++;
	    }
	}
	else { // One character escape chars
	    int32_t val = escaped_to_char[(unsigned char)lexer->at[0]];

	    if (!val && (lexer->at[0] != '0')) {
		lexer_error(lexer, "Invalid escaped character '\\%c'", lexer->at[0]);
		skip_word_end(lexer);
		skip_char(lexer, '\'');
		return token;
	    }

	    token.value = val;
	    lexer->at++;
	}
    }
    // Regular non-escaped char
    else {
	token.value = lexer->at[0];
	lexer->at++;
    }

    // Check for a closing quote
    if (lexer->at[0] != '\'') {
	lexer_error(lexer, "Missing closing character quote (found '%c' instead)", lexer->at[0]);
	skip_word_end(lexer);
	skip_char(lexer, '\'');
    }
    else {
	lexer->at++;
    }

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
	case '[': {
	    token.type = TKN_LBRACE;
	    lexer->at++;
	} break;
	case ']': {
	    token.type = TKN_RBRACE;
	    lexer->at++;
	} break;
	case '{': {
	    token.type = TKN_LBRACKET;
	    lexer->at++;
	} break;
	case '}': {
	    token.type = TKN_RBRACKET;
	    lexer->at++;
	} break;
	case ';': {
	    token.type = TKN_SEMICOLON;
	    lexer->at++;
	} break;
	case ':': {
	    token.type = TKN_COLON;
	    lexer->at++;
	} break;
	case ',': {
	    token.type = TKN_COMMA;
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
	case '.': {
	    if (is_digit(lexer->at[1])) {
		token.type = TKN_FLOAT;
		token.float_ = scan_float(lexer);
	    }
	    else {
		token.type = TKN_DOT;
	    }

	    lexer->at++;
	} break;
	case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9': {
	    // Need to determine if this is an integer or a floating point value.
	    const char* at = lexer->at;
	    while (is_digit(*at)) {
	        at++;
	    }

	    if ((*at == '.') || (*at == 'e') || (*at == 'E')) {
	        token.type = TKN_FLOAT;
		token.float_ = scan_float(lexer);
	    }
	    else {
		token.type = TKN_INT;
		token.int_ = scan_uint(lexer);
	    }
	} break;
	case '\'': {
	    token.type = TKN_CHAR;
	    token.char_ = scan_char(lexer);
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

