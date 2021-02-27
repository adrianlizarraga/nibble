#include "lexer.h"
#include <assert.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>

#define INT_HEX_BASE 16U
#define INT_DEC_BASE 10U
#define INT_OCT_BASE 8U
#define INT_BIN_BASE 2U

// Converts a numeric character to an integer value. Values are biased by +1
// so that a result of 0 is known to be invalid.
static const unsigned int char_to_biased_digit[256] = {
    ['0'] = 1,  ['1'] = 2,  ['2'] = 3,  ['3'] = 4,  ['4'] = 5,  ['5'] = 6,  ['6'] = 7,  ['7'] = 8,
    ['8'] = 9,  ['9'] = 10, ['a'] = 11, ['b'] = 12, ['c'] = 13, ['d'] = 14, ['e'] = 15, ['f'] = 16,
    ['A'] = 11, ['B'] = 12, ['C'] = 13, ['D'] = 14, ['E'] = 15, ['F'] = 16,
};

static const char escaped_to_char[256] = {
    ['0'] = '\0', ['a'] = '\a', ['b'] = '\b',  ['f'] = '\f',  ['n'] = '\n', ['r'] = '\r',
    ['t'] = '\t', ['v'] = '\v', ['\\'] = '\\', ['\''] = '\'', ['"'] = '"',  ['?'] = '?',
};

static bool is_whitespace(char c)
{
    return (c == ' ') || (c == '\t') || (c == '\r') || (c == '\n') || (c == '\v');
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

static void skip_word_end(Lexer* lexer)
{
    while (lexer->at[0] && is_alphanum(lexer->at[0])) {
        lexer->at += 1;
    }
}

static void skip_char(Lexer* lexer, char c)
{
    if (lexer->at[0] == c) {
        lexer->at += 1;
    }
}

static void lexer_error(Lexer* lexer, const char* format, ...)
{
    char* buf = lexer->error;
    size_t len = LEXER_MAX_ERROR_LEN;
    int n = snprintf(buf, len, "%u: error: ", lexer->token.start);

    buf += n;
    len -= n;

    va_list vargs;
    va_start(vargs, format);
    vsnprintf(buf, len, format, vargs);
    va_end(vargs);
}

static void process_newline(Lexer* lexer)
{
    // TODO: Implement this!
}

static int skip_c_comment(Lexer* lexer)
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
        } else {
            if (lexer->at[0] == '\n') {
                process_newline(lexer);
            }

            lexer->at++;
        }
    }

    if (level > 0) {
        lexer_error(lexer, "Missing closing '*/' for c-style comment.");
        return 1;
    }

    return 0;
}

static int scan_int(Lexer* lexer)
{
    // TODO: Support length suffixes e.g., U, UL, ULL, L, LL
    assert((lexer->at[0] >= '0') && (lexer->at[0] <= '9'));

    Token* token = &lexer->token;

    token->type = TKN_INT;
    token->int_.base = INT_DEC_BASE;
    token->int_.value = 0;

    if (lexer->at[0] == '0') {
        lexer->at++;
        if ((lexer->at[0] == 'x') || (lexer->at[0] == 'X')) {
            lexer->at++;
            token->int_.base = INT_HEX_BASE;
        } else if ((lexer->at[0] == 'b') || (lexer->at[0] == 'B')) {
            lexer->at++;
            token->int_.base = INT_BIN_BASE;
        } else {
            token->int_.base = INT_OCT_BASE;
        }
    }

    unsigned int biased = char_to_biased_digit[(unsigned char)lexer->at[0]];

    if (biased == 0) {
        lexer_error(lexer, "Invalid integer literal character '%c' after base specifier", lexer->at[0]);
        skip_word_end(lexer);
        return 1;
    }

    const char* start = lexer->at;

    do {
        unsigned int digit = biased - 1;

        if (digit >= token->int_.base) {
            lexer_error(lexer, "Integer literal digit (%c) is outside of base (%u) range", lexer->at[0],
                        token->int_.base);
            token->int_.value = 0;
            skip_word_end(lexer);
            return 1;
        }

        // Detect overflow if 10*val + digt > MAX
        if (token->int_.value > (UINT64_MAX - digit) / token->int_.base) {
            lexer_error(lexer, "Integer literal %.*s is too large for its type", (size_t)(lexer->at - start), start);
            token->int_.value = 0;
            skip_word_end(lexer);
            return 1;
        }

        token->int_.value *= token->int_.base;
        token->int_.value += digit;

        lexer->at++;
        biased = char_to_biased_digit[(unsigned char)lexer->at[0]];
    } while (biased != 0);

    return 0;
}

static int scan_float(Lexer* lexer)
{
    assert(((lexer->at[0] >= '0') && (lexer->at[0] <= '9')) || (lexer->at[0] == '.'));
    Token* token = &lexer->token;

    token->type = TKN_FLOAT;
    token->float_.value = 0.0;

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
            return 1;
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
        return 1;
    }

    token->float_.value = value;

    return 0;
}

static int scan_char(Lexer* lexer)
{
    assert(lexer->at[0] == '\'');
    Token* token = &lexer->token;

    token->type = TKN_CHAR;
    token->char_.value = 0;

    lexer->at++;

    // Check for empty character literal.
    if (lexer->at[0] == '\'') {
        lexer_error(lexer, "Character literal cannot be empty");
        lexer->at++;
        return 1;
    }

    // Check for invalid characters (e.g., newline).
    if (is_escaped_space(lexer->at[0])) {
        lexer_error(lexer, "Invalid character literal with value 0x%X", lexer->at[0]);

        if (lexer->at[0] == '\n') {
            process_newline(lexer);
            lexer->at++;
        } else {
            skip_word_end(lexer);
        }

        skip_char(lexer, '\'');

        return 1;
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
                return 1;
            }

            token->char_.value = digit;
            lexer->at++;

            // Scan the second (optional) digit.
            biased = char_to_biased_digit[(unsigned char)lexer->at[0]];

            if (biased) {
                digit = biased - 1;

                if (digit >= INT_HEX_BASE) {
                    lexer_error(lexer, "Invalid hex character digit '0x%X' is larger than 0x%X", lexer->at[0],
                                INT_HEX_BASE - 1);
                    skip_word_end(lexer);
                    skip_char(lexer, '\'');
                    return 1;
                }

                token->char_.value *= INT_HEX_BASE;
                token->char_.value += digit;

                lexer->at++;
            }
        } else { // One character escape chars
            int32_t val = escaped_to_char[(unsigned char)lexer->at[0]];

            if (!val && (lexer->at[0] != '0')) {
                lexer_error(lexer, "Invalid escaped character '\\%c'", lexer->at[0]);
                skip_word_end(lexer);
                skip_char(lexer, '\'');
                return 1;
            }

            token->char_.value = val;
            lexer->at++;
        }
    }
    // Regular non-escaped char
    else {
        token->char_.value = lexer->at[0];
        lexer->at++;
    }

    // Check for a closing quote
    if (lexer->at[0] != '\'') {
        lexer_error(lexer, "Missing closing character quote (found '%c' instead)", lexer->at[0]);
        skip_word_end(lexer);
        skip_char(lexer, '\'');
        return 1;
    }

    lexer->at++;

    return 0;
}

void init_lexer(Lexer* lexer, const char* str, uint32_t start_pos)
{
    memset(lexer, 0, sizeof(Lexer));
    lexer->str = str;
    lexer->at = str;
    lexer->start_pos = start_pos;
}

int next_token(Lexer* lexer)
{
    Token* token = &lexer->token;
    memset(token, 0, sizeof(Token));

    int errors = 0;
    bool repeat = false;

    do {
        repeat = false;
        token->start = (lexer->at - lexer->str) + lexer->start_pos;

        switch (lexer->at[0]) {
        case ' ': case '\t': case '\n': case '\r': case '\v': {
            while (is_whitespace(lexer->at[0])) {
                if (lexer->at[0] == '\n') {
                    process_newline(lexer);
                }

                lexer->at++;
            }

            repeat = true;
        } break;
        case '/': {
            if (lexer->at[1] == '/') {
                while (lexer->at[0] && (lexer->at[0] != '\n') && (lexer->at[0] != '\r')) {
                    lexer->at++;
                }

                repeat = true;
            } else if (lexer->at[1] == '*') {
                errors += skip_c_comment(lexer);
                repeat = true;
            } else {
                token->type = TKN_DIV;
                lexer->at++;
            }
        } break;
        case '(': {
            token->type = TKN_LPAREN;
            lexer->at++;
        } break;
        case ')': {
            token->type = TKN_RPAREN;
            lexer->at++;
        } break;
        case '[': {
            token->type = TKN_LBRACE;
            lexer->at++;
        } break;
        case ']': {
            token->type = TKN_RBRACE;
            lexer->at++;
        } break;
        case '{': {
            token->type = TKN_LBRACKET;
            lexer->at++;
        } break;
        case '}': {
            token->type = TKN_RBRACKET;
            lexer->at++;
        } break;
        case ';': {
            token->type = TKN_SEMICOLON;
            lexer->at++;
        } break;
        case ':': {
            token->type = TKN_COLON;
            lexer->at++;
        } break;
        case ',': {
            token->type = TKN_COMMA;
            lexer->at++;
        } break;
        case '+': {
            token->type = TKN_PLUS;
            lexer->at++;
        } break;
        case '-': {
            token->type = TKN_MINUS;
            lexer->at++;
        } break;
        case '.': {
            if (is_digit(lexer->at[1])) {
                errors += scan_float(lexer);
            } else {
                token->type = TKN_DOT;
                lexer->at++;
            }
        } break;
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9': {
            // First, determine if this is an integer or a floating point value.
            const char* p = lexer->at;

            while (is_digit(*p)) {
                p++;
            }

            if ((*p == '.') || (*p == 'e') || (*p == 'E')) {
                errors += scan_float(lexer);
            } else {
                errors += scan_int(lexer);
            }
        } break;
        case '\'': {
            errors += scan_char(lexer);
        } break;
        case '\0': {
            token->type = TKN_EOF;
            lexer->at++;
        } break;
        default: {
            lexer_error(lexer, "[INTERNAL ERROR] Unexpected token character: %c", lexer->at[0]);
            lexer->at++;
            repeat = true;
        } break;
        }

        token->end = (lexer->at - lexer->str) + lexer->start_pos;
    } while (repeat);

    return errors;
}

bool is_token(Lexer* lexer, TokenType type)
{
    return (lexer->token.type == type);
}

bool match_token(Lexer* lexer, TokenType type)
{
    bool matches = (lexer->token.type == type);

    if (matches) {
        next_token(lexer);
    }

    return matches;
}
