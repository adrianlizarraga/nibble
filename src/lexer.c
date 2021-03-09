#include "lexer.h"
#include "array.h"
#include <assert.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>

#define LEXER_MAX_ERROR_LEN 128
#define LEXER_ARENA_BLOCK_SIZE 512

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

static ProgPos lexer_at_pos(Lexer* lexer)
{
    return (ProgPos)(lexer->at - lexer->str) + lexer->start;
}

static void lexer_on_error(Lexer* lexer, const char* format, ...)
{
    if (lexer->client.on_error) {
        char buf[LEXER_MAX_ERROR_LEN];
        va_list vargs;

        va_start(vargs, format);
        vsnprintf(buf, LEXER_MAX_ERROR_LEN, format, vargs);
        va_end(vargs);

        lexer->client.on_error(lexer->client.data, lexer_at_pos(lexer), buf);
    }
}

static void lexer_on_line(Lexer* lexer)
{
    if (lexer->client.on_line) {
        lexer->client.on_line(lexer->client.data, lexer_at_pos(lexer));
    }
}

static const char* lexer_on_str(Lexer* lexer, const char* str, size_t len)
{
    LexerClient* client = &lexer->client;
    OnLexStrFunc* on_str = client->on_str;

    return on_str ? on_str(client->data, lexer_at_pos(lexer), str, len) : NULL;
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
        } else {
            if (lexer->at[0] == '\n') {
                lexer_on_line(lexer);
            }

            lexer->at++;
        }
    }

    if (level > 0) {
        lexer_on_error(lexer, "Missing closing '*/' for c-style comment.");
        return;
    }

    return;
}

static void scan_int(Lexer* lexer)
{
    // TODO: Support length suffixes e.g., U, UL, ULL, L, LL
    assert((lexer->at[0] >= '0') && (lexer->at[0] <= '9'));

    Token* token = &lexer->token;

    token->kind = TKN_INT;
    token->tint.rep = TKN_INT_DEC;
    token->tint.value = 0;
    uint32_t base = 10;

    if (lexer->at[0] == '0') {
        lexer->at++;
        if ((lexer->at[0] == 'x') || (lexer->at[0] == 'X')) {
            lexer->at++;
            token->tint.rep = TKN_INT_HEX;
            base = 16;
        } else if ((lexer->at[0] == 'b') || (lexer->at[0] == 'B')) {
            lexer->at++;
            token->tint.rep = TKN_INT_BIN;
            base = 2;
        } else {
            token->tint.rep = TKN_INT_OCT;
            base = 8;
        }
    }

    unsigned int biased = char_to_biased_digit[(unsigned char)lexer->at[0]];

    if (biased == 0) {
        lexer_on_error(lexer, "Invalid integer literal character '%c' after base specifier", lexer->at[0]);
        skip_word_end(lexer);
        return;
    }

    const char* start = lexer->at;

    do {
        unsigned int digit = biased - 1;

        if (digit >= base) {
            lexer_on_error(lexer, "Integer literal digit (%c) is outside of base (%u) range", lexer->at[0], base);
            token->tint.value = 0;
            skip_word_end(lexer);
            return;
        }

        // Detect overflow if 10*val + digt > MAX
        if (token->tint.value > (UINT64_MAX - digit) / base) {
            lexer_on_error(lexer, "Integer literal %.*s is too large for its type", (size_t)(lexer->at - start), start);
            token->tint.value = 0;
            skip_word_end(lexer);
            return;
        }

        token->tint.value *= base;
        token->tint.value += digit;

        lexer->at++;
        biased = char_to_biased_digit[(unsigned char)lexer->at[0]];
    } while (biased != 0);

    return;
}

static void scan_float(Lexer* lexer)
{
    assert(((lexer->at[0] >= '0') && (lexer->at[0] <= '9')) || (lexer->at[0] == '.'));
    Token* token = &lexer->token;

    token->kind = TKN_FLOAT;
    token->tfloat.value = 0.0;

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
            lexer_on_error(lexer, "Unexpected character '%c' after floating point literal's exponent", lexer->at[0]);
            skip_word_end(lexer);
            return;
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
        lexer_on_error(lexer, "Floating point literal is too large");
        return;
    }

    token->tfloat.value = value;

    return;
}

static int scan_hex_escape(Lexer* lexer)
{
    // Scan the first of two hex digits.
    unsigned int biased = char_to_biased_digit[(unsigned char)lexer->at[0]];
    unsigned int digit = biased - 1;

    if (!biased || (digit >= 16)) {
        lexer_on_error(lexer, "Invalid hex character digit '0x%X'", lexer->at[0]);
        return -1;
    }

    int value = digit;
    lexer->at++;

    // Scan the second (optional) digit.
    biased = char_to_biased_digit[(unsigned char)lexer->at[0]];

    if (biased) {
        digit = biased - 1;

        if (digit >= 16) {
            lexer_on_error(lexer, "Invalid hex character digit '0x%X' is larger than 0x%X", lexer->at[0], 15);
            return -1;
        }

        value *= 16;
        value += digit;

        lexer->at++;
    }

    return value;
}

static void scan_string(Lexer* lexer)
{
    assert(lexer->at[0] == '"');
    Token* token = &lexer->token;
    token->kind = TKN_STR;
 
    lexer->at++;

    Allocator* arena = &lexer->allocator;
    AllocatorState state = allocator_get_state(arena);
    char* tmp = array_create(arena, char, 128); 

    while (lexer->at[0] && (lexer->at[0] != '"') && (lexer->at[0] != '\n')) {
        char c = lexer->at[0];

        if (c == '\\') {
            lexer->at++;

            c = lexer->at[0];
            if ((c == 'x') || (c == 'X')) {
                lexer->at++;

                int hex_val = scan_hex_escape(lexer);
                if (hex_val < 0) {
                    skip_word_end(lexer);
                    c = '?'; // TODO: What do other languages do?
                } else {
                    c = (char)hex_val;
                }
            } else { // One character escapes
                c = escaped_to_char[(unsigned char)lexer->at[0]];

                if (!c && (lexer->at[0] != '0')) {
                    lexer_on_error(lexer, "Invalid escaped character '\\%c'", lexer->at[0]);
                }

                lexer->at++;
            }
        } else {
            lexer->at++;
        }

        array_push(tmp, c);
    }

    if (lexer->at[0] == '"') {
        lexer->at++;
    } else if (lexer->at[0] == '\n') {
        lexer_on_error(lexer, "String literal cannot span multiple lines");
        lexer_on_line(lexer);
        lexer->at++;
        skip_char(lexer, '"');
    } else {
        lexer_on_error(lexer, "Encountered end-of-file while parsing string literal");
    }

    array_push(tmp, '\0');
    token->tstr.value = lexer_on_str(lexer, tmp, array_len(tmp) - 1);

    allocator_restore_state(state);
}

static void scan_char(Lexer* lexer)
{
    assert(lexer->at[0] == '\'');
    Token* token = &lexer->token;

    token->kind = TKN_INT;
    token->tint.rep = TKN_INT_CHAR;
    token->tint.value = 0;

    lexer->at++;

    // Check for empty character literal.
    if (lexer->at[0] == '\'') {
        lexer_on_error(lexer, "Character literal cannot be empty");
        lexer->at++;
        return;
    }

    if (lexer->at[0] == '\n') {
        lexer_on_error(lexer, "Character literal cannot contain a newline character");
        lexer_on_line(lexer);
        lexer->at++;
        skip_char(lexer, '\'');

        return;
    }

    // Scan escaped sequence
    if (lexer->at[0] == '\\') {
        lexer->at++;

        if ((lexer->at[0] == 'x') || (lexer->at[0] == 'X')) {
            lexer->at++;

            int hex_val = scan_hex_escape(lexer);

            token->tint.value = hex_val >= 0 ? hex_val : 0;
        } else { // One character escape chars
            int32_t val = escaped_to_char[(unsigned char)lexer->at[0]];

            if (!val && (lexer->at[0] != '0')) {
                lexer_on_error(lexer, "Invalid escaped character '\\%c'", lexer->at[0]);
            }

            token->tint.value = val;
            lexer->at++;
        }
    } else { // Regular non-escaped char
        token->tint.value = lexer->at[0];
        lexer->at++;
    }

    // Check for a closing quote
    if (lexer->at[0] != '\'') {
        lexer_on_error(lexer, "Missing closing character quote (found '%c' instead)", lexer->at[0]);
        skip_word_end(lexer);
        skip_char(lexer, '\'');
    } else {
        lexer->at++;
    }

    return;
}

void init_lexer(Lexer* lexer, const char* str, uint32_t start)
{
    memset(lexer, 0, sizeof(Lexer));
    lexer->str = str;
    lexer->at = str;
    lexer->start = start;
}

bool next_token(Lexer* lexer)
{
    Token* token = &lexer->token;
    memset(token, 0, sizeof(Token));

    bool repeat = false;

    do {
        repeat = false;
        token->start = lexer_at_pos(lexer);

        switch (lexer->at[0]) {
        case ' ': case '\t': case '\n': case '\r': case '\v': {
            while (is_whitespace(lexer->at[0])) {
                if (lexer->at[0] == '\n') {
                    lexer_on_line(lexer);
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
                skip_c_comment(lexer);
                repeat = true;
            } else {
                token->kind = TKN_DIV;
                lexer->at++;
            }
        } break;
        case '(': {
            token->kind = TKN_LPAREN;
            lexer->at++;
        } break;
        case ')': {
            token->kind = TKN_RPAREN;
            lexer->at++;
        } break;
        case '[': {
            token->kind = TKN_LBRACE;
            lexer->at++;
        } break;
        case ']': {
            token->kind = TKN_RBRACE;
            lexer->at++;
        } break;
        case '{': {
            token->kind = TKN_LBRACKET;
            lexer->at++;
        } break;
        case '}': {
            token->kind = TKN_RBRACKET;
            lexer->at++;
        } break;
        case ';': {
            token->kind = TKN_SEMICOLON;
            lexer->at++;
        } break;
        case ':': {
            token->kind = TKN_COLON;
            lexer->at++;
        } break;
        case ',': {
            token->kind = TKN_COMMA;
            lexer->at++;
        } break;
        case '+': {
            token->kind = TKN_PLUS;
            lexer->at++;
        } break;
        case '-': {
            token->kind = TKN_MINUS;
            lexer->at++;
        } break;
        case '.': {
            if (is_digit(lexer->at[1])) {
                scan_float(lexer);
            } else {
                token->kind = TKN_DOT;
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
                scan_float(lexer);
            } else {
                scan_int(lexer);
            }
        } break;
        case '\'': {
            scan_char(lexer);
        } break;
        case '"': {
            scan_string(lexer);
        } break;
        case '\0': {
            token->kind = TKN_EOF;
            lexer->at++;
        } break;
        default: {
            lexer_on_error(lexer, "Unexpected token character: %c", lexer->at[0]);
            lexer->at++;
            repeat = true;
        } break;
        }

        token->end = lexer_at_pos(lexer);
    } while (repeat);

    return token->kind != TKN_EOF;
}

bool is_token(Lexer* lexer, TokenKind kind)
{
    return (lexer->token.kind == kind);
}

bool match_token(Lexer* lexer, TokenKind kind)
{
    bool matches = (lexer->token.kind == kind);

    if (matches) {
        next_token(lexer);
    }

    return matches;
}
