#include "lexer.h"
#include "array.h"
#include "cstring.h"

#include <assert.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>

#define LEXER_ARENA_BLOCK_SIZE 256

static void skip_word_end(Lexer* lexer)
{
    while (lexer->at[0] && is_alphanum(lexer->at[0]))
        lexer->at += 1;
}

static void skip_char(Lexer* lexer, char c)
{
    if (lexer->at[0] == c)
        lexer->at += 1;
}

static ProgPos lexer_at_pos(Lexer* lexer)
{
    return (ProgPos)(lexer->at - lexer->str) + lexer->start;
}

static void lexer_on_error(Lexer* lexer, const char* format, ...)
{
    if (lexer->errors)
    {
        char buf[MAX_ERROR_LEN];
        size_t size = 0;
        va_list vargs;

        va_start(vargs, format);
        size = vsnprintf(buf, MAX_ERROR_LEN, format, vargs) + 1;
        va_end(vargs);

        size = size > sizeof(buf) ? sizeof(buf) : size;

        add_byte_stream_chunk(lexer->errors, buf, size);
    }
}

static void lexer_on_line(Lexer* lexer)
{
    (void)lexer;
}

static void skip_c_comment(Lexer* lexer)
{
    assert(lexer->at[0] == '/');
    assert(lexer->at[1] == '*');
    lexer->at += 2;

    int level = 1;

    while (lexer->at[0] && (level > 0))
    {

        // Nested c-style comment
        if ((lexer->at[0] == '/') && (lexer->at[1] == '*'))
        {
            level += 1;
            lexer->at += 2;
        }
        // End of one level of c-style comments
        else if ((lexer->at[0] == '*') && (lexer->at[1] == '/'))
        {
            level -= 1;
            lexer->at += 2;
        }
        else
        {
            if (lexer->at[0] == '\n')
                lexer_on_line(lexer);

            lexer->at++;
        }
    }

    if (level > 0)
    {
        lexer_on_error(lexer, "Missing closing '*/' for c-style comment.");
        return;
    }

    return;
}

static TokenInt scan_int(Lexer* lexer)
{
    // TODO: Support length suffixes e.g., U, UL, ULL, L, LL
    assert((lexer->at[0] >= '0') && (lexer->at[0] <= '9'));

    TokenInt tint = {.rep = TKN_INT_DEC, .value = 0};
    uint32_t base = 10;
    const char* start = lexer->at;

    if (lexer->at[0] == '0')
    {
        lexer->at++;
        if ((lexer->at[0] == 'x') || (lexer->at[0] == 'X'))
        {
            lexer->at++;
            tint.rep = TKN_INT_HEX;
            base = 16;
        }
        else if ((lexer->at[0] == 'b') || (lexer->at[0] == 'B'))
        {
            lexer->at++;
            tint.rep = TKN_INT_BIN;
            base = 2;
        }
        else if (is_dec_digit(lexer->at[0]))
        {
            tint.rep = TKN_INT_OCT;
            base = 8;
        }
    }

    unsigned int biased = biased_digit(lexer->at[0]);

    if ((biased == 0) && (base != 10))
    {
        lexer_on_error(lexer, "Invalid integer literal character '%c' after base specifier", lexer->at[0]);
        skip_word_end(lexer);
        return tint;
    }

    while (biased != 0)
    {
        unsigned int digit = biased - 1;

        if (digit >= base)
        {
            lexer_on_error(lexer, "Integer literal digit (%c) is outside of base (%u) range", lexer->at[0], base);
            tint.value = 0;
            skip_word_end(lexer);
            return tint;
        }

        // Detect overflow if 10*val + digt > MAX
        if (tint.value > (UINT64_MAX - digit) / base)
        {
            lexer_on_error(lexer, "Integer literal %.*s is too large for its type", (size_t)(lexer->at - start), start);
            tint.value = 0;
            skip_word_end(lexer);
            return tint;
        }

        tint.value *= base;
        tint.value += digit;

        lexer->at++;
        biased = biased_digit(lexer->at[0]);
    }

    if (is_alphanum(lexer->at[0]))
    {
        lexer_on_error(lexer, "Invalid integer literal character '%c'", lexer->at[0]);
        skip_word_end(lexer);
    }

    return tint;
}

static TokenFloat scan_float(Lexer* lexer)
{
    assert(((lexer->at[0] >= '0') && (lexer->at[0] <= '9')) || (lexer->at[0] == '.'));

    TokenFloat tfloat = {.value = 0.0};
    const char* start = lexer->at;

    // \d*\.?\d*(e[+-]?\d*)?
    while (is_dec_digit(lexer->at[0]))
        lexer->at++;

    if (lexer->at[0] == '.')
        lexer->at++;

    while (is_dec_digit(lexer->at[0]))
        lexer->at++;

    if ((lexer->at[0] == 'e') || (lexer->at[0] == 'E'))
    {
        lexer->at++;

        if ((lexer->at[0] == '-') || (lexer->at[0] == '+'))
            lexer->at++;

        if (!is_dec_digit(lexer->at[0]))
        {
            lexer_on_error(lexer, "Unexpected character '%c' after floating point literal's exponent", lexer->at[0]);
            skip_word_end(lexer);
            return tfloat;
        }

        while (is_dec_digit(lexer->at[0]))
            lexer->at++;
    }

    // If we reached this point, use libc's strtod to get the floating point value.
    // TODO: Make a custom atof implementation (not trivial!).
    char* end = NULL;
    double value = strtod(start, &end);

    assert(end == lexer->at);

    if (value == HUGE_VAL)
    {
        lexer_on_error(lexer, "Floating point literal is too large");
        return tfloat;
    }

    tfloat.value = value;

    return tfloat;
}

static int scan_hex_escape(Lexer* lexer)
{
    // Scan the first of two hex digits.
    unsigned int biased = biased_digit(lexer->at[0]);
    unsigned int digit = biased - 1;

    if (!biased || (digit >= 16))
    {
        lexer_on_error(lexer, "Invalid hex character digit '%c'", lexer->at[0]);
        return -1;
    }

    int value = digit;

    lexer->at++;

    // Scan the second (optional) digit.
    // TODO: Should the second digit be required? Maybe it should. If not, then should at least have
    // whitespace separating the next character.
    biased = biased_digit(lexer->at[0]);

    if (biased)
    {
        digit = biased - 1;

        if (digit >= 16)
        {
            lexer_on_error(lexer, "Invalid hex character digit '0x%X' is larger than 0x%X", lexer->at[0], 15);
            return -1;
        }

        value *= 16;
        value += digit;

        lexer->at++;
    }

    return value;
}

static TokenStr scan_string(Lexer* lexer)
{
    assert(lexer->at[0] == '"');
    TokenStr tstr = {0};

    lexer->at++;

    Allocator* arena = &lexer->allocator;
    char* tmp = array_create(arena, char, 128);

    while (lexer->at[0] && (lexer->at[0] != '"') && (lexer->at[0] != '\n'))
    {
        char c = lexer->at[0];

        if (c == '\\')
        {
            lexer->at++;

            c = lexer->at[0];
            if ((c == 'x') || (c == 'X'))
            {
                lexer->at++;

                int hex_val = scan_hex_escape(lexer);
                if (hex_val < 0)
                {
                    skip_word_end(lexer);
                    c = '?'; // TODO: What do other languages do?
                }
                else
                {
                    c = (char)hex_val;
                }
            }
            else
            { // One character escapes
                c = unescape_char(lexer->at[0]);

                if (!c && (lexer->at[0] != '0'))
                    lexer_on_error(lexer, "Invalid escaped character '\\%c'", lexer->at[0]);

                lexer->at++;
            }
        }
        else
        {
            lexer->at++;
        }

        array_push(tmp, c);
    }

    if (lexer->at[0] == '"')
    {
        lexer->at++;
    }
    else if (lexer->at[0] == '\n')
    {
        lexer_on_error(lexer, "String literal cannot span multiple lines");
        lexer_on_line(lexer);
        lexer->at++;
        skip_char(lexer, '"');
    }
    else
    {
        lexer_on_error(lexer, "Encountered end-of-file while parsing string literal");
    }

    array_push(tmp, '\0');
    tstr.value = intern_str_lit(tmp, array_len(tmp) - 1);

    allocator_reset(arena);

    return tstr;
}

static TokenInt scan_char(Lexer* lexer)
{
    assert(lexer->at[0] == '\'');
    TokenInt tint = {.rep = TKN_INT_CHAR, .value = 0};

    lexer->at++;

    // Check for empty character literal.
    if (lexer->at[0] == '\'')
    {
        lexer_on_error(lexer, "Character literal cannot be empty");
        lexer->at++;
        return tint;
    }

    if (lexer->at[0] == '\n')
    {
        lexer_on_error(lexer, "Character literal cannot contain a newline character");
        lexer_on_line(lexer);
        lexer->at++;
        skip_char(lexer, '\'');

        return tint;
    }

    // Scan escaped sequence
    if (lexer->at[0] == '\\')
    {
        lexer->at++;

        if ((lexer->at[0] == 'x') || (lexer->at[0] == 'X'))
        {
            lexer->at++;

            int hex_val = scan_hex_escape(lexer);

            tint.value = hex_val >= 0 ? hex_val : 0;
        }
        else
        { // One character escape chars
            int32_t val = unescape_char(lexer->at[0]);

            if (!val && (lexer->at[0] != '0'))
                lexer_on_error(lexer, "Invalid escaped character '\\%c'", lexer->at[0]);

            tint.value = val;
            lexer->at++;
        }
    }
    else
    { // Regular non-escaped char
        tint.value = lexer->at[0];
        lexer->at++;
    }

    // Check for a closing quote
    if (lexer->at[0] != '\'')
    {
        lexer_on_error(lexer, "Missing closing character quote (found '%c' instead)", lexer->at[0]);
        skip_word_end(lexer);
        skip_char(lexer, '\'');
    }
    else
    {
        lexer->at++;
    }

    return tint;
}

Lexer lexer_create(const char* str, uint32_t start, ByteStream* errors)
{
    Lexer lexer = {0};
    lexer.str = str;
    lexer.at = str;
    lexer.start = start;
    lexer.errors = errors;
    lexer.allocator = allocator_create(LEXER_ARENA_BLOCK_SIZE);

    return lexer;
}

void lexer_destroy(Lexer* lexer)
{
#if !defined(NDEBUG) && PRINT_MEM_USAGE
    print_allocator_stats(&lexer->allocator, "Lexer mem stats");
#endif
    allocator_destroy(&lexer->allocator);
}

const char* token_kind_names[] = {
    [TKN_INVALID] = "<invalid>",
    [TKN_EOF] = "<eof>",
    [TKN_LPAREN] = "(",
    [TKN_RPAREN] = ")",
    [TKN_LBRACE] = "{",
    [TKN_RBRACE] = "}",
    [TKN_LBRACKET] = "[",
    [TKN_RBRACKET] = "]",
    [TKN_SEMICOLON] = ";",
    [TKN_COLON] = ":",
    [TKN_COMMA] = ",",
    [TKN_DOT] = ".",
    [TKN_ELLIPSIS] = "..",
    [TKN_ARROW] = "=>",
    [TKN_CAST] = ":>",
    [TKN_POUND] = "#",

    [TKN_STR] = "string literal",
    [TKN_IDENT] = "identifier",
    [TKN_KW] = "keyword",
    [TKN_INT] = "integer literal",
    [TKN_FLOAT] = "floating-point literal",

    [TKN_PLUS] = "+",
    [TKN_MINUS] = "-",
    [TKN_ASTERISK] = "*",
    [TKN_DIV] = "/",
    [TKN_MOD] = "%",
    [TKN_RSHIFT] = ">>",
    [TKN_LSHIFT] = "<<",
    [TKN_AND] = "&",
    [TKN_OR] = "|",
    [TKN_CARET] = "^",
    [TKN_LOGIC_AND] = "&&",
    [TKN_LOGIC_OR] = "||",
    [TKN_NOT] = "!",
    [TKN_NEG] = "~",

    [TKN_QUESTION] = "?",

    [TKN_EQ] = "==",
    [TKN_NOTEQ] = "!=",
    [TKN_GT] = ">",
    [TKN_GTEQ] = ">=",
    [TKN_LT] = "<",
    [TKN_LTEQ] = "<=",

    [TKN_ASSIGN] = "=",
    [TKN_ADD_ASSIGN] = "+=",
    [TKN_SUB_ASSIGN] = "-=",
    [TKN_MUL_ASSIGN] = "*=",
    [TKN_DIV_ASSIGN] = "/=",
    [TKN_AND_ASSIGN] = "&=",
    [TKN_OR_ASSIGN] = "|=",
    [TKN_XOR_ASSIGN] = "^=",
    [TKN_MOD_ASSIGN] = "%=",
};

int print_token(Token* token, char* buf, size_t size)
{
    const char* kind_name = token_kind_names[token->kind];

    switch (token->kind)
    {
        case TKN_INT:
            return snprintf(buf, size, "%lu", token->as_int.value);
        case TKN_FLOAT:
            return snprintf(buf, size, "%.3f", token->as_float.value);
        case TKN_STR:
            return snprintf(buf, size, "\"%s\"", token->as_str.value);
        case TKN_IDENT:
            return snprintf(buf, size, "%s", token->as_ident.value);
        case TKN_KW:
            return snprintf(buf, size, "%s", token->as_kw.name);
        default:
            return snprintf(buf, size, "%s", kind_name);
    }
}

Token scan_token(Lexer* lexer)
{
    Token token = {0};

top:
    token.range.start = lexer_at_pos(lexer);

    switch (lexer->at[0])
    {
        case ' ': case '\t': case '\n': case '\r': case '\v':
            while (is_whitespace(lexer->at[0]))
            {
                if (lexer->at[0] == '\n')
                {
                    lexer_on_line(lexer);
                }

                lexer->at++;
            }

            goto top;
        case '/':
            if (lexer->at[1] == '/')
            {
                while (lexer->at[0] && (lexer->at[0] != '\n') && (lexer->at[0] != '\r'))
                    lexer->at++;

                goto top;
            }
            else if (lexer->at[1] == '*')
            {
                skip_c_comment(lexer);
                goto top;
            }
            else
            {
                lexer->at++;
                token.kind = TKN_DIV;

                if (lexer->at[0] == '=')
                {
                    lexer->at++;
                    token.kind = TKN_DIV_ASSIGN;
                }
            }

            break;
        case '(':
            lexer->at++;
            token.kind = TKN_LPAREN;
            break;
        case ')':
            lexer->at++;
            token.kind = TKN_RPAREN;
            break;
        case '[':
            lexer->at++;
            token.kind = TKN_LBRACKET;
            break;
        case ']':
            lexer->at++;
            token.kind = TKN_RBRACKET;
            break;
        case '{':
            lexer->at++;
            token.kind = TKN_LBRACE;
            break;
        case '}':
            lexer->at++;
            token.kind = TKN_RBRACE;
            break;
        case ';':
            lexer->at++;
            token.kind = TKN_SEMICOLON;
            break;
        case ':':
            lexer->at++;
            token.kind = TKN_COLON;

            if (lexer->at[0] == '>')
            {
                lexer->at++;
                token.kind = TKN_CAST;
            }

            break;
        case ',':
            lexer->at++;
            token.kind = TKN_COMMA;
            break;
        case '#':
            lexer->at++;
            token.kind = TKN_POUND;
            break;
        case '+':
            lexer->at++;
            token.kind = TKN_PLUS;

            if (lexer->at[0] == '=')
            {
                lexer->at++;
                token.kind = TKN_ADD_ASSIGN;
            }

            break;
        case '-':
            lexer->at++;
            token.kind = TKN_MINUS;

            if (lexer->at[0] == '=')
            {
                lexer->at++;
                token.kind = TKN_SUB_ASSIGN;
            }

            break;
        case '*':
            lexer->at++;
            token.kind = TKN_ASTERISK;

            if (lexer->at[0] == '=')
            {
                lexer->at++;
                token.kind = TKN_MUL_ASSIGN;
            }

            break;
        case '%':
            lexer->at++;
            token.kind = TKN_MOD;

            if (lexer->at[0] == '=')
            {
                lexer->at++;
                token.kind = TKN_MOD_ASSIGN;
            }

            break;
        case '!':
            lexer->at++;
            token.kind = TKN_NOT;

            if (lexer->at[0] == '=')
            {
                lexer->at++;
                token.kind = TKN_NOTEQ;
            }

            break;
        case '>':
            lexer->at++;
            token.kind = TKN_GT;

            if (lexer->at[0] == '=')
            {
                lexer->at++;
                token.kind = TKN_GTEQ;
            }
            else if (lexer->at[0] == '>')
            {
                lexer->at++;
                token.kind = TKN_RSHIFT;
            }

            break;
        case '<':
            lexer->at++;
            token.kind = TKN_LT;

            if (lexer->at[0] == '=')
            {
                lexer->at++;
                token.kind = TKN_LTEQ;
            }
            else if (lexer->at[0] == '<')
            {
                lexer->at++;
                token.kind = TKN_LSHIFT;
            }

            break;
        case '&':
            lexer->at++;
            token.kind = TKN_AND;

            if (lexer->at[0] == '&')
            {
                lexer->at++;
                token.kind = TKN_LOGIC_AND;
            }
            else if (lexer->at[0] == '=')
            {
                lexer->at++;
                token.kind = TKN_AND_ASSIGN;
            }

            break;
        case '|':
            lexer->at++;
            token.kind = TKN_OR;

            if (lexer->at[0] == '|')
            {
                lexer->at++;
                token.kind = TKN_LOGIC_OR;
            }
            else if (lexer->at[0] == '=')
            {
                lexer->at++;
                token.kind = TKN_OR_ASSIGN;
            }

            break;
        case '^':
            lexer->at++;
            token.kind = TKN_CARET;

            if (lexer->at[0] == '=')
            {
                lexer->at++;
                token.kind = TKN_XOR_ASSIGN;
            }

            break;
        case '~':
            lexer->at++;
            token.kind = TKN_NEG;
            break;
        case '?':
            lexer->at++;
            token.kind = TKN_QUESTION;
            break;
        case '=':
            lexer->at++;
            token.kind = TKN_ASSIGN;

            if (lexer->at[0] == '=')
            {
                lexer->at++;
                token.kind = TKN_EQ;
            }
            else if (lexer->at[0] == '>')
            {
                lexer->at++;
                token.kind = TKN_ARROW;
            }

            break;
        case '.':
            if (is_dec_digit(lexer->at[1]))
            {
                token.kind = TKN_FLOAT;
                token.as_float = scan_float(lexer);
            }
            else if (lexer->at[1] == '.')
            {
                token.kind = TKN_ELLIPSIS;
                lexer->at += 2;
            }
            else
            {
                token.kind = TKN_DOT;
                lexer->at++;
            }

            break;
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
        {
            // First, determine if this is an integer or a floating point value.
            const char* p = lexer->at;

            while (is_dec_digit(*p))
                p++;

            if ((*p == '.') || (*p == 'e') || (*p == 'E'))
            {
                token.kind = TKN_FLOAT;
                token.as_float = scan_float(lexer);
            }
            else
            {
                token.kind = TKN_INT;
                token.as_int = scan_int(lexer);
            }

            break;
        }
        case '\'':
            token.kind = TKN_INT;
            token.as_int = scan_char(lexer);
            break;
        case '"':
            token.kind = TKN_STR;
            token.as_str = scan_string(lexer);
            break;
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j': case 'k':
        case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u': case 'v':
        case 'w': case 'x': case 'y': case 'z':
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J': case 'K':
        case 'L': case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V':
        case 'W': case 'X': case 'Y': case 'Z': case '_':
        {
            const char* start = lexer->at;

            do
            {
                lexer->at++;
            } while (is_alphanum(lexer->at[0]));

            bool is_kw = false;
            Keyword kw = (Keyword)0;
            const char* interned = intern_ident(start, lexer->at - start, &is_kw, &kw);

            if (is_kw)
            {
                token.kind = TKN_KW;
                token.as_kw.kw = kw;
                token.as_kw.name = interned;
            }
            else
            {
                token.kind = TKN_IDENT;
                token.as_ident.value = interned;
            }

            break;
        }
        case '\0':
            token.kind = TKN_EOF;
            break;
        default:
            lexer_on_error(lexer, "Unexpected token character: %c", lexer->at[0]);
            lexer->at++;
            goto top;
    }

    token.range.end = lexer_at_pos(lexer);

    return token;
}
