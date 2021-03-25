#ifndef NIBBLE_LEXER_H
#define NIBBLE_LEXER_H
#include <stdbool.h>
#include <stdint.h>

#include "nibble.h"
#include "allocator.h"
#include "stream.h"

typedef enum TokenKind {
    TKN_INVALID = 0,
    TKN_EOF,
    TKN_LPAREN,
    TKN_RPAREN,
    TKN_LBRACE,
    TKN_RBRACE,
    TKN_LBRACKET,
    TKN_RBRACKET,
    TKN_SEMICOLON,
    TKN_COLON,
    TKN_COMMA,
    TKN_DOT,

    TKN_STR,
    TKN_IDENT,
    TKN_INT,
    TKN_FLOAT,

    TKN_ASTERISK,
    TKN_DIV,
    TKN_MOD,
    TKN_RSHIFT,
    TKN_LSHIFT,
    TKN_AND,
    TKN_LOGIC_AND,
    TKN_LOGIC_OR,
    TKN_NOT,
    TKN_NEG,

    TKN_QUESTION,

    TKN_EQ,
    TKN_NOTEQ,
    TKN_GT,
    TKN_GTEQ,
    TKN_LT,
    TKN_LTEQ,

    TKN_PLUS,
    TKN_MINUS,
    TKN_OR,
    TKN_XOR,

    TKN_ASSIGN,
    TKN_ADD_ASSIGN,
    TKN_SUB_ASSIGN,
    TKN_MUL_ASSIGN,
    TKN_DIV_ASSIGN,
    TKN_AND_ASSIGN,
    TKN_OR_ASSIGN,
    TKN_XOR_ASSIGN,
    TKN_MOD_ASSIGN,

    TKN_KIND_COUNT,
} TokenKind;

typedef enum TokenIntRep {
    TKN_INT_BIN,
    TKN_INT_OCT,
    TKN_INT_DEC,
    TKN_INT_HEX,
    TKN_INT_CHAR,
} TokenIntRep;

typedef struct TokenInt {
    uint64_t value;
    TokenIntRep rep;
} TokenInt;

typedef struct TokenFloat {
    double value;
} TokenFloat;

typedef struct TokenIdent {
    const char* value;
} TokenIdent;

typedef struct TokenStr {
    const char* value;
} TokenStr;


typedef struct Token {
    TokenKind kind;
    ProgRange range;

    union {
        TokenInt tint;
        TokenFloat tfloat;
        TokenIdent tident;
        TokenStr tstr;
    };
} Token;

typedef struct Lexer {
    const char* str;
    const char* at;
    ProgPos start;
    Allocator allocator;
    ByteStream* errors;
} Lexer;

Lexer lexer_create(const char* str, uint32_t start, ByteStream* errors);
void lexer_destroy(Lexer* lexer);

int print_token(Token* token, char* buf, size_t size);
Token scan_token(Lexer* lexer);
#endif
