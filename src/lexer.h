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
    TKN_DBL_COLON,
    TKN_COMMA,
    TKN_DOT,
    TKN_ELLIPSIS,
    TKN_UNINIT,
    TKN_ARROW,
    TKN_CAST,
    TKN_AT,

    TKN_STR,
    TKN_IDENT,
    TKN_KW,
    TKN_INT,
    TKN_FLOAT,

    TKN_ASTERISK,
    TKN_DIV,
    TKN_MOD,
    TKN_DIVMOD,
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
    TKN_CARET,

    TKN_ASSIGN,
    TKN_ADD_ASSIGN,
    TKN_SUB_ASSIGN,
    TKN_MUL_ASSIGN,
    TKN_DIV_ASSIGN,
    TKN_MOD_ASSIGN,
    TKN_AND_ASSIGN,
    TKN_OR_ASSIGN,
    TKN_XOR_ASSIGN,
    TKN_RSHIFT_ASSIGN,
    TKN_LSHIFT_ASSIGN,

    TKN_KIND_COUNT,
} TokenKind;

typedef enum TokenIntRep {
    TKN_INT_BIN,
    TKN_INT_OCT,
    TKN_INT_DEC,
    TKN_INT_HEX,
    TKN_INT_CHAR,
} TokenIntRep;

typedef enum TokenIntSuffix {
    TKN_INT_SUFFIX_NONE,
    TKN_INT_SUFFIX_U,
    TKN_INT_SUFFIX_L,
    TKN_INT_SUFFIX_UL,
    TKN_INT_SUFFIX_LL,
    TKN_INT_SUFFIX_ULL,
} TokenIntSuffix;

typedef struct TokenInt {
    u64 value;
    TokenIntRep rep;
    TokenIntSuffix suffix;
} TokenInt;

typedef struct TokenFloat {
    FloatKind kind; // TODO: set
    Float value;
} TokenFloat;

typedef struct TokenIdent {
    Identifier* ident;
} TokenIdent;

typedef struct TokenKW {
    Identifier* ident;
} TokenKW;

typedef struct TokenStr {
    StrLit* str_lit;
} TokenStr;

typedef struct Token {
    TokenKind kind;
    ProgRange range;

    union {
        TokenInt as_int;
        TokenFloat as_float;
        TokenIdent as_ident;
        TokenKW as_kw;
        TokenStr as_str;
    };
} Token;

typedef struct Lexer {
    const char* str;
    const char* at;
    ProgPos start;
    Allocator* arena;
    ErrorStream* errors;
    ProgPos** line_pos; // Pointer to stretchy array
} Lexer;

extern const char* token_kind_names[];

Lexer lexer_create(const char* str, ProgPos start, Allocator* arena, ErrorStream* errors, ProgPos** line_pos);

int print_token(Token* token, char* buf, size_t size);
Token scan_token(Lexer* lexer);
#endif
