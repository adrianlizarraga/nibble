#ifndef NIBBLE_LEXER_H
#define NIBBLE_LEXER_H
#include <stdbool.h>
#include <stdint.h>

#define LEXER_MAX_ERROR_LEN 128

typedef enum TokenType {
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
    TKN_WORD,
    TKN_INT,
    TKN_CHAR,
    TKN_FLOAT,

    TKN_PLUS,
    TKN_MINUS,
    TKN_ASTERISK,
    TKN_DIV,
    TKN_GTEQ,
    TKN_LTEQ,
} TokenType;

typedef struct TokenInt {
    uint64_t value;
    uint32_t base;
} TokenInt;

typedef struct TokenFloat {
    double value;
} TokenFloat;

typedef struct TokenChar {
    int32_t value;
} TokenChar;

typedef struct TokenName {
    const char* value;
} TokenName;

typedef struct TokenStr {
    const char* value;
} TokenStr;

typedef uint32_t ProgPos;

typedef struct Token {
    TokenType type;

    ProgPos start;
    ProgPos end;

    union {
        TokenChar tchar;
        TokenInt tint;
        TokenFloat tfloat;
        TokenName tname;
        TokenStr tstr;
    };
} Token;

typedef struct Lexer {
    const char* str;
    const char* at;
    Token token;

    ProgPos start_pos;

    char error[LEXER_MAX_ERROR_LEN];
} Lexer;

int next_token(Lexer* lexer);

bool match_token(Lexer* lexer, TokenType type);
bool is_token(Lexer* lexer, TokenType type);
#endif
