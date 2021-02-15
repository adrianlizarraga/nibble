#ifndef NIBBLE_LEXER_H
#define NIBBLE_LEXER_H
#include <stdint.h>

#include "str.h"


typedef enum TokenType TokenType;
typedef enum TokenIntBase TokenIntBase;
typedef struct TokenInt TokenInt;
typedef struct TokenChar TokenChar;
typedef struct TokenFloat TokenFloat;
typedef struct TokenStr TokenStr;
typedef struct TokenName TokenName;
typedef struct Token Token;
typedef struct ScanToken ScanToken;
typedef struct Lexer Lexer;

enum TokenType {
    TKN_EOF,
    TKN_LPAREN,
    TKN_RPARAN,
    TKN_LBRACE,
    TKN_RBRACE,
    TKN_LBRACKET,
    TKN_RBRACKET,
    TKN_SEMICOLON,
    TKN_COLON,
    TKN_COMMA,
    TKN_DOT,
    TKN_AT,
 
    TKN_STR,
    TKN_NAME,
    TKN_KEYWORD,
    TKN_INT,
    TKN_CHAR,
    TKN_FLOAT,

    TKN_PLUS,
    TKN_MINUS,
    TKN_ASTERISK,
    TKN_DIV,
    TKN_GTEQ,
    TKN_LTEQ,
};

enum TokenIntBase {
    TKN_INT_DEC,
    TKN_INT_HEX,
    TKN_INT_OCT,
    TKN_INT_BIN,
};

struct TokenInt {
    uint64_t value;
    TokenIntBase base;    
};

struct TokenFloat {
    double value;
};

struct TokenChar {
    uint32_t value;
};

struct TokenName {
    const char* value;
};

struct TokenStr {
    const char* value;
};

struct Token {
    TokenType type;

    union {
	TokenChar char_;
	TokenInt int_;
	TokenFloat float_;
	TokenName name_;
	TokenStr str_;
    };
};

struct ScanToken {
    Token token;

    size_t index;
    size_t len;

    size_t line;
    size_t column;
};



struct Lexer {
    ScanToken token;

    String cursor;
};

int next_token(Lexer* lexer);
#endif
