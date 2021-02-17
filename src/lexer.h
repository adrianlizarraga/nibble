#ifndef NIBBLE_LEXER_H
#define NIBBLE_LEXER_H
#include <stdint.h>

#define LEXER_MAX_NUM_ERRORS 4
#define LEXER_MAX_ERROR_LEN 128

typedef enum TokenType TokenType;
typedef enum TokenIntBase TokenIntBase;
typedef struct TokenInt TokenInt;
typedef struct TokenChar TokenChar;
typedef struct TokenFloat TokenFloat;
typedef struct TokenStr TokenStr;
typedef struct TokenName TokenName;
typedef struct Token Token;
typedef struct Lexer Lexer;

enum TokenType {
    TKN_UNKNOWN,
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
    TKN_AT,
 
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

    uint32_t line;
    uint32_t column;

    union {
	TokenChar char_;
	TokenInt int_;
	TokenFloat float_;
	TokenName name_;
	TokenStr str_;
    };
};

struct Lexer {
    const char* at;
    uint32_t line;
    uint32_t column;

    // TODO: Make this an error stream
    char errors[LEXER_MAX_NUM_ERRORS][LEXER_MAX_ERROR_LEN];
    unsigned int num_errors;
};

Token next_token(Lexer* lexer); // Returns the number of errors for now. (0 on success)
#endif
