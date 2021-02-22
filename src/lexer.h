#ifndef NIBBLE_LEXER_H
#define NIBBLE_LEXER_H
#include <stdbool.h>
#include <stdint.h>

#define LEXER_MAX_NUM_ERRORS 10
#define LEXER_MAX_ERROR_LEN 128

#define INT_HEX_BASE 16U
#define INT_DEC_BASE 10U
#define INT_OCT_BASE 8U
#define INT_BIN_BASE 2U

typedef enum TokenType {
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

typedef struct FilePos {
    uint32_t line;
    uint32_t column;
} FilePos;

typedef struct Token {
    TokenType type;
    FilePos pos;

    union {
	TokenChar char_;
	TokenInt int_;
	TokenFloat float_;
	TokenName name_;
	TokenStr str_;
    };
} Token;

typedef struct Lexer {
    const char* at;
    const char* line_at;
    uint32_t line;
    Token token;

    // TODO: Make this an error stream
    char errors[LEXER_MAX_NUM_ERRORS][LEXER_MAX_ERROR_LEN];
    unsigned int num_errors;
} Lexer;

void init_lexer(Lexer* lexer, const char* str);

bool next_token(Lexer* lexer);
bool is_token(Lexer* lexer, TokenType type);
bool match_token(Lexer* lexer, TokenType type);

// IDEAS:
// - Everything is a token, including errors (TKN_UNKNOWN), newlines, and comments.
// - Parser will take care of throwing away unnecessary tokens.
// - No need for Lexer state struct (or can be simpler)
// - int next_token(Token* token, const char* str); // Returns number of characters consumed? Or is that in the token itself?
// - Token next_token(const char* str); // Token itself has characters consumed (via start & end pointers)?
#endif
