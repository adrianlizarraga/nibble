#ifndef NIBBLE_LEXER_H
#define NIBBLE_LEXER_H
#include <stdbool.h>
#include <stdint.h>

#include "allocator.h"

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
    TKN_IDENTIFIER,
    TKN_INT,
    TKN_FLOAT,

    TKN_PLUS,
    TKN_MINUS,
    TKN_ASTERISK,
    TKN_DIV,
    TKN_MOD,
    TKN_RSHIFT,
    TKN_LSHIFT,
    TKN_AND,
    TKN_OR,
    TKN_XOR,
    TKN_LOGIC_AND,
    TKN_LOGIC_OR,

    TKN_EQ,
    TKN_GT,
    TKN_GTEQ,
    TKN_LT,
    TKN_LTEQ,

    TKN_ASSIGN,
    TKN_ADD_ASSIGN,
    TKN_SUB_ASSIGN,
    TKN_MUL_ASSIGN,
    TKN_DIV_ASSIGN,
    TKN_AND_ASSIGN,
    TKN_OR_ASSIGN,
    TKN_XOR_ASSIGN,
    TKN_MOD_ASSIGN,

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

typedef struct TokenIdentifier {
    const char* value;
} TokenIdentifier;

typedef struct TokenStr {
    const char* value;
} TokenStr;

typedef uint32_t ProgPos;

typedef struct Token {
    TokenKind kind;

    ProgPos start;
    ProgPos end;

    union {
        TokenInt tint;
        TokenFloat tfloat;
        TokenIdentifier tidentifier;
        TokenStr tstr;
    };
} Token;

typedef void OnLexErrFunc(void* data, ProgPos pos, const char* msg);
typedef void OnLexLineFunc(void* data, ProgPos pos);
typedef const char* OnLexIdenFunc(void* data, ProgPos pos, const char* str, size_t len);
typedef const char* OnLexStrFunc(void* data, ProgPos pos, const char* str, size_t len);

typedef struct LexerClient {
    void* data;

    OnLexErrFunc* on_error;
    OnLexLineFunc* on_line;
    OnLexIdenFunc* on_identifier;
    OnLexStrFunc* on_str;
} LexerClient;

typedef struct Lexer {
    const char* str;
    const char* at;
    Token token;
    ProgPos start;

    LexerClient client;
    Allocator allocator;
} Lexer;

Lexer lexer_from_str(const char* str, uint32_t start);
void lexer_set_client(Lexer* lexer, void* data, OnLexErrFunc* on_error, OnLexLineFunc* on_line, 
                      OnLexIdenFunc* on_ident, OnLexStrFunc* on_str);
void free_lexer(Lexer* lexer);

int print_token(Token* token, char* buf, size_t size);
bool next_token(Lexer* lexer);
bool match_token(Lexer* lexer, TokenKind kind);
bool is_token(Lexer* lexer, TokenKind kind);
#endif
