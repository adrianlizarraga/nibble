#ifndef NIBBLE_PARSER_H
#define NIBBLE_PARSER_H
#include "nibble.h"
#include "allocator.h"
#include "lexer.h"

typedef struct Parser {
    Allocator* allocator;
    ByteStream* errors;
    ProgPos start;
    Lexer lexer;
    Token token;
    Token ptoken;
} Parser;

Parser parser_create(Allocator* allocator, const char* str, ProgPos pos, ByteStream* errors);
void parser_destroy(Parser* parser);

typedef enum OpPrecedence {
    OP_PRECEDENCE_TERNARY = 1 << 0,
    OP_PRECEDENCE_OR = 1 << 1,
    OP_PRECEDENCE_AND = 1 << 2,
    OP_PRECEDENCE_CMP = 1 << 3,
    OP_PRECEDENCE_ADD = 1 << 4,
    OP_PRECEDENCE_MUL = 1 << 5,
    OP_PRECEDENCE_UNARY = 1 << 6,
} OpPrecedence;

bool next_token(Parser* parser);
bool is_token(Parser* parser, TokenKind kind);
bool is_token_op(Parser* parser, uint8_t precedence);
bool is_keyword(Parser* parser, Keyword kw);
bool match_token_next(Parser* parser, TokenKind kind);
bool match_keyword_next(Parser* parser, Keyword kw);
bool expect_token_next(Parser* parser, TokenKind kind, const char* error_prefix);
bool expect_keyword_next(Parser* parser, Keyword kw, const char* error_prefix);
bool skip_after_token(Parser* parser, TokenKind kind);


TypeSpec* parse_typespec(Parser* parser);
Expr* parse_expr(Parser* parser);
Stmt* parse_stmt(Parser* parser);
Decl* parse_decl(Parser* parser);
#endif
