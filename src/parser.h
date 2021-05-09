#ifndef NIBBLE_PARSER_H
#define NIBBLE_PARSER_H
#include "nibble.h"
#include "allocator.h"
#include "lexer.h"

typedef struct Parser {
    Allocator* ast_arena;
    Allocator temp_arena;
    ByteStream* errors;
    ProgPos start;
    Lexer lexer;
    Token token;
    Token ptoken;

    Scope* curr_scope;
} Parser;

void parser_init(Parser* parser, Allocator* ast_arena, const char* str, ProgPos pos, ByteStream* errors);
void parser_destroy(Parser* parser);

typedef enum TokenKindProps {
    OP_PRECEDENCE_TERNARY = 1 << 0,
    OP_PRECEDENCE_OR = 1 << 1,
    OP_PRECEDENCE_AND = 1 << 2,
    OP_PRECEDENCE_CMP = 1 << 3,
    OP_PRECEDENCE_ADD = 1 << 4,
    OP_PRECEDENCE_MUL = 1 << 5,
    OP_PRECEDENCE_UNARY = 1 << 6,

    OP_ASSIGN = 1 << 7,
} TokenKindProps;

bool next_token(Parser* parser);
bool is_token_kind(Parser* parser, TokenKind kind);
bool is_token_prop_kind(Parser* parser, uint8_t props);
bool is_keyword(Parser* parser, Keyword kw);
bool match_token(Parser* parser, TokenKind kind);
bool match_keyword(Parser* parser, Keyword kw);
bool expect_token(Parser* parser, TokenKind kind, const char* error_prefix);
bool expect_keyword(Parser* parser, Keyword kw, const char* error_prefix);
bool skip_after_token(Parser* parser, TokenKind kind);


TypeSpec* parse_typespec(Parser* parser);
Expr* parse_expr(Parser* parser);
Stmt* parse_stmt(Parser* parser);
Decl* parse_decl(Parser* parser);
#endif
