#ifndef NIBBLE_PARSER_H
#define NIBBLE_PARSER_H
#include "nibble.h"
#include "allocator.h"
#include "lexer/module.h"
#include "ast/module.h"

typedef struct Parser {
    Allocator* ast_arena;
    ErrorStream* errors;
    Lexer* lexer;
    Token token;
    Token ptoken;
} Parser;

bool next_token(Parser* parser);
bool is_token_kind(Parser* parser, TokenKind kind);

TypeSpec* parse_typespec(Parser* parser);
Expr* parse_expr(Parser* parser);
Stmt* parse_stmt(Parser* parser);
Decl* parse_decl(Parser* parser);
Stmt* parse_global_stmt(Parser* parser);
#endif
