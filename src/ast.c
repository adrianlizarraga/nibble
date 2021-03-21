#include "ast.h"

TypeSpec* typespec_alloc(Allocator* allocator, TypeSpecKind kind, ProgRange range)
{
    TypeSpec* type = mem_allocate(allocator, sizeof(TypeSpec), DEFAULT_ALIGN, true); 
    type->kind = kind;
    type->range = range;

    return type;
}

TypeSpec* typespec_ident(Allocator* allocator, const char* name, ProgRange range)
{
    TypeSpec* type = typespec_alloc(allocator, TYPE_SPEC_IDENT, range); 
    type->ident.name = name;

    return type;
}

TypeSpec* typespec_ptr(Allocator* allocator, TypeSpec* base, ProgRange range)
{
    TypeSpec* type = typespec_alloc(allocator, TYPE_SPEC_PTR, range);
    type->ptr.base = base;
}

TypeSpec* typespec_array(Allocator* allocator, TypeSpec* base, Expr* len, ProgRange range)
{
    TypeSpec* type = typespec_alloc(allocator, TYPE_SPEC_ARRAY, range);
    type->array.base = base;
    type->array.len = len;
}

bool is_token(Parser* parser, TokenKind kind)
{
    return (parser->lexer.token.kind == kind);
}

bool match_token(Parser* parser, TokenKind kind)
{
    Lexer* lexer = &parser->lexer;
    bool matches = (lexer->token.kind == kind);

    if (matches) {
        next_token(lexer);
    }

    return matches;
}

bool expect_token(Parser* parser, TokenKind kind)
{
    Lexer* lexer = &parser->lexer;
    bool matches = (lexer->token.kind == kind);

    if (matches) {
        next_token(lexer);
    } else {
        // TODO: Handle error properly.
        fprintf(stderr, "Expected token of kind '%s', but got token of kind '%s'", 
                token_kind_names[kind], token_kind_names[lexer->token.kind]);
    }

    return matches;
}

Expr* parse_expr(Parser* parser);

TypeSpec* parse_base_typespec(Parser* parser)
{
    TypeSpec* type = NULL;

    if (is_token(parser, TKN_IDENTIFIER)) {
        type = typespec_ident(allocator, lexer->token.tidentifier.value, lexer->token.range); 
    } else if (is_token_ident(parser, func_kw)) {

    } else if (is_token(parser, TKN_LPAREN)) {

    } else {
        // TODO: Add proper error reporting here.
        fprintf(stderr, "Invalid typespec token\n");
    }

    return type;
}

TypeSpec* parse_typespec(Parser* parser)
{
    TypeSpec* type = parse_base_typespec(parser); 

    while (is_token(parser, TKN_ASTERISK) || is_token(parser, TKN_LBRACKET)) {
        if (is_token(parser, TKN_ASTERISK)) {
            type = typespec_ptr(parser->allocator, type, lexer->token.range);

            next_token(&parser->lexer);
        } else {
            assert(is_token(parser, TKN_LBRACKET));
            ProgPos start = parser->lexer.token.range.start;

            next_token(&parser->lexer);

            Expr* len = NULL;
            if (!is_token(parser, TKN_RBRACKET)) {
                len = parse_expr(parser);
            }

            ProgPos end = parser->lexer.token.range.end;
            expect_token(parser, TKN_RBRACKET);

            ProgRange range = {.start = start, .end = end};
            type = typespec_array(allocator, type, len, range);
        }
    }

    return type;
}

Expr* parse_expr(Parser* parser)
{
    // TODO: Implement
    return NULL;
}
