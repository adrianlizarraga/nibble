#include "parser.h"
#include "ast.h"

static void parser_on_error(Parser* parser, const char* format, ...)
{
    if (parser->errors) {
        char buf[MAX_ERROR_LEN];
        size_t size = 0;
        va_list vargs;

        va_start(vargs, format);
        size = vsnprintf(buf, MAX_ERROR_LEN, format, vargs) + 1;
        va_end(vargs);

        add_byte_stream_chunk(parser->errors, buf, size > sizeof(buf) ? sizeof(buf) : size);
    }
}

Parser parser_create(Allocator* allocator, const char* str, ProgPos pos, ByteStream* errors)
{
    Parser parser = {
        .allocator = allocator,
        .errors = errors,
        .start = pos,
        .lexer = lexer_create(str, pos, errors)
    };

    return parser;
}

void parser_destroy(Parser* parser)
{
    lexer_destroy(&parser->lexer);
}

bool next_token(Parser* parser)
{
    parser->token = scan_token(&parser->lexer);

    return parser->token.kind != TKN_EOF;
}

bool is_token(Parser* parser, TokenKind kind)
{
    return (parser->token.kind == kind);
}

bool match_token(Parser* parser, TokenKind kind)
{
    bool matches = (parser->token.kind == kind);

    if (matches) {
        next_token(parser);
    }

    return matches;
}

bool expect_token(Parser* parser, TokenKind kind)
{
    bool matches = (parser->token.kind == kind);

    if (matches) {
        next_token(parser);
    } else {
        parser_on_error(parser, "Expected token of kind '%s', but got token of kind '%s'", 
                        token_kind_names[kind], token_kind_names[parser->token.kind]);
    }

    return matches;
}

Expr* parse_expr(Parser* parser);

TypeSpec* parse_base_typespec(Parser* parser)
{
    TypeSpec* type = NULL;

#if 0
    if (is_token(parser, TKN_IDENT)) {
        type = typespec_ident(parser->allocator, parser->token.tident.value, parser->token.range); 
    } else if (is_token_ident(parser, func_kw)) {

    } else if (is_token(parser, TKN_LPAREN)) {

    } else {
        parser_on_error(parser, "Invalid typespec token\n");
    }
#endif

    return type;
}

TypeSpec* parse_typespec(Parser* parser)
{
    TypeSpec* type = parse_base_typespec(parser); 

    while (is_token(parser, TKN_ASTERISK) || is_token(parser, TKN_LBRACKET)) {
        if (is_token(parser, TKN_ASTERISK)) {
            type = typespec_ptr(parser->allocator, type, parser->token.range);

            next_token(parser);
        } else {
            assert(is_token(parser, TKN_LBRACKET));
            ProgPos start = parser->token.range.start;

            next_token(parser);

            Expr* len = NULL;
            if (!is_token(parser, TKN_RBRACKET)) {
                len = parse_expr(parser);
            }

            ProgPos end = parser->token.range.end;
            expect_token(parser, TKN_RBRACKET);

            ProgRange range = {.start = start, .end = end};
            type = typespec_array(parser->allocator, type, len, range);
        }
    }

    return type;
}

Expr* parse_expr(Parser* parser)
{
    // TODO: Implement
    return NULL;
}
