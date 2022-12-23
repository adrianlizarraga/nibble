#include "parser/module.h"
#include "ast/module.h"

#include <string.h>

void parser_on_error(Parser* parser, ProgRange range, const char* format, ...)
{
    if (parser->errors) {
        char buf[MAX_ERROR_LEN];
        size_t size = 0;
        va_list vargs;

        va_start(vargs, format);
        size = vsnprintf(buf, MAX_ERROR_LEN, format, vargs) + 1;
        va_end(vargs);

        error_stream_add(parser->errors, range, buf, size > sizeof(buf) ? sizeof(buf) : size);
    }
}

bool next_token(Parser* parser)
{
    parser->ptoken = parser->token;
    parser->token = scan_token(parser->lexer);

    return parser->token.kind != TKN_EOF;
}

bool is_token_kind(Parser* parser, TokenKind kind)
{
    return (parser->token.kind == kind);
}

static const uint8_t token_kind_props[TKN_KIND_COUNT] = {[TKN_QUESTION] = OP_PRECEDENCE_TERNARY,
                                                         [TKN_LOGIC_OR] = OP_PRECEDENCE_OR,
                                                         [TKN_LOGIC_AND] = OP_PRECEDENCE_AND,

                                                         [TKN_EQ] = OP_PRECEDENCE_CMP,
                                                         [TKN_NOTEQ] = OP_PRECEDENCE_CMP,
                                                         [TKN_GT] = OP_PRECEDENCE_CMP,
                                                         [TKN_GTEQ] = OP_PRECEDENCE_CMP,
                                                         [TKN_LT] = OP_PRECEDENCE_CMP,
                                                         [TKN_LTEQ] = OP_PRECEDENCE_CMP,

                                                         [TKN_PLUS] = OP_PRECEDENCE_ADD | OP_PRECEDENCE_UNARY,
                                                         [TKN_MINUS] = OP_PRECEDENCE_ADD | OP_PRECEDENCE_UNARY,
                                                         [TKN_OR] = OP_PRECEDENCE_ADD,
                                                         [TKN_CARET] = OP_PRECEDENCE_ADD | OP_PRECEDENCE_UNARY,

                                                         [TKN_ASTERISK] = OP_PRECEDENCE_MUL | OP_PRECEDENCE_UNARY,
                                                         [TKN_DIV] = OP_PRECEDENCE_MUL,
                                                         [TKN_MOD] = OP_PRECEDENCE_MUL,
                                                         [TKN_DIVMOD] = OP_PRECEDENCE_MUL,
                                                         [TKN_AND] = OP_PRECEDENCE_MUL,
                                                         [TKN_LSHIFT] = OP_PRECEDENCE_MUL,
                                                         [TKN_RSHIFT] = OP_PRECEDENCE_MUL,

                                                         [TKN_NEG] = OP_PRECEDENCE_UNARY,
                                                         [TKN_NOT] = OP_PRECEDENCE_UNARY,

                                                         [TKN_ASSIGN] = OP_ASSIGN,
                                                         [TKN_ADD_ASSIGN] = OP_ASSIGN,
                                                         [TKN_SUB_ASSIGN] = OP_ASSIGN,
                                                         [TKN_MUL_ASSIGN] = OP_ASSIGN,
                                                         [TKN_DIV_ASSIGN] = OP_ASSIGN,
                                                         [TKN_AND_ASSIGN] = OP_ASSIGN,
                                                         [TKN_OR_ASSIGN] = OP_ASSIGN,
                                                         [TKN_XOR_ASSIGN] = OP_ASSIGN,
                                                         [TKN_MOD_ASSIGN] = OP_ASSIGN,
                                                         [TKN_RSHIFT_ASSIGN] = OP_ASSIGN,
                                                         [TKN_LSHIFT_ASSIGN] = OP_ASSIGN};

bool is_token_prop_kind(Parser* parser, uint8_t props)
{
    TokenKind kind = parser->token.kind;

    return (token_kind_props[kind] & props);
}

bool is_keyword(Parser* parser, Keyword kw)
{
    return (parser->token.kind == TKN_KW) && (parser->token.as_kw.ident->kw == kw);
}

bool match_token(Parser* parser, TokenKind kind)
{
    bool matches = (parser->token.kind == kind);

    if (matches) {
        next_token(parser);
    }

    return matches;
}

bool match_keyword(Parser* parser, Keyword kw)
{
    bool matches = (parser->token.kind == TKN_KW) && (parser->token.as_kw.ident->kw == kw);

    if (matches) {
        next_token(parser);
    }

    return matches;
}

void parser_unexpected_token(Parser* parser, TokenKind expected_kind, const char* error_prefix)
{
    char tmp[32];

    print_token(&parser->token, tmp, sizeof(tmp));
    parser_on_error(parser, parser->token.range, "%s: wanted token `%s`, but got token `%s`.",
                    error_prefix ? error_prefix : "Unexpected token", token_kind_names[expected_kind], tmp);
}

bool expect_token(Parser* parser, TokenKind kind, const char* error_prefix)
{
    bool matches = (parser->token.kind == kind);

    if (matches)
        next_token(parser);
    else
        parser_unexpected_token(parser, kind, error_prefix);

    return matches;
}

bool expect_keyword(Parser* parser, Keyword kw, const char* error_prefix)
{
    bool matches = (parser->token.kind == TKN_KW) && (parser->token.as_kw.ident->kw == kw);

    if (matches) {
        next_token(parser);
    }
    else {
        char tmp[32];

        print_token(&parser->token, tmp, sizeof(tmp));
        parser_on_error(parser, parser->token.range, "%s : wanted keyword `%s`, but got token `%s`",
                        error_prefix ? error_prefix : "Unexpected token", keyword_names[kw], tmp);
    }

    return matches;
}

bool skip_after_token(Parser* parser, TokenKind kind)
{
    while (!is_token_kind(parser, TKN_EOF) && !is_token_kind(parser, kind)) {
        next_token(parser);
    }

    return match_token(parser, kind);
}
