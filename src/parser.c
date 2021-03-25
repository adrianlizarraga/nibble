#include "ast.h"
#include "parser.h"

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
    Parser parser = {.allocator = allocator, .errors = errors, .start = pos, .lexer = lexer_create(str, pos, errors)};

    return parser;
}

void parser_destroy(Parser* parser)
{
    lexer_destroy(&parser->lexer);
}

bool next_token(Parser* parser)
{
    parser->ptoken = parser->token;
    parser->token = scan_token(&parser->lexer);

    return parser->token.kind != TKN_EOF;
}

bool is_token(Parser* parser, TokenKind kind)
{
    return (parser->token.kind == kind);
}

static const uint8_t op_precedence[TKN_KIND_COUNT] = {
    [TKN_QUESTION] = OP_PRECEDENCE_TERNARY,
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
    [TKN_XOR] = OP_PRECEDENCE_ADD,

    [TKN_ASTERISK] = OP_PRECEDENCE_MUL | OP_PRECEDENCE_UNARY,
    [TKN_DIV] = OP_PRECEDENCE_MUL,
    [TKN_MOD] = OP_PRECEDENCE_MUL,
    [TKN_AND] = OP_PRECEDENCE_MUL | OP_PRECEDENCE_UNARY,
    [TKN_LSHIFT] = OP_PRECEDENCE_MUL,
    [TKN_RSHIFT] = OP_PRECEDENCE_MUL,

    [TKN_NEG] = OP_PRECEDENCE_UNARY,
    [TKN_NOT] = OP_PRECEDENCE_UNARY,
};

bool is_token_op(Parser* parser, uint8_t precedence)
{
    TokenKind kind = parser->token.kind;

    return (op_precedence[kind] & precedence);
}

bool match_keyword_next(Parser* parser, Keyword kw)
{
    bool matches = (parser->token.kind == TKN_IDENT) && (parser->token.tident.value == keywords[kw]);

    if (matches) {
        next_token(parser);
    }

    return matches;
}

bool match_token_next(Parser* parser, TokenKind kind)
{
    bool matches = (parser->token.kind == kind);

    if (matches) {
        next_token(parser);
    }

    return matches;
}

bool expect_token_next(Parser* parser, TokenKind kind)
{
    bool matches = (parser->token.kind == kind);

    if (matches) {
        next_token(parser);
    } else {
        parser_on_error(parser, "Expected token of kind '%s', but got token of kind '%s'", token_kind_names[kind],
                        token_kind_names[parser->token.kind]);
    }

    return matches;
}

///////////////////////////////
//    Parse type specifiers
//////////////////////////////
static TypeSpecParam* parse_typespec_func_param(Parser* parser)
{
    TypeSpec* type = parse_typespec(parser);
    const char* name = NULL;

    if (match_token_next(parser, TKN_COLON)) {
        if (type->kind != TYPE_SPEC_IDENT) {
            parser_on_error(parser, "A colon in a parameter type specification must be preceded by an identifier");
        } else {
            name = type->ident.name;
        }

        type = parse_typespec(parser);
    }

    return typespec_func_param(parser->allocator, type, name);
}

static TypeSpec* parse_typespec_func(Parser* parser)
{
    ProgRange range = {.start = parser->ptoken.range.start};

    expect_token_next(parser, TKN_LPAREN);

    size_t num_params = 0;
    DLList params = dllist_head_create(params);
    if (!is_token(parser, TKN_RPAREN)) {
        TypeSpecParam* param = parse_typespec_func_param(parser);

        dllist_add(params.prev, &param->list);
        num_params += 1;

        while (match_token_next(parser, TKN_COMMA)) {
            param = parse_typespec_func_param(parser);

            dllist_add(params.prev, &param->list);
            num_params += 1;
        }
    }

    expect_token_next(parser, TKN_RPAREN);

    TypeSpec* ret = NULL;
    if (match_token_next(parser, TKN_COLON)) {
        ret = parse_typespec(parser);
    }

    range.end = parser->ptoken.range.end;

    return typespec_func(parser->allocator, num_params, &params, ret, range);
}

static TypeSpec* parse_typespec_base(Parser* parser)
{
    TypeSpec* type = NULL;

    if (match_token_next(parser, TKN_IDENT)) {
        type = typespec_ident(parser->allocator, parser->ptoken.tident.value, parser->ptoken.range);
    } else if (match_keyword_next(parser, KW_FUNC)) {
        type = parse_typespec_func(parser);
    } else if (match_token_next(parser, TKN_LPAREN)) {
        type = parse_typespec(parser);

        expect_token_next(parser, TKN_RPAREN);
    } else {
        parser_on_error(parser, "Invalid typespec token\n");
    }

    return type;
}

TypeSpec* parse_typespec(Parser* parser)
{
    TypeSpec* type = parse_typespec_base(parser);

    while (is_token(parser, TKN_ASTERISK) || is_token(parser, TKN_LBRACKET)) {
        if (match_token_next(parser, TKN_ASTERISK)) {
            type = typespec_ptr(parser->allocator, type, parser->ptoken.range);
        } else {
            assert(is_token(parser, TKN_LBRACKET));
            ProgPos start = parser->token.range.start;

            next_token(parser);

            Expr* len = NULL;
            if (!is_token(parser, TKN_RBRACKET)) {
                len = parse_expr(parser);
            }

            ProgPos end = parser->token.range.end;
            expect_token_next(parser, TKN_RBRACKET);

            ProgRange range = {.start = start, .end = end};
            type = typespec_array(parser->allocator, type, len, range);
        }
    }

    return type;
}

///////////////////////////////
//    Parse expressions
//////////////////////////////

static Expr* parse_expr_unary(Parser* parser);

// expr_base = TKN_INT
//           | TKN_FLOAT
//           | TKN_STR
//           | TKN_IDENT
//           | expr_cast
//           | expr_compound_init
//           | expr_sizeof
//           | '(' expr ')'
//
// typespec_spec = '(' ':' typespec ')'
// expr_cast = typespec_spec? expr_unary
// expr_sizeof = KW_SIZEOF '(' ((':' typespec) | expr) ')'
static Expr* parse_expr_base(Parser* parser)
{
    Expr* expr = NULL;

    if (match_token_next(parser, TKN_INT)) {
        Token* token = &parser->ptoken;
        expr = expr_int(parser->allocator, token->tint.value, token->range);
    } else if (match_token_next(parser, TKN_FLOAT)) {
        Token* token = &parser->ptoken;
        expr = expr_float(parser->allocator, token->tfloat.value, token->range);
    } else if (match_token_next(parser, TKN_STR)) {
        Token* token = &parser->ptoken;
        expr = expr_str(parser->allocator, token->tstr.value, token->range);
    } else if (match_token_next(parser, TKN_IDENT)) {
        Token* token = &parser->ptoken;
        expr = expr_ident(parser->allocator, token->tident.value, token->range);
    } else if (match_token_next(parser, TKN_LPAREN)) {
        ProgRange range = { .start = parser->ptoken.range.start };

        if (match_token_next(parser, TKN_COLON)) {
            TypeSpec* type = parse_typespec(parser);
            expect_token_next(parser, TKN_RPAREN);

            if (is_token(parser, TKN_LBRACE)) {
                // TODO: parse compound initializer expression.
                assert(0);
            } else {
                Expr* unary = parse_expr_unary(parser);

                range.end = unary->range.end;
                expr = expr_cast(parser->allocator, type, unary, range);
            }
        } else {
            expr = parse_expr(parser);
            expect_token_next(parser, TKN_RPAREN);
        }
    } else if (match_keyword_next(parser, KW_SIZEOF)) {
        ProgRange range = { .start = parser->ptoken.range.start };

        expect_token_next(parser, TKN_LPAREN);

        if (match_token_next(parser, TKN_COLON)) {
            TypeSpec* type = parse_typespec(parser);
            expect_token_next(parser, TKN_RPAREN);

            range.end = parser->ptoken.range.end;
            expr = expr_sizeof_type(parser->allocator, type, range);
        } else {
            Expr* arg = parse_expr(parser);
            expect_token_next(parser, TKN_RPAREN);

            range.end = parser->ptoken.range.end;
            expr = expr_sizeof_expr(parser->allocator, arg, range);
        }
    }
    else {
        parser_on_error(parser, "Unexpected token in expression"); // TODO: Better info
    }

    return expr;
}

// expr_call_arg = (IDENTIFIER '=')? expr
static ExprCallArg* parse_expr_call_arg(Parser* parser)
{
    Expr* expr = parse_expr(parser);
    const char* name = NULL;

    if (match_token_next(parser, TKN_ASSIGN)) {
        if (expr->kind != EXPR_IDENT) {
            parser_on_error(parser, "Function argument's name must be an alphanumeric identifier");
        } else {
            name = expr->eident.name;
        }

        expr = parse_expr(parser);
    }

    return expr_call_arg(parser->allocator, expr, name);
}

// expr_deref = expr_base ('.' IDENTIFIER | '[' expr ']' | '(' expr_call_arg_list* ')')*
// expr_call_arg_list = expr_call_arg (',' expr_call_arg)*
static Expr* parse_expr_deref(Parser* parser)
{
    Expr* expr = parse_expr_base(parser);

    while (is_token(parser, TKN_DOT) || is_token(parser, TKN_LBRACKET) || is_token(parser, TKN_LPAREN)) {
        if (match_token_next(parser, TKN_DOT)) {
            expect_token_next(parser, TKN_IDENT);

            const char* field = parser->ptoken.tident.value;
            ProgRange range = {
                .start = expr->range.start,
                .end = parser->ptoken.range.end,
            };

            expr = expr_field(parser->allocator, expr, field, range);
        } else if (match_token_next(parser, TKN_LBRACKET)) {
            Expr* index = parse_expr(parser);
            expect_token_next(parser, TKN_RBRACKET);
            ProgRange range = {
                .start = expr->range.start,
                .end = parser->ptoken.range.end,
            };

            expr = expr_index(parser->allocator, expr, index, range);
        } else {
            assert(is_token(parser, TKN_LPAREN));
            next_token(parser);

            size_t num_args = 0;
            DLList args = dllist_head_create(args);
            if (!is_token(parser, TKN_RPAREN)) {
                ExprCallArg* arg = parse_expr_call_arg(parser);

                dllist_add(args.prev, &arg->list);
                num_args += 1;

                while (match_token_next(parser, TKN_COMMA)) {
                    arg = parse_expr_call_arg(parser);

                    dllist_add(args.prev, &arg->list);
                    num_args += 1;
                }
            }

            expect_token_next(parser, TKN_RPAREN);
            ProgRange range = {
                .start = expr->range.start,
                .end = parser->ptoken.range.end,
            };

            expr = expr_call(parser->allocator, expr, num_args, &args, range);
        }
    }

    return expr;
}

// expr_unary = OP_PRECEDENCE_UNARY expr_unary
//            | expr_deref
static Expr* parse_expr_unary(Parser* parser)
{
    if (is_token_op(parser, OP_PRECEDENCE_UNARY)) {
        next_token(parser);
        ProgRange range = {.start = parser->ptoken.range.start};
        TokenKind op = parser->ptoken.kind;
        Expr* expr = parse_expr_unary(parser);

        range.end = expr->range.end;
        return expr_unary(parser->allocator, op, expr, range);
    }

    return parse_expr_deref(parser);
}

// expr_mul = expr_unary (OP_PRECEDENCE_MUL expr_unary)*
static Expr* parse_expr_mul(Parser* parser)
{
    Expr* expr = parse_expr_unary(parser);

    while (is_token_op(parser, OP_PRECEDENCE_MUL)) {
        next_token(parser);

        TokenKind op = parser->ptoken.kind;
        Expr* left = expr;
        Expr* right = parse_expr_unary(parser);
        expr = expr_binary(parser->allocator, op, left, right);
    }

    return expr;
}

// expr_add = expr_mul (OP_PRECEDENCE_ADD expr_mul)*
static Expr* parse_expr_add(Parser* parser)
{
    Expr* expr = parse_expr_mul(parser);

    while (is_token_op(parser, OP_PRECEDENCE_ADD)) {
        next_token(parser);

        TokenKind op = parser->ptoken.kind;
        Expr* left = expr;
        Expr* right = parse_expr_mul(parser);
        expr = expr_binary(parser->allocator, op, left, right);
    }

    return expr;
}

// expr_cmp = expr_add (OP_PRECEDENCE_CMP expr_add)*
static Expr* parse_expr_cmp(Parser* parser)
{
    Expr* expr = parse_expr_add(parser);

    while (is_token_op(parser, OP_PRECEDENCE_CMP)) {
        next_token(parser);

        TokenKind op = parser->ptoken.kind;
        Expr* left = expr;
        Expr* right = parse_expr_add(parser);
        expr = expr_binary(parser->allocator, op, left, right);
    }

    return expr;
}

// expr_and = expr_cmp ('&&' expr_cmp)*
static Expr* parse_expr_and(Parser* parser)
{
    Expr* expr = parse_expr_cmp(parser);

    while (match_token_next(parser, TKN_LOGIC_AND)) {
        Expr* left = expr;
        Expr* right = parse_expr_cmp(parser);
        expr = expr_binary(parser->allocator, TKN_LOGIC_AND, left, right);
    }

    return expr;
}

// expr_or = expr_and ('||' expr_and)*
static Expr* parse_expr_or(Parser* parser)
{
    Expr* expr = parse_expr_and(parser);

    while (match_token_next(parser, TKN_LOGIC_OR)) {
        Expr* left = expr;
        Expr* right = parse_expr_and(parser);
        expr = expr_binary(parser->allocator, TKN_LOGIC_OR, left, right);
    }

    return expr;
}

// expr_ternary = expr_or ('?' expr_ternary ':' expr_ternary)?
static Expr* parse_expr_ternary(Parser* parser)
{
    Expr* expr = parse_expr_or(parser);

    if (match_token_next(parser, TKN_QUESTION)) {
        Expr* then_expr = parse_expr_ternary(parser);
        expect_token_next(parser, TKN_COLON);
        Expr* else_expr = parse_expr_ternary(parser);

        expr = expr_ternary(parser->allocator, expr, then_expr, else_expr);
    }

    return expr;
}

// expr = expr_ternary
Expr* parse_expr(Parser* parser)
{
    return parse_expr_ternary(parser);
}
