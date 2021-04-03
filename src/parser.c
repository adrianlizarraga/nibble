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
    [TKN_CARET] = OP_PRECEDENCE_ADD | OP_PRECEDENCE_UNARY,

    [TKN_ASTERISK] = OP_PRECEDENCE_MUL | OP_PRECEDENCE_UNARY,
    [TKN_DIV] = OP_PRECEDENCE_MUL,
    [TKN_MOD] = OP_PRECEDENCE_MUL,
    [TKN_AND] = OP_PRECEDENCE_MUL,
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

bool is_keyword(Parser* parser, Keyword kw)
{
    return (parser->token.kind == TKN_IDENT) && (parser->token.tident.value == keywords[kw]);
}

bool match_token_next(Parser* parser, TokenKind kind)
{
    bool matches = (parser->token.kind == kind);

    if (matches) {
        next_token(parser);
    }

    return matches;
}

bool match_keyword_next(Parser* parser, Keyword kw)
{
    bool matches = (parser->token.kind == TKN_IDENT) && (parser->token.tident.value == keywords[kw]);

    if (matches) {
        next_token(parser);
    }

    return matches;
}

bool expect_token_next(Parser* parser, TokenKind kind, const char* error_prefix)
{
    bool matches = (parser->token.kind == kind);

    if (matches) {
        next_token(parser);
    } else {
        char tmp[32];

        print_token(&parser->token, tmp, sizeof(tmp));
        parser_on_error(parser, "%s: wanted token `%s`, but got token `%s`.",
                        error_prefix ? error_prefix : "Unexpected token", token_kind_names[kind], tmp);
    }

    return matches;
}

bool expect_keyword_next(Parser* parser, Keyword kw, const char* error_prefix)
{
    bool matches = (parser->token.kind == TKN_IDENT) && (parser->token.tident.value == keywords[kw]);

    if (matches) {
        next_token(parser);
    } else {
        char tmp[32];

        print_token(&parser->token, tmp, sizeof(tmp));
        parser_on_error(parser, "%s : wanted keyword `%s`, but got token `%s`",
                        error_prefix ? error_prefix : "Unexpected token", keywords[kw], tmp);
    }

    return matches;
}

bool skip_after_token(Parser* parser, TokenKind kind)
{
    while (!is_token(parser, TKN_EOF) && !is_token(parser, kind)) {
        next_token(parser);
    }

    return match_token_next(parser, kind);
}

///////////////////////////////
//    Parse type specifiers
//////////////////////////////

// aggregate_field = TKN_IDENT ':' type_spec ';'
static AggregateField* parse_aggregate_field(Parser* parser)
{
    AggregateField* field = NULL;
    const char* error_prefix = "Failed to parse field";

    if (expect_token_next(parser, TKN_IDENT, error_prefix)) {
        const char* name = parser->ptoken.tident.value;
        ProgRange range = {.start = parser->ptoken.range.start};

        if (expect_token_next(parser, TKN_COLON, error_prefix)) {
            TypeSpec* type = parse_typespec(parser);

            if (type && expect_token_next(parser, TKN_SEMICOLON, error_prefix)) {
                range.end = parser->ptoken.range.end;
                field = aggregate_field(parser->allocator, name, type, range);
            }
        }
    }

    return field;
}

// aggregate_body  = TKN_IDENT '{' aggregate_field* '}'
static bool parse_fill_aggregate_body(Parser* parser, AggregateBody* body)
{
    bool bad_field = false;

    dllist_head_init(&body->fields);
    body->num_fields = 0;

    while (!is_token(parser, TKN_RBRACE) && !is_token(parser, TKN_EOF)) {
        AggregateField* field = parse_aggregate_field(parser);

        if (field) {
            body->num_fields += 1;
            dllist_add(body->fields.prev, &field->list);
        } else {
            bad_field = true;
            break;
        }
    }

    return bad_field;
}

static TypeSpec* parse_typespec_anon_aggregate(Parser* parser, const char* error_prefix,
                                               TypeSpecAggregateFunc* typespec_aggregate_fn)
{
    assert(is_keyword(parser, KW_STRUCT) || is_keyword(parser, KW_UNION));

    TypeSpec* type = NULL;
    ProgRange range = {.start = parser->token.range.start};

    next_token(parser);

    if (expect_token_next(parser, TKN_LBRACE, error_prefix)) {
        AggregateBody body = {0};
        bool bad_field = parse_fill_aggregate_body(parser, &body);

        if (!bad_field && expect_token_next(parser, TKN_RBRACE, error_prefix)) {
            if (body.num_fields) {
                range.end = parser->ptoken.range.end;
                type = typespec_aggregate_fn(parser->allocator, body.num_fields, &body.fields, range);
            } else {
                parser_on_error(parser, "%s: must have at least one field", error_prefix);
            }
        }
    }

    return type;
}

// typespec_func_param = typespec
static TypeSpecParam* parse_typespec_func_param(Parser* parser)
{
    TypeSpecParam* type_param = NULL;
    TypeSpec* type = parse_typespec(parser);

    if (type) {
        type_param = typespec_func_param(parser->allocator, type);
    }

    return type_param;
}

// typespec_func = 'func' '(' typespec_func_param_list? ')' ('=>' typespec)?
//
// typespec_func_param_list = typespec_func_param (',' typespec_func_param)*
static TypeSpec* parse_typespec_func(Parser* parser)
{
    assert(is_keyword(parser, KW_FUNC));
    TypeSpec* type = NULL;
    ProgRange range = {.start = parser->token.range.start};
    const char* error_prefix = "Failed to parse function type specification";

    next_token(parser);

    if (expect_token_next(parser, TKN_LPAREN, error_prefix)) {
        size_t num_params = 0;
        DLList params = dllist_head_create(params);
        bool bad_param = false;

        while (!is_token(parser, TKN_RPAREN) && !is_token(parser, TKN_EOF)) {
            TypeSpecParam* param = parse_typespec_func_param(parser);

            if (param) {
                num_params += 1;
                dllist_add(params.prev, &param->list);
            } else {
                bad_param = true;
                break;
            }

            if (!match_token_next(parser, TKN_COMMA)) {
                break;
            }
        }

        if (!bad_param && expect_token_next(parser, TKN_RPAREN, error_prefix)) {
            TypeSpec* ret = NULL;
            bool bad_ret = false;

            if (match_token_next(parser, TKN_ARROW)) {
                ret = parse_typespec(parser);
                bad_ret = ret == NULL;
            }

            if (!bad_ret) {
                range.end = parser->ptoken.range.end;
                type = typespec_func(parser->allocator, num_params, &params, ret, range);
            }
        }
    }

    return type;
}

// typespec_base  = typespec_func
//                | typespec_anon_struct
//                | typespec_anon_union
//                | TKN_IDENT
//                | '(' type_spec ')'
static TypeSpec* parse_typespec_base(Parser* parser)
{
    TypeSpec* type = NULL;

    if (is_keyword(parser, KW_FUNC)) {
        type = parse_typespec_func(parser);
    } else if (is_keyword(parser, KW_STRUCT)) {
        type = parse_typespec_anon_aggregate(parser, "Failed to parse anonymous struct", typespec_anon_struct);
    } else if (is_keyword(parser, KW_UNION)) {
        type = parse_typespec_anon_aggregate(parser, "Failed to parse anonymous union", typespec_anon_union);
    } else if (match_token_next(parser, TKN_IDENT)) {
        type = typespec_ident(parser->allocator, parser->ptoken.tident.value, parser->ptoken.range);
    } else if (match_token_next(parser, TKN_LPAREN)) {
        TypeSpec* enclosed_type = parse_typespec(parser);

        if (enclosed_type && expect_token_next(parser, TKN_RPAREN, NULL)) {
            type = enclosed_type;
        }
    } else {
        char tmp[32];

        print_token(&parser->token, tmp, sizeof(tmp));
        parser_on_error(parser, "Unexpected token `%s` in type specification", tmp);
    }

    return type;
}

// typespec = ('^' | '[' expr? ']' | KW_CONST) typespec
//          | typespec_base
TypeSpec* parse_typespec(Parser* parser)
{
    TypeSpec* type = NULL;

    if (match_token_next(parser, TKN_CARET)) {
        //
        // Pointer typespec.
        //

        ProgRange range = {.start = parser->ptoken.range.start};
        TypeSpec* base = parse_typespec(parser);

        if (base) {
            range.end = base->range.end;
            type = typespec_ptr(parser->allocator, base, range);
        }
    } else if (match_token_next(parser, TKN_LBRACKET)) {
        //
        // Array typespec.
        //

        ProgRange range = {.start = parser->ptoken.range.start};

        Expr* len = NULL;
        bool bad_len = false;

        if (!is_token(parser, TKN_RBRACKET)) {
            len = parse_expr(parser);
            bad_len = len == NULL;
        }

        if (!bad_len && expect_token_next(parser, TKN_RBRACKET, "Failed to parse array type")) {
            TypeSpec* base = parse_typespec(parser);

            if (base) {
                range.end = base->range.end;
                type = typespec_array(parser->allocator, base, len, range);
            }
        }
    } else if (match_keyword_next(parser, KW_CONST)) {
        //
        // Const typespec
        //

        ProgRange range = {.start = parser->ptoken.range.start};
        TypeSpec* base = parse_typespec(parser);

        if (base) {
            range.end = base->range.end;
            type = typespec_const(parser->allocator, base, range);
        }
    } else {
        type = parse_typespec_base(parser);
    }

    return type;
}

///////////////////////////////
//    Parse expressions
//////////////////////////////

static Expr* parse_expr_unary(Parser* parser);

static ExprInitializer* parse_expr_initializer(Parser* parser)
{
    ExprInitializer* initzer = NULL;
    ProgRange range = {.start = parser->token.range.start};
    const char* error_prefix = "Failed to parse initializer expression";

    if (match_token_next(parser, TKN_LBRACKET)) {
        Expr* index = parse_expr(parser);

        if (index && expect_token_next(parser, TKN_RBRACKET, error_prefix) &&
            expect_token_next(parser, TKN_ASSIGN, error_prefix)) {
            Expr* init = parse_expr(parser);

            if (init) {
                range.end = init->range.end;
                initzer = expr_index_initializer(parser->allocator, index, init, range);
            }
        }
    } else {
        Expr* expr = parse_expr(parser);

        if (expr && match_token_next(parser, TKN_ASSIGN)) {
            if (expr->kind == EXPR_IDENT) {
                // TODO: No need to allocate full expr for name. Consider resetting arena allocation state.
                const char* name = expr->eident.name;
                Expr* init = parse_expr(parser);

                if (init) {
                    range.end = init->range.end;
                    initzer = expr_name_initializer(parser->allocator, name, init, range);
                }
            } else {
                parser_on_error(parser, "Initializer name must be alphanumeric");
            }
        } else if (expr) {
            range.end = expr->range.end;
            initzer = expr_pos_initializer(parser->allocator, expr, range);
        }
    }

    return initzer;
}

// expr_compound_lit = '{' expr_init_list (':' typespec)? '}'
// expr_init_list = expr_init_item (',' expr_init_item)*
// expr_init_item = (TKN_IDENT '=')? expr
//               | ('[' (TKN_INT | TKN_IDENT) ']' '=')? expr
static Expr* parse_expr_compound_lit(Parser* parser)
{
    Expr* expr = NULL;
    const char* error_prefix = "Failed to parse compound literal expression";

    if (expect_token_next(parser, TKN_LBRACE, error_prefix)) {
        ProgRange range = {.start = parser->ptoken.range.start};
        size_t num_initzers = 0;
        DLList initzers = dllist_head_create(initzers);
        bool bad_initzer = false;

        while (!(is_token(parser, TKN_RBRACE) || is_token(parser, TKN_COLON) || is_token(parser, TKN_EOF))) {
            ExprInitializer* initzer = parse_expr_initializer(parser);

            if (initzer) {
                num_initzers += 1;
                dllist_add(initzers.prev, &initzer->list);
            } else {
                bad_initzer = true;
                break;
            }

            if (!match_token_next(parser, TKN_COMMA)) {
                break;
            }
            // TODO: Can record the maximum index value for array initializers
        }

        if (!bad_initzer) {
            TypeSpec* type = NULL;

            if (match_token_next(parser, TKN_COLON)) {
                type = parse_typespec(parser);
                // TODO: Can report error if using indexed initializers and not array type (etc.)

                if (type && expect_token_next(parser, TKN_RBRACE, error_prefix)) {
                    range.end = parser->ptoken.range.end;
                    expr = expr_compound_lit(parser->allocator, type, num_initzers, &initzers, range);
                }
            } else if (expect_token_next(parser, TKN_RBRACE, error_prefix)) {
                range.end = parser->ptoken.range.end;
                expr = expr_compound_lit(parser->allocator, type, num_initzers, &initzers, range);
            }
        }
    }

    return expr;
}

// expr_base = TKN_INT
//           | TKN_FLOAT
//           | TKN_STR
//           | TKN_IDENT
//           | expr_compound_init
//           | expr_sizeof
//           | expr_typeof
//           | '(' expr ')'
//
// expr_sizeof = '#' KW_SIZEOF '('type_spec')'
// expr_typeof = '#' KW_TYPEOF '(' expr ')'
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
    } else if (match_token_next(parser, TKN_LPAREN)) {
        Expr* enclosed = parse_expr(parser);

        if (enclosed && expect_token_next(parser, TKN_RPAREN, NULL)) {
            expr = enclosed;
        }
    } else if (is_token(parser, TKN_LBRACE)) {
        expr = parse_expr_compound_lit(parser);
    } else if (match_token_next(parser, TKN_POUND)) {
        ProgRange range = {.start = parser->ptoken.range.start};

        if (match_keyword_next(parser, KW_SIZEOF)) {
            const char* error_prefix = "Failed to parse #sizeof expression";

            if (expect_token_next(parser, TKN_LPAREN, error_prefix)) {
                TypeSpec* type = parse_typespec(parser);

                if (type && expect_token_next(parser, TKN_RPAREN, error_prefix)) {
                    range.end = parser->ptoken.range.end;
                    expr = expr_sizeof(parser->allocator, type, range);
                }
            }
        } else if (match_keyword_next(parser, KW_TYPEOF)) {
            const char* error_prefix = "Failed to parse #typeof expression";

            if (expect_token_next(parser, TKN_LPAREN, error_prefix)) {
                Expr* arg = parse_expr(parser);

                if (arg && expect_token_next(parser, TKN_RPAREN, error_prefix)) {
                    range.end = parser->ptoken.range.end;
                    expr = expr_typeof(parser->allocator, arg, range);
                }
            }
        } else {
            char tmp[32];

            print_token(&parser->token, tmp, sizeof(tmp));
            parser_on_error(parser, "Unexpected token `%s` after `#`", tmp);
        }
    } else if (match_token_next(parser, TKN_IDENT)) {
        Token* token = &parser->ptoken;
        expr = expr_ident(parser->allocator, token->tident.value, token->range);
    } else {
        char tmp[32];

        print_token(&parser->token, tmp, sizeof(tmp));
        parser_on_error(parser, "Unexpected token `%s` in expression", tmp);
    }

    return expr;
}

// expr_call_arg = (IDENTIFIER '=')? expr
static ExprCallArg* parse_expr_call_arg(Parser* parser)
{
    ExprCallArg* arg = NULL;
    Expr* expr = parse_expr(parser);

    if (expr && match_token_next(parser, TKN_ASSIGN)) {
        if (expr->kind == EXPR_IDENT) {
            const char* name = expr->eident.name; // TODO: No need to parse as expression here.

            expr = parse_expr(parser);

            if (expr) {
                arg = expr_call_arg(parser->allocator, expr, name);
            }
        } else {
            parser_on_error(parser, "Function argument's name must be an alphanumeric identifier");
        }
    } else if (expr) {
        arg = expr_call_arg(parser->allocator, expr, NULL);
    }

    return arg;
}

// expr_base_mod = expr_base ('.' IDENTIFIER | '[' expr ']' | '(' expr_call_arg_list* ')' | ':>' typespec)*
// expr_call_arg_list = expr_call_arg (',' expr_call_arg)*
static Expr* parse_expr_base_mod(Parser* parser)
{
    Expr* expr = parse_expr_base(parser);

    while (expr && (is_token(parser, TKN_DOT) || is_token(parser, TKN_LBRACKET) || is_token(parser, TKN_LPAREN) ||
                    is_token(parser, TKN_CAST))) {
        if (match_token_next(parser, TKN_DOT)) {
            //
            // Field access.
            //

            if (expect_token_next(parser, TKN_IDENT, "Failed to parse field access")) {
                const char* field = parser->ptoken.tident.value;
                ProgRange range = {.start = expr->range.start, .end = parser->ptoken.range.end};
                expr = expr_field(parser->allocator, expr, field, range);
            } else {
                expr = NULL;
            }
        } else if (match_token_next(parser, TKN_LBRACKET)) {
            //
            // Array index access.
            //

            Expr* index = parse_expr(parser);

            if (index && expect_token_next(parser, TKN_RBRACKET, "Failed to parse array access")) {
                ProgRange range = {.start = expr->range.start, .end = parser->ptoken.range.end};
                expr = expr_index(parser->allocator, expr, index, range);
            } else {
                expr = NULL;
            }
        } else if (match_token_next(parser, TKN_LPAREN)) {
            //
            // Function call.
            //

            bool bad_arg = false;
            size_t num_args = 0;
            DLList args = dllist_head_create(args);

            while (!is_token(parser, TKN_RPAREN) && !is_token(parser, TKN_EOF)) {
                ExprCallArg* arg = parse_expr_call_arg(parser);

                if (arg) {
                    num_args += 1;
                    dllist_add(args.prev, &arg->list);
                } else {
                    bad_arg = true;
                    break;
                }

                if (!match_token_next(parser, TKN_COMMA)) {
                    break;
                }
            }

            if (!bad_arg && expect_token_next(parser, TKN_RPAREN, "Failed to parse function call")) {
                ProgRange range = {.start = expr->range.start, .end = parser->ptoken.range.end};
                expr = expr_call(parser->allocator, expr, num_args, &args, range);
            } else {
                expr = NULL;
            }
        } else {
            //
            // Cast expression.
            //

            assert(is_token(parser, TKN_CAST));
            next_token(parser);

            TypeSpec* type = parse_typespec(parser);

            if (type) {
                ProgRange range = {.start = expr->range.start, .end = type->range.end};
                expr = expr_cast(parser->allocator, type, expr, range);
            } else {
                expr = NULL;
            }
        }
    }

    return expr;
}

// expr_unary = OP_PRECEDENCE_UNARY expr_unary
//            | expr_base_mod
static Expr* parse_expr_unary(Parser* parser)
{
    Expr* expr = NULL;

    if (is_token_op(parser, OP_PRECEDENCE_UNARY)) {
        ProgRange range = {.start = parser->token.range.start};
        TokenKind op = parser->token.kind;

        next_token(parser);

        Expr* unary = parse_expr_unary(parser);

        if (unary) {
            range.end = unary->range.end;
            expr = expr_unary(parser->allocator, op, unary, range);
        }
    } else {
        expr = parse_expr_base_mod(parser);
    }

    return expr;
}

// expr_mul = expr_unary (OP_PRECEDENCE_MUL expr_unary)*
static Expr* parse_expr_mul(Parser* parser)
{
    Expr* expr = parse_expr_unary(parser);

    while (expr && is_token_op(parser, OP_PRECEDENCE_MUL)) {
        TokenKind op = parser->token.kind;

        next_token(parser);

        Expr* left = expr;
        Expr* right = parse_expr_unary(parser);

        if (right) {
            expr = expr_binary(parser->allocator, op, left, right);
        } else {
            expr = NULL;
        }
    }

    return expr;
}

// expr_add = expr_mul (OP_PRECEDENCE_ADD expr_mul)*
static Expr* parse_expr_add(Parser* parser)
{
    Expr* expr = parse_expr_mul(parser);

    while (expr && is_token_op(parser, OP_PRECEDENCE_ADD)) {
        TokenKind op = parser->token.kind;

        next_token(parser);

        Expr* left = expr;
        Expr* right = parse_expr_mul(parser);

        if (right) {
            expr = expr_binary(parser->allocator, op, left, right);
        } else {
            expr = NULL;
        }
    }

    return expr;
}

// expr_cmp = expr_add (OP_PRECEDENCE_CMP expr_add)*
static Expr* parse_expr_cmp(Parser* parser)
{
    Expr* expr = parse_expr_add(parser);

    while (expr && is_token_op(parser, OP_PRECEDENCE_CMP)) {
        TokenKind op = parser->token.kind;

        next_token(parser);

        Expr* left = expr;
        Expr* right = parse_expr_add(parser);

        if (right) {
            expr = expr_binary(parser->allocator, op, left, right);
        } else {
            expr = NULL;
        }
    }

    return expr;
}

// expr_and = expr_cmp ('&&' expr_cmp)*
static Expr* parse_expr_and(Parser* parser)
{
    Expr* expr = parse_expr_cmp(parser);

    while (expr && match_token_next(parser, TKN_LOGIC_AND)) {
        Expr* left = expr;
        Expr* right = parse_expr_cmp(parser);

        if (right) {
            expr = expr_binary(parser->allocator, TKN_LOGIC_AND, left, right);
        } else {
            expr = NULL;
        }
    }

    return expr;
}

// expr_or = expr_and ('||' expr_and)*
static Expr* parse_expr_or(Parser* parser)
{
    Expr* expr = parse_expr_and(parser);

    while (expr && match_token_next(parser, TKN_LOGIC_OR)) {
        Expr* left = expr;
        Expr* right = parse_expr_and(parser);

        if (right) {
            expr = expr_binary(parser->allocator, TKN_LOGIC_OR, left, right);
        } else {
            expr = NULL;
        }
    }

    return expr;
}

// expr_ternary = expr_or ('?' expr_ternary ':' expr_ternary)?
static Expr* parse_expr_ternary(Parser* parser)
{
    Expr* expr = parse_expr_or(parser);

    if (expr && match_token_next(parser, TKN_QUESTION)) {
        Expr* then_expr = parse_expr_ternary(parser);

        if (then_expr) {
            if (expect_token_next(parser, TKN_COLON, "Failed to parse ternary operator expression")) {
                Expr* else_expr = parse_expr_ternary(parser);

                if (else_expr) {
                    expr = expr_ternary(parser->allocator, expr, then_expr, else_expr);
                }
            }
        }
    }

    return expr;
}

// expr = expr_ternary
Expr* parse_expr(Parser* parser)
{
    return parse_expr_ternary(parser);
}

///////////////////////////////
//    Parse declarations
//////////////////////////////

// decl_var = TKN_IDENT ':' type_spec? ('=' expr)? ';'
//
// Ex 1: x : int = 0;
// Ex 2: x := 0;
// Ex 3: x :int;
static Decl* parse_decl_var(Parser* parser)
{
    assert(is_token(parser, TKN_IDENT));
    Decl* decl = NULL;
    ProgRange range = {.start = parser->token.range.start};
    const char* name = parser->token.tident.value;
    const char* error_prefix = "Failed to parse variable declaration";

    next_token(parser);

    if (expect_token_next(parser, TKN_COLON, error_prefix)) {
        TypeSpec* type = NULL;
        Expr* expr = NULL;
        bool bad_type = false;
        bool bad_expr = false;

        if (!is_token(parser, TKN_ASSIGN) && !is_token(parser, TKN_SEMICOLON)) {
            type = parse_typespec(parser);
            bad_type = !type;
        }

        if (match_token_next(parser, TKN_ASSIGN)) {
            expr = parse_expr(parser);
            bad_expr = !expr;
        }

        if (!bad_type && !bad_expr) {
            if (type || expr) {
                if (expect_token_next(parser, TKN_SEMICOLON, error_prefix)) {
                    range.end = parser->ptoken.range.end;
                    decl = decl_var(parser->allocator, name, type, expr, range);
                }
            } else {
                parser_on_error(parser, "Variable declaration must have either a type or an initial value");
            }
        }
    }

    return decl;
}

// decl_union  = 'union' aggregate_body
// decl_struct = 'struct' aggregate_body
static Decl* parse_decl_aggregate(Parser* parser, const char* error_prefix, DeclAggregateFunc* decl_aggregate_fn)
{
    assert(is_keyword(parser, KW_STRUCT) || is_keyword(parser, KW_UNION));
    Decl* decl = NULL;
    ProgRange range = {.start = parser->token.range.start};

    next_token(parser);

    if (expect_token_next(parser, TKN_IDENT, error_prefix)) {
        const char* name = parser->ptoken.tident.value;

        if (expect_token_next(parser, TKN_LBRACE, error_prefix)) {
            AggregateBody body = {0};
            bool bad_field = parse_fill_aggregate_body(parser, &body);

            if (!bad_field && expect_token_next(parser, TKN_RBRACE, error_prefix)) {
                if (body.num_fields) {
                    range.end = parser->ptoken.range.end;
                    decl = decl_aggregate_fn(parser->allocator, name, body.num_fields, &body.fields, range);
                } else {
                    parser_on_error(parser, "%s: must have at least one field", error_prefix);
                }
            }
        }
    }

    return decl;
}

// decl_enum_item  = TKN_IDENT ('=' expr)?
static DeclEnumItem* parse_decl_enum_item(Parser* parser)
{
    DeclEnumItem* item = NULL;

    if (expect_token_next(parser, TKN_IDENT, "Failed to parse enum value")) {
        const char* name = parser->ptoken.tident.value;
        Expr* value = NULL;
        bool bad_value = false;

        if (match_token_next(parser, TKN_ASSIGN)) {
            value = parse_expr(parser);
            bad_value = !value;
        }

        if (!bad_value) {
            item = decl_enum_item(parser->allocator, name, value);
        }
    }

    return item;
}

// decl_enum  = 'enum' TKN_IDENT (':' typespec)? '{' decl_enum_items? '}'
// decl_enum_items = decl_enum_item (',' decl_enum_item)* ','?
//
// Ex 1: enum TokenKind :uint { TKN_NONE = 0, TKN_EOF, }
// Ex 2: enum TokenKind { TKN_NONE, TKN_EOF }
// Ex 3: enum TokenKind {}
static Decl* parse_decl_enum(Parser* parser)
{
    assert(is_keyword(parser, KW_ENUM));
    Decl* decl = NULL;
    ProgRange range = {.start = parser->token.range.start};
    const char* error_prefix = "Failed to parse enum declaration";

    next_token(parser);

    if (expect_token_next(parser, TKN_IDENT, error_prefix)) {
        const char* name = parser->ptoken.tident.value;
        TypeSpec* type = NULL;
        bool bad_type = false;

        if (match_token_next(parser, TKN_COLON)) {
            type = parse_typespec(parser);
            bad_type = !type;
        }

        if (!bad_type && expect_token_next(parser, TKN_LBRACE, error_prefix)) {
            size_t num_items = 0;
            DLList items = dllist_head_create(items);
            bool bad_item = false;

            while (!is_token(parser, TKN_RBRACE) && !is_token(parser, TKN_EOF)) {
                DeclEnumItem* item = parse_decl_enum_item(parser);

                if (item) {
                    num_items += 1;
                    dllist_add(items.prev, &item->list);
                } else {
                    bad_item = true;
                    break;
                }

                if (!match_token_next(parser, TKN_COMMA)) {
                    break;
                }
            }

            if (!bad_item && expect_token_next(parser, TKN_RBRACE, error_prefix)) {
                if (num_items) {
                    range.end = parser->ptoken.range.end;
                    decl = decl_enum(parser->allocator, name, type, num_items, &items, range);
                } else {
                    parser_on_error(parser, "%s: must have at least one enumeration constant", error_prefix);
                }
            }
        }
    }

    return decl;
}

// decl_typedef = KW_TYPEDEF TKN_IDENT '=' type_spec ';'
//
// Ex: #typedef i8 = int8;
static Decl* parse_decl_typedef(Parser* parser)
{
    assert(is_keyword(parser, KW_TYPEDEF));
    Decl* decl = NULL;
    ProgRange range = {.start = parser->ptoken.range.start};
    const char* error_prefix = "Failed to parse #typedef declaration";

    next_token(parser);

    if (expect_token_next(parser, TKN_IDENT, error_prefix)) {
        const char* name = parser->ptoken.tident.value;

        if (expect_token_next(parser, TKN_ASSIGN, error_prefix)) {
            TypeSpec* type = parse_typespec(parser);

            if (type && expect_token_next(parser, TKN_SEMICOLON, error_prefix)) {
                range.end = parser->ptoken.range.end;
                decl = decl_typedef(parser->allocator, name, type, range);
            }
        }
    }

    return decl;
}

// decl_const = KW_CONST TKN_IDENT ':' typespec?  '=' expr ';'
//
// Ex 1: #const x : int = 0;
// Ex 2: #const x := 0;
static Decl* parse_decl_const(Parser* parser)
{
    assert(is_keyword(parser, KW_CONST));
    Decl* decl = NULL;
    ProgRange range = {.start = parser->ptoken.range.start};
    const char* error_prefix = "Failed to parse #const declaration";

    next_token(parser);

    if (expect_token_next(parser, TKN_IDENT, error_prefix)) {
        const char* name = parser->ptoken.tident.value;

        if (expect_token_next(parser, TKN_COLON, error_prefix)) {
            TypeSpec* type = NULL;
            bool bad_type = false;

            if (!is_token(parser, TKN_ASSIGN)) {
                type = parse_typespec(parser);
                bad_type = type == NULL;
            }

            if (!bad_type && expect_token_next(parser, TKN_ASSIGN, error_prefix)) {
                Expr* expr = parse_expr(parser);

                if (expr && expect_token_next(parser, TKN_SEMICOLON, error_prefix)) {
                    range.end = parser->ptoken.range.end;
                    decl = decl_const(parser->allocator, name, type, expr, range);
                }
            }
        }
    }

    return decl;
}

// decl = '#' decl_const
//      | '#' decl_typedef
//      | decl_enum
//      | decl_struct
//      | decl_union
//      | decl_func
//      | decl_var
Decl* parse_decl(Parser* parser)
{
    Decl* decl = NULL;

    if (match_token_next(parser, TKN_POUND)) {
        if (is_keyword(parser, KW_CONST)) {
            decl = parse_decl_const(parser);
        } else if (is_keyword(parser, KW_TYPEDEF)) {
            decl = parse_decl_typedef(parser);
        } else {
            char tmp[32];

            print_token(&parser->token, tmp, sizeof(tmp));
            parser_on_error(parser, "Unexpected token `%s` in `#` declaration", tmp);
        }
    } else if (is_keyword(parser, KW_ENUM)) {
        decl = parse_decl_enum(parser);
    } else if (is_keyword(parser, KW_STRUCT)) {
        decl = parse_decl_aggregate(parser, "Failed to parse struct declaration", decl_struct);
    } else if (is_keyword(parser, KW_UNION)) {
        decl = parse_decl_aggregate(parser, "Failed to parse union declaration", decl_union);
    } else if (is_token(parser, TKN_IDENT)) {
        decl = parse_decl_var(parser);
    } else {
        char tmp[32];

        print_token(&parser->token, tmp, sizeof(tmp));
        parser_on_error(parser, "Unexpected token `%s` in declaration", tmp);

        // NOTE: Not sure if this is right. Might want to skip to first of newline or semicolon?
        // Alternatively, just skip semicolon if it is the next token???
        // skip_after_token(parser, TKN_SEMICOLON);
    }

    return decl;
}
