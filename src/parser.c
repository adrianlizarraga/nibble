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

bool is_token_kind(Parser* parser, TokenKind kind)
{
    return (parser->token.kind == kind);
}

static const uint8_t token_kind_props[TKN_KIND_COUNT] = {
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

    [TKN_ASSIGN] = OP_ASSIGN,
    [TKN_ADD_ASSIGN] = OP_ASSIGN,
    [TKN_SUB_ASSIGN] = OP_ASSIGN,
    [TKN_MUL_ASSIGN] = OP_ASSIGN,
    [TKN_DIV_ASSIGN] = OP_ASSIGN,
    [TKN_AND_ASSIGN] = OP_ASSIGN,
    [TKN_OR_ASSIGN] = OP_ASSIGN,
    [TKN_XOR_ASSIGN] = OP_ASSIGN,
    [TKN_MOD_ASSIGN] = OP_ASSIGN,
};

bool is_token_prop_kind(Parser* parser, uint8_t props)
{
    TokenKind kind = parser->token.kind;

    return (token_kind_props[kind] & props);
}

bool is_keyword(Parser* parser, Keyword kw)
{
    return (parser->token.kind == TKN_KW) && (parser->token.as_kw.kw == kw);
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
    bool matches = (parser->token.kind == TKN_KW) && (parser->token.as_kw.kw == kw);

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
    bool matches = (parser->token.kind == TKN_KW) && (parser->token.as_kw.kw == kw);

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
    while (!is_token_kind(parser, TKN_EOF) && !is_token_kind(parser, kind)) {
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
        const char* name = parser->ptoken.as_ident.value;
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
static bool parse_fill_aggregate_body(Parser* parser, size_t* num_fields, DLList* fields)
{
    bool bad_field = false;

    dllist_head_init(fields);
    *num_fields = 0;

    while (!is_token_kind(parser, TKN_RBRACE) && !is_token_kind(parser, TKN_EOF)) {
        AggregateField* field = parse_aggregate_field(parser);

        if (field) {
            *num_fields += 1;
            dllist_add(fields->prev, &field->list);
        } else {
            bad_field = true;
            break;
        }
    }

    return bad_field;
}

static TypeSpec* parse_typespec_aggregate(Parser* parser, const char* error_prefix,
                                          TypeSpecAggregateProc* typespec_aggregate_proc)
{
    assert(is_keyword(parser, KW_STRUCT) || is_keyword(parser, KW_UNION));

    TypeSpec* type = NULL;
    ProgRange range = {.start = parser->token.range.start};

    next_token(parser);

    if (expect_token_next(parser, TKN_LBRACE, error_prefix)) {
        size_t num_fields = 0;
        DLList fields = {0};
        bool bad_field = parse_fill_aggregate_body(parser, &num_fields, &fields);

        if (!bad_field && expect_token_next(parser, TKN_RBRACE, error_prefix)) {
            if (num_fields) {
                range.end = parser->ptoken.range.end;
                type = typespec_aggregate_proc(parser->allocator, num_fields, &fields, range);
            } else {
                parser_on_error(parser, "%s: must have at least one field", error_prefix);
            }
        }
    }

    return type;
}

// typespec_proc_param = (name ':')? typespec
static ProcParam* parse_typespec_proc_param(Parser* parser)
{
    ProcParam* param = NULL;
    TypeSpec* type = parse_typespec(parser);

    if (type && match_token_next(parser, TKN_COLON)) {
        if (type->kind == AST_TypeSpecIdent) { // NOTE: I wish this was truly LL1
            ProgRange range = {.start = type->range.start};
            const char* name = ((TypeSpecIdent*)type)->name;

            mem_free(parser->allocator, type);

            type = parse_typespec(parser);

            if (type) {
                range.end = type->range.end;
                param = proc_param(parser->allocator, name, type, range);
            }
        } else {
            parser_on_error(parser, "Parameter's name must be an alphanumeric identifier");
        }
    } else if (type) {
        param = proc_param(parser->allocator, NULL, type, type->range);
    }

    return param;
}

// typespec_proc = 'proc' '(' proc_param_list? ')' ('=>' typespec)?
//
// proc_param_list = typespec_proc_param (',' typespec_proc_param)*
static TypeSpec* parse_typespec_proc(Parser* parser)
{
    assert(is_keyword(parser, KW_PROC));
    TypeSpec* type = NULL;
    ProgRange range = {.start = parser->token.range.start};
    const char* error_prefix = "Failed to parse procedure type specification";

    next_token(parser);

    if (expect_token_next(parser, TKN_LPAREN, error_prefix)) {
        size_t num_params = 0;
        DLList params = dllist_head_create(params);
        bool bad_param = false;

        while (!is_token_kind(parser, TKN_RPAREN) && !is_token_kind(parser, TKN_EOF)) {
            ProcParam* param = parse_typespec_proc_param(parser);

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
                type = typespec_proc(parser->allocator, num_params, &params, ret, range);
            }
        }
    }

    return type;
}

// typespec_base  = typespec_proc
//                | typespec_anon_struct
//                | typespec_anon_union
//                | TKN_IDENT
//                | '(' type_spec ')'
static TypeSpec* parse_typespec_base(Parser* parser)
{
    Token token = parser->token;

    switch (token.kind) {
    case TKN_KW: {
        switch (token.as_kw.kw) {
        case KW_PROC:
            return parse_typespec_proc(parser);
        case KW_STRUCT:
            return parse_typespec_aggregate(parser, "Failed to parse anonymous struct", typespec_struct);
        case KW_UNION:
            return parse_typespec_aggregate(parser, "Failed to parse anonymous union", typespec_union);
        default:
            break;
        }
    } break;
    case TKN_IDENT:
        next_token(parser);
        return typespec_ident(parser->allocator, token.as_ident.value, token.range);
    case TKN_LPAREN: {
        next_token(parser);

        TypeSpec* type = NULL;
        TypeSpec* enclosed_type = parse_typespec(parser);

        if (enclosed_type && expect_token_next(parser, TKN_RPAREN, NULL)) {
            type = enclosed_type;
        }

        return type;
    } break;
    default:
        break;
    }

    // If we got here, we have an unexpected token.
    char tmp[32];

    print_token(&parser->token, tmp, sizeof(tmp));
    parser_on_error(parser, "Unexpected token `%s` in type specification", tmp);

    return NULL;
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

        if (!is_token_kind(parser, TKN_RBRACKET)) {
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

static MemberInitializer* parse_member_initializer(Parser* parser)
{
    MemberInitializer* initzer = NULL;
    ProgRange range = {.start = parser->token.range.start};
    Designator designator = {0};
    const char* error_prefix = "Failed to parse initializer expression";

    if (match_token_next(parser, TKN_LBRACKET)) {
        Expr* index = parse_expr(parser);

        if (index && expect_token_next(parser, TKN_RBRACKET, error_prefix) &&
            expect_token_next(parser, TKN_ASSIGN, error_prefix)) {
            Expr* init = parse_expr(parser);

            if (init) {
                range.end = init->range.end;
                designator.kind = DESIGNATOR_INDEX;
                designator.index = index;
                initzer = member_initializer(parser->allocator, init, designator, range);
            }
        }
    } else {
        Expr* expr = parse_expr(parser);

        if (expr && match_token_next(parser, TKN_ASSIGN)) {
            if (expr->kind == AST_ExprIdent) {
                const char* name = ((ExprIdent*)expr)->name;

                mem_free(parser->allocator, expr);

                Expr* init = parse_expr(parser);

                if (init) {
                    range.end = init->range.end;
                    designator.kind = DESIGNATOR_NAME;
                    designator.name = name;
                    initzer = member_initializer(parser->allocator, init, designator, range);
                }
            } else {
                parser_on_error(parser, "Initializer designator name must be alphanumeric");
            }
        } else if (expr) {
            range.end = expr->range.end;
            initzer = member_initializer(parser->allocator, expr, designator, range);
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

        while (!(is_token_kind(parser, TKN_RBRACE) || is_token_kind(parser, TKN_COLON) ||
                 is_token_kind(parser, TKN_EOF))) {
            MemberInitializer* initzer = parse_member_initializer(parser);

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

static Expr* parse_expr_sizeof(Parser* parser)
{
    Expr* expr = NULL;
    const char* error_prefix = "Failed to parse sizeof expression";

    if (expect_keyword_next(parser, KW_SIZEOF, error_prefix)) {
        ProgRange range = {.start = parser->ptoken.range.start};

        if (expect_token_next(parser, TKN_LPAREN, error_prefix)) {
            TypeSpec* type = parse_typespec(parser);

            if (type && expect_token_next(parser, TKN_RPAREN, error_prefix)) {
                range.end = parser->ptoken.range.end;
                expr = expr_sizeof(parser->allocator, type, range);
            }
        }
    }

    return expr;
}

static Expr* parse_expr_typeof(Parser* parser)
{
    Expr* expr = NULL;
    const char* error_prefix = "Failed to parse typeof expression";

    if (expect_keyword_next(parser, KW_TYPEOF, error_prefix)) {
        ProgRange range = {.start = parser->ptoken.range.start};

        if (expect_token_next(parser, TKN_LPAREN, error_prefix)) {
            Expr* arg = parse_expr(parser);

            if (arg && expect_token_next(parser, TKN_RPAREN, error_prefix)) {
                range.end = parser->ptoken.range.end;
                expr = expr_typeof(parser->allocator, arg, range);
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
// expr_sizeof = KW_SIZEOF '('type_spec')'
// expr_typeof = KW_TYPEOF '(' expr ')'
static Expr* parse_expr_base(Parser* parser)
{
    Token token = parser->token;

    switch (token.kind) {
    case TKN_INT:
        next_token(parser);
        return expr_int(parser->allocator, token.as_int.value, token.range);
    case TKN_FLOAT:
        next_token(parser);
        return expr_float(parser->allocator, token.as_float.value, token.range);
    case TKN_STR:
        next_token(parser);
        return expr_str(parser->allocator, token.as_str.value, token.range);
    case TKN_LPAREN: {
        next_token(parser);

        Expr* enclosed = parse_expr(parser);

        if (enclosed && expect_token_next(parser, TKN_RPAREN, NULL)) {
            return enclosed;
        }
    } break;
    case TKN_LBRACE:
        return parse_expr_compound_lit(parser);
    case TKN_KW: {
        switch (token.as_kw.kw) {
        case KW_SIZEOF:
            return parse_expr_sizeof(parser);
        case KW_TYPEOF:
            return parse_expr_typeof(parser);
        default:
            break;
        }
    } break;
    case TKN_IDENT:
        next_token(parser);
        return expr_ident(parser->allocator, token.as_ident.value, token.range);
    default:
        break;
    }

    // If we got here, we have an unexpected token.
    char tmp[32];

    print_token(&parser->token, tmp, sizeof(tmp));
    parser_on_error(parser, "Unexpected token `%s` in expression", tmp);

    return NULL;
}

// proc_call_arg = (IDENTIFIER '=')? expr
static ProcCallArg* parse_proc_call_arg(Parser* parser)
{
    ProcCallArg* arg = NULL;
    Expr* expr = parse_expr(parser);

    if (expr && match_token_next(parser, TKN_ASSIGN)) {
        if (expr->kind == AST_ExprIdent) {
            const char* name = ((ExprIdent*)expr)->name;

            mem_free(parser->allocator, expr);

            expr = parse_expr(parser);

            if (expr) {
                arg = proc_call_arg(parser->allocator, expr, name);
            }
        } else {
            parser_on_error(parser, "Procedure argument's name must be an alphanumeric identifier");
        }
    } else if (expr) {
        arg = proc_call_arg(parser->allocator, expr, NULL);
    }

    return arg;
}

// expr_base_mod = expr_base ('.' IDENTIFIER | '[' expr ']' | '(' proc_call_arg_list* ')' | ':>' typespec)*
// proc_call_arg_list = proc_call_arg (',' proc_call_arg)*
static Expr* parse_expr_base_mod(Parser* parser)
{
    Expr* expr = parse_expr_base(parser);

    while (expr && (is_token_kind(parser, TKN_DOT) || is_token_kind(parser, TKN_LBRACKET) ||
                    is_token_kind(parser, TKN_LPAREN) || is_token_kind(parser, TKN_CAST))) {
        if (match_token_next(parser, TKN_DOT)) {
            //
            // Field access.
            //

            if (expect_token_next(parser, TKN_IDENT, "Failed to parse field access")) {
                const char* field = parser->ptoken.as_ident.value;
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
            // Procedure call.
            //

            bool bad_arg = false;
            size_t num_args = 0;
            DLList args = dllist_head_create(args);

            while (!is_token_kind(parser, TKN_RPAREN) && !is_token_kind(parser, TKN_EOF)) {
                ProcCallArg* arg = parse_proc_call_arg(parser);

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

            if (!bad_arg && expect_token_next(parser, TKN_RPAREN, "Failed to parse procedure call")) {
                ProgRange range = {.start = expr->range.start, .end = parser->ptoken.range.end};
                expr = expr_call(parser->allocator, expr, num_args, &args, range);
            } else {
                expr = NULL;
            }
        } else {
            //
            // Cast expression.
            //

            assert(is_token_kind(parser, TKN_CAST));
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

    if (is_token_prop_kind(parser, OP_PRECEDENCE_UNARY)) {
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

    while (expr && is_token_prop_kind(parser, OP_PRECEDENCE_MUL)) {
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

    while (expr && is_token_prop_kind(parser, OP_PRECEDENCE_ADD)) {
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

    while (expr && is_token_prop_kind(parser, OP_PRECEDENCE_CMP)) {
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
//    Parse statements
//////////////////////////////

static bool parse_fill_stmt_list(Parser* parser, size_t* num_stmts, DLList* stmts)
{
    bool bad_stmt = false;

    dllist_head_init(stmts);
    *num_stmts = 0;

    while (!is_token_kind(parser, TKN_RBRACE) && !is_token_kind(parser, TKN_EOF)) {
        Stmt* stmt = parse_stmt(parser);

        if (stmt) {
            *num_stmts += 1;
            dllist_add(stmts->prev, &stmt->list);
        } else {
            bad_stmt = true;
            break;
        }
    }

    return !bad_stmt;
}

static Stmt* parse_stmt_block(Parser* parser)
{
    assert(is_token_kind(parser, TKN_LBRACE));
    Stmt* stmt = NULL;
    ProgRange range = {.start = parser->token.range.start};

    next_token(parser);

    size_t num_stmts = 0;
    DLList stmts = {0};
    bool ok = parse_fill_stmt_list(parser, &num_stmts, &stmts);

    if (ok && expect_token_next(parser, TKN_RBRACE, "Failed to parse end of statement block")) {
        range.end = parser->ptoken.range.end;
        stmt = stmt_block(parser->allocator, num_stmts, &stmts, range);
    }

    return stmt;
}

static bool parse_fill_if_cond_block(Parser* parser, IfCondBlock* cblock, const char* error_prefix)
{
    cblock->range.start = parser->token.range.start;

    if (expect_token_next(parser, TKN_LPAREN, error_prefix)) {
        cblock->cond = parse_expr(parser);

        if (cblock->cond && expect_token_next(parser, TKN_RPAREN, error_prefix) &&
            expect_token_next(parser, TKN_LBRACE, error_prefix)) {
            bool ok = parse_fill_stmt_list(parser, &cblock->num_stmts, &cblock->stmts);

            if (ok && expect_token_next(parser, TKN_RBRACE, error_prefix)) {
                cblock->range.end = parser->ptoken.range.end;

                return true;
            }
        }
    }

    return false;
}

static ElifBlock* parse_stmt_elif_block(Parser* parser)
{
    assert(is_keyword(parser, KW_ELIF));
    ElifBlock* elif_blk = NULL;
    const char* error_prefix = "Failed to parse elif statement";

    next_token(parser);

    IfCondBlock cblock = {0};
    bool ok = parse_fill_if_cond_block(parser, &cblock, error_prefix);

    if (ok) {
        elif_blk = elif_block(parser->allocator, cblock.cond, cblock.num_stmts, &cblock.stmts, cblock.range);
    }

    return elif_blk;
}

static bool parse_fill_else_block(Parser* parser, ElseBlock* else_blk)
{
    assert(is_keyword(parser, KW_ELSE));
    const char* error_prefix = "Failed to parse else statement";

    else_blk->range.start = parser->token.range.start;

    next_token(parser);

    if (expect_token_next(parser, TKN_LBRACE, error_prefix)) {
        bool filled_stmts = parse_fill_stmt_list(parser, &else_blk->num_stmts, &else_blk->stmts);

        if (filled_stmts && expect_token_next(parser, TKN_RBRACE, error_prefix)) {
            else_blk->range.end = parser->ptoken.range.end;

            return true;
        }
    }

    return false;
}

// stmt_if = 'if' '(' expr ')' stmt_block ('elif' '(' expr ')' stmt_block)* ('else' stmt_block)?
static Stmt* parse_stmt_if(Parser* parser)
{
    assert(is_keyword(parser, KW_IF));
    Stmt* stmt = NULL;
    ProgRange range = {.start = parser->token.range.start};
    const char* err_pre = "Failed to parse if statement";

    next_token(parser);

    IfCondBlock if_blk = {0};
    bool ok_if = parse_fill_if_cond_block(parser, &if_blk, err_pre);

    if (ok_if) {
        size_t num_elif_blks = 0;
        DLList elif_blks = dllist_head_create(elif_blks);
        bool bad_elif = false;

        while (is_keyword(parser, KW_ELIF)) {
            ElifBlock* elif = parse_stmt_elif_block(parser);

            if (elif) {
                num_elif_blks += 1;
                dllist_add(elif_blks.prev, &elif->list);
            } else {
                bad_elif = true;
                break;
            }
        }

        if (!bad_elif) {
            bool bad_else = false;
            ElseBlock else_blk = {0};

            dllist_head_init(&else_blk.stmts);

            if (is_keyword(parser, KW_ELSE)) {
                bad_else = !parse_fill_else_block(parser, &else_blk);
            }

            if (!bad_else) {
                stmt = stmt_if(parser->allocator, &if_blk, num_elif_blks, &elif_blks, &else_blk, range);
            }
        }
    }

    return stmt;
}

// stmt_while = 'while' '(' expr ')' stmt_block
static Stmt* parse_stmt_while(Parser* parser)
{
    assert(is_keyword(parser, KW_WHILE));
    Stmt* stmt = NULL;
    ProgRange range = {.start = parser->token.range.start};
    const char* error_prefix = "Failed to parse while-loop statement";

    next_token(parser);

    if (expect_token_next(parser, TKN_LPAREN, error_prefix)) {
        Expr* cond = parse_expr(parser);

        if (cond && expect_token_next(parser, TKN_RPAREN, error_prefix) &&
            expect_token_next(parser, TKN_LBRACE, error_prefix)) {
            DLList stmts = {0};
            size_t num_stmts = 0;
            bool ok = parse_fill_stmt_list(parser, &num_stmts, &stmts);

            if (ok && expect_token_next(parser, TKN_RBRACE, error_prefix)) {
                range.end = parser->ptoken.range.end;
                stmt = stmt_while(parser->allocator, cond, num_stmts, &stmts, range);
            }
        }
    }

    return stmt;
}

// stmt_do_while = 'do' stmt_block 'while' '(' expr ')' ';'
static Stmt* parse_stmt_do_while(Parser* parser)
{
    assert(is_keyword(parser, KW_DO));
    Stmt* stmt = NULL;
    ProgRange range = {.start = parser->token.range.start};
    const char* error_prefix = "Failed to parse do-while-loop statement";

    next_token(parser);

    if (expect_token_next(parser, TKN_LBRACE, error_prefix)) {
        DLList stmts = {0};
        size_t num_stmts = 0;
        bool ok = parse_fill_stmt_list(parser, &num_stmts, &stmts);

        if (ok && expect_token_next(parser, TKN_RBRACE, error_prefix) &&
            expect_keyword_next(parser, KW_WHILE, error_prefix) &&
            expect_token_next(parser, TKN_LPAREN, error_prefix)) {
            Expr* cond = parse_expr(parser);

            if (cond && expect_token_next(parser, TKN_RPAREN, error_prefix) &&
                expect_token_next(parser, TKN_SEMICOLON, error_prefix)) {
                range.end = parser->ptoken.range.end;
                stmt = stmt_do_while(parser->allocator, cond, num_stmts, &stmts, range);
            }
        }
    }

    return stmt;
}

static Stmt* parse_stmt_decl(Parser* parser)
{
    Stmt* stmt = NULL;
    Decl* decl = parse_decl(parser);

    if (decl) {
        stmt = stmt_decl(parser->allocator, decl);
    }

    return stmt;
}

static Stmt* parse_stmt_expr(Parser* parser)
{
    Stmt* stmt = NULL;
    Expr* expr = parse_expr(parser);
    ProgRange range = {.start = expr->range.start};
    const char* error_prefix = "Failed to parse expression statement";

    if (expr) {
        if (match_token_next(parser, TKN_SEMICOLON)) {
            range.end = parser->ptoken.range.end;
            stmt = stmt_expr(parser->allocator, expr, range);
        } else if (is_token_prop_kind(parser, OP_ASSIGN)) {
            TokenKind op_assign = parser->token.kind;

            next_token(parser);

            Expr* rexpr = parse_expr(parser);

            if (rexpr && expect_token_next(parser, TKN_SEMICOLON, error_prefix)) {
                range.end = parser->ptoken.range.end;
                stmt = stmt_expr_assign(parser->allocator, expr, op_assign, rexpr, range);
            }
        }
    }

    return stmt;
}

// stmt = 'if' '(' expr ')' stmt_block ('elif' '(' expr ')' stmt_block)* ('else' stmt_block)?
//      | stmt_while
//      | stmt_do_while
//      | 'for' '(' (decl_var | expr | expr_assign)? ';' expr? ';' (expr | expr_assign)? ')' stmt_block
//      | 'switch' '(' expr ')' '{' case_item* '}'
//      | 'return' expr? ';'
//      | 'break' TKN_IDENT? ';'
//      | 'continue' TKN_IDENT? ';'
//      | 'goto' TKN_IDENT ';'
//      | 'label' TKN_IDENT ':'
//      | decl
//      | stmt_block
//      | expr ';'
//      | expr_assign ';'
Stmt* parse_stmt(Parser* parser)
{
    Token token = parser->token;

    switch (token.kind) {
    case TKN_LBRACE:
        return parse_stmt_block(parser);
    case TKN_KW: {
        switch (token.as_kw.kw) {
        case KW_IF:
            return parse_stmt_if(parser);
        case KW_WHILE:
            return parse_stmt_while(parser);
        case KW_DO:
            return parse_stmt_do_while(parser);
        case KW_FOR:
            break;
        case KW_SWITCH:
            break;
        case KW_RETURN:
            break;
        case KW_BREAK:
            break;
        case KW_CONTINUE:
            break;
        case KW_GOTO:
            break;
        case KW_LABEL:
            break;
        default:
            return parse_stmt_decl(parser);
        }
    } break;
    case TKN_IDENT:
        return parse_stmt_expr(parser);
    default:
        break;
    }

    // If we got here, we have an unexpected token.
    char tmp[32];

    print_token(&parser->token, tmp, sizeof(tmp));
    parser_on_error(parser, "Unexpected token `%s` while parsing a statement", tmp);

    return NULL;
}

///////////////////////////////
//    Parse declarations
//////////////////////////////

// decl_var = KW_VAR TKN_IDENT ':' type_spec? ('=' expr)? ';'
//
// Ex 1: var x : int = 0;
// Ex 2: var x := 0;
// Ex 3: var x :int;
static Decl* parse_decl_var(Parser* parser)
{
    assert(is_keyword(parser, KW_VAR));
    Decl* decl = NULL;
    ProgRange range = {.start = parser->token.range.start};
    const char* error_prefix = "Failed to parse var declaration";

    next_token(parser);

    if (expect_token_next(parser, TKN_IDENT, error_prefix)) {
        const char* name = parser->ptoken.as_ident.value;

        if (expect_token_next(parser, TKN_COLON, error_prefix)) {
            TypeSpec* type = NULL;
            Expr* expr = NULL;
            bool bad_type = false;
            bool bad_expr = false;

            if (!is_token_kind(parser, TKN_ASSIGN) && !is_token_kind(parser, TKN_SEMICOLON)) {
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
                    parser_on_error(parser, "A var declaration must have either a type or an initial value");
                }
            }
        }
    }

    return decl;
}

// proc_param = TKN_IDENT ':' type_spec
static ProcParam* parse_proc_param(Parser* parser)
{
    ProcParam* param = NULL;
    const char* error_prefix = "Failed to parse procedure parameter";

    if (expect_token_next(parser, TKN_IDENT, error_prefix)) {
        const char* name = parser->ptoken.as_ident.value;
        ProgRange range = {.start = parser->ptoken.range.start};

        if (expect_token_next(parser, TKN_COLON, error_prefix)) {
            TypeSpec* type = parse_typespec(parser);

            if (type) {
                range.end = type->range.end;
                param = proc_param(parser->allocator, name, type, range);
            }
        }
    }

    return param;
}

// decl_proc  = 'proc' TKN_IDENT '(' param_list ')' ('=>' typespec)? stmt_block
// proc_param_list = proc_param (',' proc_param)*
static Decl* parse_decl_proc(Parser* parser)
{
    assert(is_keyword(parser, KW_PROC));
    Decl* decl = NULL;
    ProgRange range = {.start = parser->token.range.start};
    const char* error_prefix = "Failed to parse procedure declaration";

    next_token(parser);

    if (expect_token_next(parser, TKN_IDENT, error_prefix)) {
        const char* name = parser->ptoken.as_ident.value;

        if (expect_token_next(parser, TKN_LPAREN, error_prefix)) {
            size_t num_params = 0;
            DLList params = dllist_head_create(params);
            bool bad_param = false;

            while (!is_token_kind(parser, TKN_RPAREN) && !is_token_kind(parser, TKN_EOF)) {
                ProcParam* param = parse_proc_param(parser);

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
                    bad_ret = !ret;
                }

                if (!bad_ret && expect_token_next(parser, TKN_LBRACE, error_prefix)) {
                    DLList stmts = {0};
                    size_t num_stmts = 0;
                    bool ok = parse_fill_stmt_list(parser, &num_stmts, &stmts);

                    if (ok && expect_token_next(parser, TKN_RBRACE, error_prefix)) {
                        range.end = parser->ptoken.range.end;
                        decl = decl_proc(parser->allocator, name, num_params, &params, ret, num_stmts, &stmts, range);
                    }
                }
            }
        }
    }

    return decl;
}

// decl_union  = 'union' aggregate_body
// decl_struct = 'struct' aggregate_body
static Decl* parse_decl_aggregate(Parser* parser, const char* error_prefix, DeclAggregateProc* decl_aggregate_proc)
{
    assert(is_keyword(parser, KW_STRUCT) || is_keyword(parser, KW_UNION));
    Decl* decl = NULL;
    ProgRange range = {.start = parser->token.range.start};

    next_token(parser);

    if (expect_token_next(parser, TKN_IDENT, error_prefix)) {
        const char* name = parser->ptoken.as_ident.value;

        if (expect_token_next(parser, TKN_LBRACE, error_prefix)) {
            size_t num_fields = 0;
            DLList fields = {0};
            bool bad_field = parse_fill_aggregate_body(parser, &num_fields, &fields);

            if (!bad_field && expect_token_next(parser, TKN_RBRACE, error_prefix)) {
                if (num_fields) {
                    range.end = parser->ptoken.range.end;
                    decl = decl_aggregate_proc(parser->allocator, name, num_fields, &fields, range);
                } else {
                    parser_on_error(parser, "%s: must have at least one field", error_prefix);
                }
            }
        }
    }

    return decl;
}

// enum_item  = TKN_IDENT ('=' expr)?
static EnumItem* parse_enum_item(Parser* parser)
{
    EnumItem* item = NULL;

    if (expect_token_next(parser, TKN_IDENT, "Failed to parse enum value")) {
        const char* name = parser->ptoken.as_ident.value;
        Expr* value = NULL;
        bool bad_value = false;

        if (match_token_next(parser, TKN_ASSIGN)) {
            value = parse_expr(parser);
            bad_value = !value;
        }

        if (!bad_value) {
            item = enum_item(parser->allocator, name, value);
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
        const char* name = parser->ptoken.as_ident.value;
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

            while (!is_token_kind(parser, TKN_RBRACE) && !is_token_kind(parser, TKN_EOF)) {
                EnumItem* item = parse_enum_item(parser);

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
// Ex: typedef i8 = int8;
static Decl* parse_decl_typedef(Parser* parser)
{
    assert(is_keyword(parser, KW_TYPEDEF));
    Decl* decl = NULL;
    ProgRange range = {.start = parser->token.range.start};
    const char* error_prefix = "Failed to parse typedef declaration";

    next_token(parser);

    if (expect_token_next(parser, TKN_IDENT, error_prefix)) {
        const char* name = parser->ptoken.as_ident.value;

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
// Ex 1: const x : int = 0;
// Ex 2: const x := 0;
static Decl* parse_decl_const(Parser* parser)
{
    assert(is_keyword(parser, KW_CONST));
    Decl* decl = NULL;
    ProgRange range = {.start = parser->token.range.start};
    const char* error_prefix = "Failed to parse const declaration";

    next_token(parser);

    if (expect_token_next(parser, TKN_IDENT, error_prefix)) {
        const char* name = parser->ptoken.as_ident.value;

        if (expect_token_next(parser, TKN_COLON, error_prefix)) {
            TypeSpec* type = NULL;
            bool bad_type = false;

            if (!is_token_kind(parser, TKN_ASSIGN)) {
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

// decl = decl_const
//      | decl_typedef
//      | decl_var
//      | decl_enum
//      | decl_struct
//      | decl_union
//      | decl_proc
Decl* parse_decl(Parser* parser)
{
    if (is_token_kind(parser, TKN_KW)) {
        switch (parser->token.as_kw.kw) {
        case KW_CONST:
            return parse_decl_const(parser);
        case KW_TYPEDEF:
            return parse_decl_typedef(parser);
        case KW_VAR:
            return parse_decl_var(parser);
        case KW_ENUM:
            return parse_decl_enum(parser);
        case KW_STRUCT:
            return parse_decl_aggregate(parser, "Failed to parse struct declaration", decl_struct);
        case KW_UNION:
            return parse_decl_aggregate(parser, "Failed to parse union declaration", decl_union);
        case KW_PROC:
            return parse_decl_proc(parser);
        default:
            break;
        }
    }

    // If we got here, we have an unexpected token.
    char tmp[32];

    print_token(&parser->token, tmp, sizeof(tmp));
    parser_on_error(parser, "Unexpected token: wanted a declaration keyword, but got `%s`", tmp);

    return NULL;
}
