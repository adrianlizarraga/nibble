#include "parser.h"
#include "ast.h"

#include <string.h>

#define PARSER_ARENA_BLOCK_SIZE 512
//#define NIBBLE_PRINT_DECLS

static void parser_on_error(Parser* parser, const char* format, ...)
{
    if (parser->errors)
    {
        char buf[MAX_ERROR_LEN];
        size_t size = 0;
        va_list vargs;

        va_start(vargs, format);
        size = vsnprintf(buf, MAX_ERROR_LEN, format, vargs) + 1;
        va_end(vargs);

        add_byte_stream_chunk(parser->errors, buf, size > sizeof(buf) ? sizeof(buf) : size);
    }
}

void parser_init(Parser* parser, Allocator* ast_arena, const char* str, ProgPos pos, ByteStream* errors)
{
    memset(parser, 0, sizeof(Parser));

    parser->ast_arena = ast_arena;
    parser->errors = errors;
    parser->start = pos;
    parser->temp_arena = allocator_create(PARSER_ARENA_BLOCK_SIZE);
    parser->lexer = lexer_create(str, pos, &parser->temp_arena, errors);
}

void parser_destroy(Parser* parser)
{
#if !defined(NDEBUG) && PRINT_MEM_USAGE
    print_allocator_stats(&parser->temp_arena, "Parser temp mem stats");
#endif
    allocator_destroy(&parser->temp_arena);
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

bool match_token(Parser* parser, TokenKind kind)
{
    bool matches = (parser->token.kind == kind);

    if (matches)
    {
        next_token(parser);
    }

    return matches;
}

bool match_keyword(Parser* parser, Keyword kw)
{
    bool matches = (parser->token.kind == TKN_KW) && (parser->token.as_kw.kw == kw);

    if (matches)
    {
        next_token(parser);
    }

    return matches;
}

bool expect_token(Parser* parser, TokenKind kind, const char* error_prefix)
{
    bool matches = (parser->token.kind == kind);

    if (matches)
    {
        next_token(parser);
    }
    else
    {
        char tmp[32];

        print_token(&parser->token, tmp, sizeof(tmp));
        parser_on_error(parser, "%s: wanted token `%s`, but got token `%s`.",
                        error_prefix ? error_prefix : "Unexpected token", token_kind_names[kind], tmp);
    }

    return matches;
}

bool expect_keyword(Parser* parser, Keyword kw, const char* error_prefix)
{
    bool matches = (parser->token.kind == TKN_KW) && (parser->token.as_kw.kw == kw);

    if (matches)
    {
        next_token(parser);
    }
    else
    {
        char tmp[32];

        print_token(&parser->token, tmp, sizeof(tmp));
        parser_on_error(parser, "%s : wanted keyword `%s`, but got token `%s`",
                        error_prefix ? error_prefix : "Unexpected token", keywords[kw], tmp);
    }

    return matches;
}

bool skip_after_token(Parser* parser, TokenKind kind)
{
    while (!is_token_kind(parser, TKN_EOF) && !is_token_kind(parser, kind))
    {
        next_token(parser);
    }

    return match_token(parser, kind);
}

///////////////////////////////
//    Parse type specifiers
//////////////////////////////

// aggregate_field = TKN_IDENT ':' type_spec ';'
static AggregateField* parse_aggregate_field(Parser* parser)
{
    const char* error_prefix = "Failed to parse field";

    if (!expect_token(parser, TKN_IDENT, error_prefix))
        return NULL;

    const char* name = parser->ptoken.as_ident.value;
    ProgRange range = {.start = parser->ptoken.range.start};

    if (!expect_token(parser, TKN_COLON, error_prefix))
        return NULL;

    TypeSpec* type = parse_typespec(parser);

    if (!type || !expect_token(parser, TKN_SEMICOLON, error_prefix))
        return NULL;

    range.end = parser->ptoken.range.end;

    return aggregate_field(parser->ast_arena, name, type, range);
}

// aggregate_body  = TKN_IDENT '{' aggregate_field* '}'
static bool parse_fill_aggregate_body(Parser* parser, size_t* num_fields, List* fields)
{
    list_head_init(fields);
    *num_fields = 0;

    while (!is_token_kind(parser, TKN_RBRACE) && !is_token_kind(parser, TKN_EOF))
    {
        AggregateField* field = parse_aggregate_field(parser);

        if (!field)
            return false;

        *num_fields += 1;
        list_add(fields->prev, &field->lnode);
    }

    return true;
}

static TypeSpec* parse_typespec_aggregate(Parser* parser, const char* error_prefix,
                                          TypeSpecAggregateProc* typespec_aggregate_proc)
{
    assert(is_keyword(parser, KW_STRUCT) || is_keyword(parser, KW_UNION));

    TypeSpec* type = NULL;
    ProgRange range = {.start = parser->token.range.start};

    next_token(parser);

    if (expect_token(parser, TKN_LBRACE, error_prefix))
    {
        size_t num_fields = 0;
        List fields = {0};

        if (parse_fill_aggregate_body(parser, &num_fields, &fields) && expect_token(parser, TKN_RBRACE, error_prefix))
        {
            if (num_fields)
            {
                range.end = parser->ptoken.range.end;
                type = typespec_aggregate_proc(parser->ast_arena, num_fields, &fields, range);
            }
            else
            {
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

    if (type && match_token(parser, TKN_COLON))
    {
        if (type->kind == AST_TypeSpecIdent)
        {
            // NOTE: I wish this was truly LL1
            ProgRange range = {.start = type->range.start};
            TypeSpecIdent* tident = (TypeSpecIdent*)type;
            const char* name = tident->name;

            mem_free(parser->ast_arena, type);

            type = parse_typespec(parser);

            if (type)
            {
                range.end = type->range.end;
                param = proc_param(parser->ast_arena, name, type, range);
            }
        }
        else
        {
            parser_on_error(parser, "Parameter's name must be an alphanumeric identifier");
        }
    }
    else if (type)
    {
        param = proc_param(parser->ast_arena, NULL, type, type->range);
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

    if (expect_token(parser, TKN_LPAREN, error_prefix))
    {
        size_t num_params = 0;
        List params = list_head_create(params);
        bool bad_param = false;

        while (!is_token_kind(parser, TKN_RPAREN) && !is_token_kind(parser, TKN_EOF))
        {
            ProcParam* param = parse_typespec_proc_param(parser);

            if (param)
            {
                num_params += 1;
                list_add(params.prev, &param->lnode);
            }
            else
            {
                bad_param = true;
                break;
            }

            if (!match_token(parser, TKN_COMMA))
                break;
        }

        if (!bad_param && expect_token(parser, TKN_RPAREN, error_prefix))
        {
            TypeSpec* ret = NULL;
            bool bad_ret = false;

            if (match_token(parser, TKN_ARROW))
            {
                ret = parse_typespec(parser);
                bad_ret = ret == NULL;
            }

            if (!bad_ret)
            {
                range.end = parser->ptoken.range.end;
                type = typespec_proc(parser->ast_arena, num_params, &params, ret, range);
            }
        }
    }

    return type;
}

static TypeSpec* parse_typespec_ident(Parser* parser)
{
    assert(is_token_kind(parser, TKN_IDENT));
    Token token = parser->token;

    next_token(parser);

    return typespec_ident(parser->ast_arena, token.as_ident.value, token.range);
}

// typespec_base  = typespec_proc
//                | typespec_anon_struct
//                | typespec_anon_union
//                | typespec_ident
//                | '(' type_spec ')'
static TypeSpec* parse_typespec_base(Parser* parser)
{
    Token token = parser->token;

    switch (token.kind)
    {
        case TKN_KW:
        {
            switch (token.as_kw.kw)
            {
                case KW_PROC:
                    return parse_typespec_proc(parser);
                case KW_STRUCT:
                    return parse_typespec_aggregate(parser, "Failed to parse anonymous struct", typespec_struct);
                case KW_UNION:
                    return parse_typespec_aggregate(parser, "Failed to parse anonymous union", typespec_union);
                default:
                    break;
            }
        }
        break;
        case TKN_IDENT:
            return parse_typespec_ident(parser);
        case TKN_LPAREN:
        {
            next_token(parser);

            TypeSpec* type = NULL;
            TypeSpec* enclosed_type = parse_typespec(parser);

            if (enclosed_type && expect_token(parser, TKN_RPAREN, NULL))
            {
                type = enclosed_type;
            }

            return type;
        }
        break;
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

    if (match_token(parser, TKN_CARET))
    {
        //
        // Pointer typespec.
        //

        ProgRange range = {.start = parser->ptoken.range.start};
        TypeSpec* base = parse_typespec(parser);

        if (base)
        {
            range.end = base->range.end;
            type = typespec_ptr(parser->ast_arena, base, range);
        }
    }
    else if (match_token(parser, TKN_LBRACKET))
    {
        //
        // Array typespec.
        //

        ProgRange range = {.start = parser->ptoken.range.start};

        Expr* len = NULL;
        bool bad_len = false;

        if (!is_token_kind(parser, TKN_RBRACKET))
        {
            len = parse_expr(parser);
            bad_len = len == NULL;
        }

        if (!bad_len && expect_token(parser, TKN_RBRACKET, "Failed to parse array type"))
        {
            TypeSpec* base = parse_typespec(parser);

            if (base)
            {
                range.end = base->range.end;
                type = typespec_array(parser->ast_arena, base, len, range);
            }
        }
    }
    else if (match_keyword(parser, KW_CONST))
    {
        //
        // Const typespec
        //

        ProgRange range = {.start = parser->ptoken.range.start};
        TypeSpec* base = parse_typespec(parser);

        if (base)
        {
            range.end = base->range.end;
            type = typespec_const(parser->ast_arena, base, range);
        }
    }
    else
    {
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

    if (match_token(parser, TKN_LBRACKET))
    {
        Expr* index = parse_expr(parser);

        if (index && expect_token(parser, TKN_RBRACKET, error_prefix) && expect_token(parser, TKN_ASSIGN, error_prefix))
        {
            Expr* init = parse_expr(parser);

            if (init)
            {
                range.end = init->range.end;
                designator.kind = DESIGNATOR_INDEX;
                designator.index = index;
                initzer = member_initializer(parser->ast_arena, init, designator, range);
            }
        }
    }
    else
    {
        Expr* expr = parse_expr(parser);

        if (expr && match_token(parser, TKN_ASSIGN))
        {
            if (expr->kind == AST_ExprIdent)
            {
                const char* name = ((ExprIdent*)expr)->name;

                mem_free(parser->ast_arena, expr);

                Expr* init = parse_expr(parser);

                if (init)
                {
                    range.end = init->range.end;
                    designator.kind = DESIGNATOR_NAME;
                    designator.name = name;
                    initzer = member_initializer(parser->ast_arena, init, designator, range);
                }
            }
            else
            {
                parser_on_error(parser, "Initializer designator name must be alphanumeric");
            }
        }
        else if (expr)
        {
            range.end = expr->range.end;
            initzer = member_initializer(parser->ast_arena, expr, designator, range);
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

    if (expect_token(parser, TKN_LBRACE, error_prefix))
    {
        ProgRange range = {.start = parser->ptoken.range.start};
        size_t num_initzers = 0;
        List initzers = list_head_create(initzers);
        bool bad_initzer = false;

        while (!is_token_kind(parser, TKN_RBRACE) && !is_token_kind(parser, TKN_COLON) &&
               !is_token_kind(parser, TKN_EOF))
        {
            MemberInitializer* initzer = parse_member_initializer(parser);

            if (initzer)
            {
                num_initzers += 1;
                list_add(initzers.prev, &initzer->lnode);
            }
            else
            {
                bad_initzer = true;
                break;
            }

            if (!match_token(parser, TKN_COMMA))
                break;
        }

        if (!bad_initzer)
        {
            TypeSpec* type = NULL;

            if (match_token(parser, TKN_COLON))
            {
                type = parse_typespec(parser);

                if (type && expect_token(parser, TKN_RBRACE, error_prefix))
                {
                    range.end = parser->ptoken.range.end;
                    expr = expr_compound_lit(parser->ast_arena, type, num_initzers, &initzers, range);
                }
            }
            else if (expect_token(parser, TKN_RBRACE, error_prefix))
            {
                range.end = parser->ptoken.range.end;
                expr = expr_compound_lit(parser->ast_arena, type, num_initzers, &initzers, range);
            }
        }
    }

    return expr;
}

static Expr* parse_expr_sizeof(Parser* parser)
{
    assert(is_keyword(parser, KW_SIZEOF));
    Expr* expr = NULL;
    ProgRange range = {.start = parser->token.range.start};
    const char* error_prefix = "Failed to parse sizeof expression";

    next_token(parser);

    if (expect_token(parser, TKN_LPAREN, error_prefix))
    {
        TypeSpec* type = parse_typespec(parser);

        if (type && expect_token(parser, TKN_RPAREN, error_prefix))
        {
            range.end = parser->ptoken.range.end;
            expr = expr_sizeof(parser->ast_arena, type, range);
        }
    }

    return expr;
}

static Expr* parse_expr_typeof(Parser* parser)
{
    assert(is_keyword(parser, KW_TYPEOF));
    Expr* expr = NULL;
    ProgRange range = {.start = parser->token.range.start};
    const char* error_prefix = "Failed to parse typeof expression";

    next_token(parser);

    if (expect_token(parser, TKN_LPAREN, error_prefix))
    {
        Expr* arg = parse_expr(parser);

        if (arg && expect_token(parser, TKN_RPAREN, error_prefix))
        {
            range.end = parser->ptoken.range.end;
            expr = expr_typeof(parser->ast_arena, arg, range);
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

    switch (token.kind)
    {
        case TKN_INT:
            next_token(parser);
            return expr_int(parser->ast_arena, token.as_int.value, token.range);
        case TKN_FLOAT:
            next_token(parser);
            return expr_float(parser->ast_arena, token.as_float.value, token.range);
        case TKN_STR:
            next_token(parser);
            return expr_str(parser->ast_arena, token.as_str.value, token.range);
        case TKN_LPAREN:
        {
            next_token(parser);

            Expr* enclosed = parse_expr(parser);

            if (enclosed && expect_token(parser, TKN_RPAREN, NULL))
            {
                return enclosed;
            }
        }
        break;
        case TKN_LBRACE:
            return parse_expr_compound_lit(parser);
        case TKN_KW:
        {
            switch (token.as_kw.kw)
            {
                case KW_SIZEOF:
                    return parse_expr_sizeof(parser);
                case KW_TYPEOF:
                    return parse_expr_typeof(parser);
                default:
                    break;
            }
        }
        break;
        case TKN_IDENT:
            next_token(parser);
            return expr_ident(parser->ast_arena, token.as_ident.value, token.range);
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

    if (expr && match_token(parser, TKN_ASSIGN))
    {
        if (expr->kind == AST_ExprIdent)
        {
            const char* name = ((ExprIdent*)expr)->name;

            mem_free(parser->ast_arena, expr);

            expr = parse_expr(parser);

            if (expr)
            {
                arg = proc_call_arg(parser->ast_arena, expr, name);
            }
        }
        else
        {
            parser_on_error(parser, "Procedure argument's name must be an alphanumeric identifier");
        }
    }
    else if (expr)
    {
        arg = proc_call_arg(parser->ast_arena, expr, NULL);
    }

    return arg;
}

// expr_base_mod = expr_base ('.' IDENTIFIER | '[' expr ']' | '(' proc_call_arg_list* ')' | ':>' typespec)*
// proc_call_arg_list = proc_call_arg (',' proc_call_arg)*
static Expr* parse_expr_base_mod(Parser* parser)
{
    Expr* expr = parse_expr_base(parser);

    while (expr && (is_token_kind(parser, TKN_DOT) || is_token_kind(parser, TKN_LBRACKET) ||
                    is_token_kind(parser, TKN_LPAREN) || is_token_kind(parser, TKN_CAST)))
    {
        if (match_token(parser, TKN_DOT))
        {
            //
            // Field access.
            //

            if (expect_token(parser, TKN_IDENT, "Failed to parse field access"))
            {
                const char* field = parser->ptoken.as_ident.value;
                ProgRange range = {.start = expr->range.start, .end = parser->ptoken.range.end};
                expr = expr_field(parser->ast_arena, expr, field, range);
            }
            else
            {
                expr = NULL;
            }
        }
        else if (match_token(parser, TKN_LBRACKET))
        {
            //
            // Array index access.
            //

            Expr* index = parse_expr(parser);

            if (index && expect_token(parser, TKN_RBRACKET, "Failed to parse array access"))
            {
                ProgRange range = {.start = expr->range.start, .end = parser->ptoken.range.end};
                expr = expr_index(parser->ast_arena, expr, index, range);
            }
            else
            {
                expr = NULL;
            }
        }
        else if (match_token(parser, TKN_LPAREN))
        {
            //
            // Procedure call.
            //

            size_t num_args = 0;
            List args = list_head_create(args);
            bool bad_arg = false;

            while (!is_token_kind(parser, TKN_RPAREN) && !is_token_kind(parser, TKN_EOF))
            {
                ProcCallArg* arg = parse_proc_call_arg(parser);

                if (arg)
                {
                    num_args += 1;
                    list_add(args.prev, &arg->lnode);
                }
                else
                {
                    bad_arg = true;
                    break;
                }

                if (!match_token(parser, TKN_COMMA))
                    break;
            }

            if (!bad_arg && expect_token(parser, TKN_RPAREN, "Failed to parse procedure call"))
            {
                ProgRange range = {.start = expr->range.start, .end = parser->ptoken.range.end};
                expr = expr_call(parser->ast_arena, expr, num_args, &args, range);
            }
            else
            {
                expr = NULL;
            }
        }
        else
        {
            //
            // Cast expression.
            //

            assert(is_token_kind(parser, TKN_CAST));
            next_token(parser);

            TypeSpec* type = parse_typespec(parser);

            if (type)
            {
                ProgRange range = {.start = expr->range.start, .end = type->range.end};
                expr = expr_cast(parser->ast_arena, type, expr, range);
            }
            else
            {
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

    if (is_token_prop_kind(parser, OP_PRECEDENCE_UNARY))
    {
        ProgRange range = {.start = parser->token.range.start};
        TokenKind op = parser->token.kind;

        next_token(parser);

        Expr* unary = parse_expr_unary(parser);

        if (unary)
        {
            range.end = unary->range.end;
            expr = expr_unary(parser->ast_arena, op, unary, range);
        }
    }
    else
    {
        expr = parse_expr_base_mod(parser);
    }

    return expr;
}

// expr_mul = expr_unary (OP_PRECEDENCE_MUL expr_unary)*
static Expr* parse_expr_mul(Parser* parser)
{
    Expr* expr = parse_expr_unary(parser);

    while (expr && is_token_prop_kind(parser, OP_PRECEDENCE_MUL))
    {
        TokenKind op = parser->token.kind;

        next_token(parser);

        Expr* left = expr;
        Expr* right = parse_expr_unary(parser);

        if (right)
        {
            expr = expr_binary(parser->ast_arena, op, left, right);
        }
        else
        {
            expr = NULL;
        }
    }

    return expr;
}

// expr_add = expr_mul (OP_PRECEDENCE_ADD expr_mul)*
static Expr* parse_expr_add(Parser* parser)
{
    Expr* expr = parse_expr_mul(parser);

    while (expr && is_token_prop_kind(parser, OP_PRECEDENCE_ADD))
    {
        TokenKind op = parser->token.kind;

        next_token(parser);

        Expr* left = expr;
        Expr* right = parse_expr_mul(parser);

        if (right)
        {
            expr = expr_binary(parser->ast_arena, op, left, right);
        }
        else
        {
            expr = NULL;
        }
    }

    return expr;
}

// expr_cmp = expr_add (OP_PRECEDENCE_CMP expr_add)*
static Expr* parse_expr_cmp(Parser* parser)
{
    Expr* expr = parse_expr_add(parser);

    while (expr && is_token_prop_kind(parser, OP_PRECEDENCE_CMP))
    {
        TokenKind op = parser->token.kind;

        next_token(parser);

        Expr* left = expr;
        Expr* right = parse_expr_add(parser);

        if (right)
        {
            expr = expr_binary(parser->ast_arena, op, left, right);
        }
        else
        {
            expr = NULL;
        }
    }

    return expr;
}

// expr_and = expr_cmp ('&&' expr_cmp)*
static Expr* parse_expr_and(Parser* parser)
{
    Expr* expr = parse_expr_cmp(parser);

    while (expr && match_token(parser, TKN_LOGIC_AND))
    {
        Expr* left = expr;
        Expr* right = parse_expr_cmp(parser);

        if (right)
        {
            expr = expr_binary(parser->ast_arena, TKN_LOGIC_AND, left, right);
        }
        else
        {
            expr = NULL;
        }
    }

    return expr;
}

// expr_or = expr_and ('||' expr_and)*
static Expr* parse_expr_or(Parser* parser)
{
    Expr* expr = parse_expr_and(parser);

    while (expr && match_token(parser, TKN_LOGIC_OR))
    {
        Expr* left = expr;
        Expr* right = parse_expr_and(parser);

        if (right)
        {
            expr = expr_binary(parser->ast_arena, TKN_LOGIC_OR, left, right);
        }
        else
        {
            expr = NULL;
        }
    }

    return expr;
}

// expr_ternary = expr_or ('?' expr_ternary ':' expr_ternary)?
static Expr* parse_expr_ternary(Parser* parser)
{
    Expr* expr = parse_expr_or(parser);

    if (expr && match_token(parser, TKN_QUESTION))
    {
        Expr* then_expr = parse_expr_ternary(parser);

        if (then_expr)
        {
            if (expect_token(parser, TKN_COLON, "Failed to parse ternary operator expression"))
            {
                Expr* else_expr = parse_expr_ternary(parser);

                if (else_expr)
                {
                    expr = expr_ternary(parser->ast_arena, expr, then_expr, else_expr);
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

static bool parse_fill_stmt_list(Parser* parser, size_t* num_stmts, List* stmts)
{
    bool bad_stmt = false;

    list_head_init(stmts);
    *num_stmts = 0;

    while (!is_token_kind(parser, TKN_RBRACE) && !is_token_kind(parser, TKN_EOF))
    {
        Stmt* stmt = parse_stmt(parser);

        if (stmt)
        {
            *num_stmts += 1;
            list_add(stmts->prev, &stmt->lnode);
        }
        else
        {
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
    List stmts = {0};
    bool ok = parse_fill_stmt_list(parser, &num_stmts, &stmts);

    if (ok && expect_token(parser, TKN_RBRACE, "Failed to parse end of statement block"))
    {
        range.end = parser->ptoken.range.end;
        stmt = stmt_block(parser->ast_arena, num_stmts, &stmts, range);
    }

    return stmt;
}

static bool parse_fill_if_cond_block(Parser* parser, IfCondBlock* cblock, const char* error_prefix)
{
    ProgPos start = parser->token.range.start;

    if (!expect_token(parser, TKN_LPAREN, error_prefix))
        return false;

    Expr* cond = parse_expr(parser);

    if (!cond || !expect_token(parser, TKN_RPAREN, error_prefix))
        return false;

    Stmt* body = parse_stmt(parser);

    if (!body)
        return false;

    cblock->cond = cond;
    cblock->body = body;
    cblock->range.start = start;
    cblock->range.end = body->range.end;

    return true;
}

static IfCondBlock* parse_stmt_elif_block(Parser* parser)
{
    assert(is_keyword(parser, KW_ELIF));
    IfCondBlock* elif_blk = NULL;
    const char* error_prefix = "Failed to parse elif statement";

    next_token(parser);

    IfCondBlock cblock = {0};

    if (parse_fill_if_cond_block(parser, &cblock, error_prefix))
        elif_blk = if_cond_block(parser->ast_arena, cblock.cond, cblock.body, cblock.range);

    return elif_blk;
}

// stmt_else = 'else' stmt
static bool parse_fill_else_block(Parser* parser, ElseBlock* else_blk)
{
    assert(is_keyword(parser, KW_ELSE));
    ProgPos start = parser->token.range.start;

    next_token(parser);

    Stmt* body = parse_stmt(parser);

    if (!body)
        return false;

    else_blk->body = body;
    else_blk->range.start = start;
    else_blk->range.end = body->range.end;

    return true;
}

// stmt_if = 'if' '(' expr ')' stmt ('elif' '(' expr ')' stmt)* ('else' stmt)?
static Stmt* parse_stmt_if(Parser* parser)
{
    assert(is_keyword(parser, KW_IF));
    Stmt* stmt = NULL;
    ProgRange range = {.start = parser->token.range.start};
    const char* err_pre = "Failed to parse if statement";

    next_token(parser);

    IfCondBlock if_blk = {0};
    bool ok_if = parse_fill_if_cond_block(parser, &if_blk, err_pre);

    if (ok_if)
    {
        size_t num_elif_blks = 0;
        List elif_blks = list_head_create(elif_blks);
        bool bad_elif = false;

        while (is_keyword(parser, KW_ELIF))
        {
            IfCondBlock* elif = parse_stmt_elif_block(parser);

            if (elif)
            {
                num_elif_blks += 1;
                list_add(elif_blks.prev, &elif->lnode);
            }
            else
            {
                bad_elif = true;
                break;
            }
        }

        if (!bad_elif)
        {
            bool bad_else = false;
            ElseBlock else_blk = {0};

            if (is_keyword(parser, KW_ELSE))
                bad_else = !parse_fill_else_block(parser, &else_blk);

            if (!bad_else)
                stmt = stmt_if(parser->ast_arena, &if_blk, num_elif_blks, &elif_blks, &else_blk, range);
        }
    }

    return stmt;
}

// switch_case = 'case' (expr ('..' expr)?)? ':' stmt*
static SwitchCase* parse_switch_case(Parser* parser)
{
    const char* error_prefix = "Failed to parse switch statement case";

    if (!expect_keyword(parser, KW_CASE, error_prefix))
        return NULL;

    ProgRange range = {.start = parser->ptoken.range.start};
    Expr* start = NULL;
    Expr* end = NULL;

    // Parse case start/end expression(s).
    if (!is_token_kind(parser, TKN_COLON))
    {
        start = parse_expr(parser);

        if (!start)
            return NULL;

        if (match_token(parser, TKN_ELLIPSIS))
        {
            end = parse_expr(parser);

            if (!end)
                return NULL;
        }
    }

    if (!expect_token(parser, TKN_COLON, error_prefix))
        return NULL;

    SwitchCase* swcase = NULL;
    size_t num_stmts = 0;
    List stmts = list_head_create(stmts);
    bool bad_stmt = false;

    // Parse case statements
    while (!is_keyword(parser, KW_CASE) && !is_token_kind(parser, TKN_RBRACE) && !is_token_kind(parser, TKN_EOF))
    {
        Stmt* stmt = parse_stmt(parser);

        if (!stmt)
        {
            bad_stmt = true;
            break;
        }

        num_stmts += 1;
        list_add(stmts.prev, &stmt->lnode);
    }

    if (!bad_stmt)
    {
        range.end = parser->ptoken.range.end;
        swcase = switch_case(parser->ast_arena, start, end, num_stmts, &stmts, range);
    }

    return swcase;
}

// stmt_switch = 'switch' '(' expr ')' '{' switch_case+ '}'
static Stmt* parse_stmt_switch(Parser* parser)
{
    assert(is_keyword(parser, KW_SWITCH));
    ProgRange range = {.start = parser->token.range.start};
    const char* error_prefix = "Failed to parse switch statement";

    next_token(parser);

    if (!expect_token(parser, TKN_LPAREN, error_prefix))
        return NULL;

    Expr* expr = parse_expr(parser);

    if (!expr || !expect_token(parser, TKN_RPAREN, error_prefix) || !expect_token(parser, TKN_LBRACE, error_prefix))
        return NULL;

    Stmt* stmt = NULL;
    size_t num_cases = 0;
    List cases = list_head_create(cases);
    bool has_default = false;
    bool bad_case = false;

    do
    {
        SwitchCase* swcase = parse_switch_case(parser);

        if (!swcase)
        {
            bad_case = true;
            break;
        }

        bool is_default = !swcase->start && !swcase->end;

        if (has_default && is_default)
        {
            parser_on_error(parser, "Switch statement can have at most one default case");
            bad_case = true;
            break;
        }

        has_default = has_default || is_default;

        num_cases += 1;
        list_add(cases.prev, &swcase->lnode);
    } while (is_keyword(parser, KW_CASE));

    if (!bad_case && expect_token(parser, TKN_RBRACE, error_prefix))
    {
        range.end = parser->ptoken.range.end;
        stmt = stmt_switch(parser->ast_arena, expr, num_cases, &cases, range);
    }

    return stmt;
}

// stmt_while = 'while' '(' expr ')' stmt
static Stmt* parse_stmt_while(Parser* parser)
{
    assert(is_keyword(parser, KW_WHILE));
    ProgRange range = {.start = parser->token.range.start};
    const char* error_prefix = "Failed to parse while-loop statement";

    next_token(parser);

    if (!expect_token(parser, TKN_LPAREN, error_prefix))
        return NULL;

    Expr* cond = parse_expr(parser);

    if (!cond || !expect_token(parser, TKN_RPAREN, error_prefix))
        return NULL;

    Stmt* body = parse_stmt(parser);

    if (!body)
        return NULL;

    range.end = body->range.end;

    return stmt_while(parser->ast_arena, cond, body, range);
}

// stmt_do_while = 'do' stmt 'while' '(' expr ')' ';'
static Stmt* parse_stmt_do_while(Parser* parser)
{
    assert(is_keyword(parser, KW_DO));
    Stmt* stmt = NULL;
    ProgRange range = {.start = parser->token.range.start};
    const char* error_prefix = "Failed to parse do-while-loop statement";

    next_token(parser);

    Stmt* body = parse_stmt(parser);

    if (body && expect_keyword(parser, KW_WHILE, error_prefix) && expect_token(parser, TKN_LPAREN, error_prefix))
    {
        Expr* cond = parse_expr(parser);

        if (cond && expect_token(parser, TKN_RPAREN, error_prefix) && expect_token(parser, TKN_SEMICOLON, error_prefix))
        {
            range.end = parser->ptoken.range.end;
            stmt = stmt_do_while(parser->ast_arena, cond, body, range);
        }
    }

    return stmt;
}

static Stmt* parse_stmt_decl(Parser* parser)
{
    Stmt* stmt = NULL;
    Decl* decl = parse_decl(parser);

    if (decl)
        stmt = stmt_decl(parser->ast_arena, decl);

    return stmt;
}

static Stmt* parse_stmt_expr(Parser* parser, bool terminate)
{
    Stmt* stmt = NULL;
    Expr* expr = parse_expr(parser);
    ProgRange range = {.start = expr->range.start};
    const char* error_prefix = "Failed to parse expression statement";

    if (expr)
    {
        if (is_token_prop_kind(parser, OP_ASSIGN))
        {
            TokenKind op_assign = parser->token.kind;

            next_token(parser);

            Expr* rexpr = parse_expr(parser);

            if (rexpr && !terminate)
            {
                range.end = rexpr->range.end;
                stmt = stmt_expr_assign(parser->ast_arena, expr, op_assign, rexpr, range);
            }
            else if (rexpr && expect_token(parser, TKN_SEMICOLON, error_prefix))
            {
                range.end = parser->ptoken.range.end;
                stmt = stmt_expr_assign(parser->ast_arena, expr, op_assign, rexpr, range);
            }
        }
        else if (!terminate)
        {
            range.end = expr->range.end;
            stmt = stmt_expr(parser->ast_arena, expr, range);
        }
        else if (expect_token(parser, TKN_SEMICOLON, error_prefix))
        {
            range.end = parser->ptoken.range.end;
            stmt = stmt_expr(parser->ast_arena, expr, range);
        }
    }

    return stmt;
}

// stmt_for = 'for' '(' (decl_var | expr | expr_assign)? ';' expr? ';' (expr | expr_assign)? ')' stmt
static Stmt* parse_stmt_for(Parser* parser)
{
    assert(is_keyword(parser, KW_FOR));
    ProgRange range = {.start = parser->token.range.start};
    const char* error_prefix = "Failed to parse for-loop statement";

    next_token(parser);

    if (!expect_token(parser, TKN_LPAREN, error_prefix))
        return NULL;

    Stmt* init = NULL;

    if (!match_token(parser, TKN_SEMICOLON))
    {
        if (is_keyword(parser, KW_VAR))
        {
            init = parse_stmt_decl(parser);
        }
        else
        {
            init = parse_stmt_expr(parser, true);
        }

        if (!init)
            return NULL;
    }

    Expr* cond = NULL;

    if (!match_token(parser, TKN_SEMICOLON))
    {
        cond = parse_expr(parser);

        if (!cond || !expect_token(parser, TKN_SEMICOLON, error_prefix))
            return NULL;
    }

    Stmt* next = NULL;

    if (!is_token_kind(parser, TKN_RPAREN))
    {
        next = parse_stmt_expr(parser, false);

        if (!next)
            return NULL;
    }

    if (!expect_token(parser, TKN_RPAREN, error_prefix))
        return NULL;

    Stmt* body = parse_stmt(parser);

    if (!body)
        return NULL;

    range.end = body->range.end;

    return stmt_for(parser->ast_arena, init, cond, next, body, range);
}

// stmt_return = 'return' expr? ';'
static Stmt* parse_stmt_return(Parser* parser)
{
    assert(is_keyword(parser, KW_RETURN));
    Expr* expr = NULL;
    ProgRange range = {.start = parser->token.range.start};

    next_token(parser);

    // This seems like a common error, so we can try to look for it.
    // However, this is not a complete check, or even necessary. Bikeshedding!!
    if (is_token_kind(parser, TKN_RBRACE))
    {
        parser_on_error(parser, "Failed to parse return statement: wanted `;` or expression, but got `}`");

        return NULL;
    }

    if (!is_token_kind(parser, TKN_SEMICOLON))
    {
        expr = parse_expr(parser);

        if (!expr)
            return NULL;
    }

    if (!expect_token(parser, TKN_SEMICOLON, "Failed to parse return statement"))
        return NULL;

    range.end = parser->ptoken.range.end;

    return stmt_return(parser->ast_arena, expr, range);
}

// stmt_break = 'break' TKN_IDENT? ';'
static Stmt* parse_stmt_break(Parser* parser)
{
    assert(is_keyword(parser, KW_BREAK));
    ProgRange range = {.start = parser->token.range.start};
    const char* label = NULL;

    next_token(parser);

    if (match_token(parser, TKN_IDENT))
        label = parser->ptoken.as_ident.value;

    if (!expect_token(parser, TKN_SEMICOLON, "Failed to parse break statement"))
        return NULL;

    range.end = parser->ptoken.range.end;

    return stmt_break(parser->ast_arena, label, range);
}

// stmt_continue = 'continue' TKN_IDENT? ';'
static Stmt* parse_stmt_continue(Parser* parser)
{
    assert(is_keyword(parser, KW_CONTINUE));
    ProgRange range = {.start = parser->token.range.start};
    const char* label = NULL;

    next_token(parser);

    if (match_token(parser, TKN_IDENT))
        label = parser->ptoken.as_ident.value;

    if (!expect_token(parser, TKN_SEMICOLON, "Failed to parse continue statement"))
        return NULL;

    range.end = parser->ptoken.range.end;

    return stmt_continue(parser->ast_arena, label, range);
}

// stmt_goto = 'goto' TKN_IDENT ';'
static Stmt* parse_stmt_goto(Parser* parser)
{
    assert(is_keyword(parser, KW_GOTO));
    ProgRange range = {.start = parser->token.range.start};
    const char* error_prefix = "Failed to parse goto statement";
    const char* label = NULL;

    next_token(parser);

    if (!expect_token(parser, TKN_IDENT, error_prefix))
        return NULL;

    label = parser->ptoken.as_ident.value;

    if (!expect_token(parser, TKN_SEMICOLON, error_prefix))
        return NULL;

    range.end = parser->ptoken.range.end;

    return stmt_goto(parser->ast_arena, label, range);
}

// stmt_label = 'label' TKN_IDENT ':' stmt
static Stmt* parse_stmt_label(Parser* parser)
{
    assert(is_keyword(parser, KW_LABEL));
    ProgRange range = {.start = parser->token.range.start};
    const char* error_prefix = "Failed to parse label";

    next_token(parser);

    if (!expect_token(parser, TKN_IDENT, error_prefix))
        return NULL;

    const char* label = parser->ptoken.as_ident.value;

    if (!expect_token(parser, TKN_COLON, error_prefix))
        return NULL;

    Stmt* stmt = parse_stmt(parser);

    if (!stmt)
    {
        parser_on_error(parser, "Label must be attached to a statement");
        return NULL;
    }

    range.end = stmt->range.end;

    return stmt_label(parser->ast_arena, label, stmt, range);
}

// stmt = 'if' '(' expr ')' stmt ('elif' '(' expr ')' stmt)* ('else' stmt)?
//      | ';'
//      | stmt_while
//      | stmt_do_while
//      | 'for' '(' (decl_var | expr | expr_assign)? ';' expr? ';' (expr | expr_assign)? ')' stmt
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

    switch (token.kind)
    {
        case TKN_SEMICOLON:
            next_token(parser);
            return stmt_noop(parser->ast_arena, token.range);
        case TKN_LBRACE:
            return parse_stmt_block(parser);
        case TKN_KW:
        {
            switch (token.as_kw.kw)
            {
                case KW_IF:
                    return parse_stmt_if(parser);
                case KW_WHILE:
                    return parse_stmt_while(parser);
                case KW_DO:
                    return parse_stmt_do_while(parser);
                case KW_FOR:
                    return parse_stmt_for(parser);
                case KW_SWITCH:
                    return parse_stmt_switch(parser);
                case KW_RETURN:
                    return parse_stmt_return(parser);
                case KW_BREAK:
                    return parse_stmt_break(parser);
                case KW_CONTINUE:
                    return parse_stmt_continue(parser);
                case KW_GOTO:
                    return parse_stmt_goto(parser);
                case KW_LABEL:
                    return parse_stmt_label(parser);
                default:
                    return parse_stmt_decl(parser);
            }
        }
        break;
        default:
            return parse_stmt_expr(parser, true);
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

    if (expect_token(parser, TKN_IDENT, error_prefix))
    {
        const char* name = parser->ptoken.as_ident.value;

        if (expect_token(parser, TKN_COLON, error_prefix))
        {
            TypeSpec* type = NULL;
            Expr* expr = NULL;
            bool bad_type = false;
            bool bad_expr = false;

            if (!is_token_kind(parser, TKN_ASSIGN) && !is_token_kind(parser, TKN_SEMICOLON))
            {
                type = parse_typespec(parser);
                bad_type = !type;
            }

            if (match_token(parser, TKN_ASSIGN))
            {
                expr = parse_expr(parser);
                bad_expr = !expr;
            }

            if (!bad_type && !bad_expr)
            {
                if (type || expr)
                {
                    if (expect_token(parser, TKN_SEMICOLON, error_prefix))
                    {
                        range.end = parser->ptoken.range.end;
                        decl = decl_var(parser->ast_arena, name, type, expr, range);
                    }
                }
                else
                {
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

    if (expect_token(parser, TKN_IDENT, error_prefix))
    {
        const char* name = parser->ptoken.as_ident.value;
        ProgRange range = {.start = parser->ptoken.range.start};

        if (expect_token(parser, TKN_COLON, error_prefix))
        {
            TypeSpec* type = parse_typespec(parser);

            if (type)
            {
                range.end = type->range.end;
                param = proc_param(parser->ast_arena, name, type, range);
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

    if (expect_token(parser, TKN_IDENT, error_prefix))
    {
        const char* name = parser->ptoken.as_ident.value;

        if (expect_token(parser, TKN_LPAREN, error_prefix))
        {
            size_t num_params = 0;
            List params = list_head_create(params);
            bool bad_param = false;

            while (!is_token_kind(parser, TKN_RPAREN) && !is_token_kind(parser, TKN_EOF))
            {
                ProcParam* param = parse_proc_param(parser);

                if (param)
                {
                    num_params += 1;
                    list_add(params.prev, &param->lnode);
                }
                else
                {
                    bad_param = true;
                    break;
                }

                if (!match_token(parser, TKN_COMMA))
                    break;
            }

            if (!bad_param && expect_token(parser, TKN_RPAREN, error_prefix))
            {
                TypeSpec* ret = NULL;
                bool bad_ret = false;

                if (match_token(parser, TKN_ARROW))
                {
                    ret = parse_typespec(parser);
                    bad_ret = !ret;
                }

                if (!bad_ret && expect_token(parser, TKN_LBRACE, error_prefix))
                {
                    size_t num_stmts = 0;
                    List stmts = {0};
                    bool ok = parse_fill_stmt_list(parser, &num_stmts, &stmts);

                    if (ok && expect_token(parser, TKN_RBRACE, error_prefix))
                    {
                        range.end = parser->ptoken.range.end;
                        decl = decl_proc(parser->ast_arena, name, num_params, &params, ret, num_stmts, &stmts, range);
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

    if (expect_token(parser, TKN_IDENT, error_prefix))
    {
        const char* name = parser->ptoken.as_ident.value;

        if (expect_token(parser, TKN_LBRACE, error_prefix))
        {
            size_t num_fields = 0;
            List fields = {0};

            if (parse_fill_aggregate_body(parser, &num_fields, &fields) &&
                expect_token(parser, TKN_RBRACE, error_prefix))
            {
                if (num_fields)
                {
                    range.end = parser->ptoken.range.end;
                    decl = decl_aggregate_proc(parser->ast_arena, name, num_fields, &fields, range);
                }
                else
                {
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

    if (expect_token(parser, TKN_IDENT, "Failed to parse enum value"))
    {
        const char* name = parser->ptoken.as_ident.value;
        Expr* value = NULL;
        bool bad_value = false;

        if (match_token(parser, TKN_ASSIGN))
        {
            value = parse_expr(parser);
            bad_value = !value;
        }

        if (!bad_value)
            item = enum_item(parser->ast_arena, name, value);
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

    if (expect_token(parser, TKN_IDENT, error_prefix))
    {
        const char* name = parser->ptoken.as_ident.value;
        TypeSpec* type = NULL;
        bool bad_type = false;

        if (match_token(parser, TKN_COLON))
        {
            type = parse_typespec(parser);
            bad_type = !type;
        }

        if (!bad_type && expect_token(parser, TKN_LBRACE, error_prefix))
        {
            size_t num_items = 0;
            List items = list_head_create(items);
            bool bad_item = false;

            while (!is_token_kind(parser, TKN_RBRACE) && !is_token_kind(parser, TKN_EOF))
            {
                EnumItem* item = parse_enum_item(parser);

                if (item)
                {
                    num_items += 1;
                    list_add(items.prev, &item->lnode);
                }
                else
                {
                    bad_item = true;
                    break;
                }

                if (!match_token(parser, TKN_COMMA))
                    break;
            }

            if (!bad_item && expect_token(parser, TKN_RBRACE, error_prefix))
            {
                if (num_items)
                {
                    range.end = parser->ptoken.range.end;
                    decl = decl_enum(parser->ast_arena, name, type, num_items, &items, range);
                }
                else
                {
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

    if (expect_token(parser, TKN_IDENT, error_prefix))
    {
        const char* name = parser->ptoken.as_ident.value;

        if (expect_token(parser, TKN_ASSIGN, error_prefix))
        {
            TypeSpec* type = parse_typespec(parser);

            if (type && expect_token(parser, TKN_SEMICOLON, error_prefix))
            {
                range.end = parser->ptoken.range.end;
                decl = decl_typedef(parser->ast_arena, name, type, range);
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

    if (expect_token(parser, TKN_IDENT, error_prefix))
    {
        const char* name = parser->ptoken.as_ident.value;

        if (expect_token(parser, TKN_COLON, error_prefix))
        {
            TypeSpec* type = NULL;
            bool bad_type = false;

            if (!is_token_kind(parser, TKN_ASSIGN))
            {
                type = parse_typespec(parser);
                bad_type = type == NULL;
            }

            if (!bad_type && expect_token(parser, TKN_ASSIGN, error_prefix))
            {
                Expr* expr = parse_expr(parser);

                if (expr && expect_token(parser, TKN_SEMICOLON, error_prefix))
                {
                    range.end = parser->ptoken.range.end;
                    decl = decl_const(parser->ast_arena, name, type, expr, range);
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
    if (is_token_kind(parser, TKN_KW))
    {
        switch (parser->token.as_kw.kw)
        {
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
