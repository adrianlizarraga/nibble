#include <assert.h>
#include "parser/internal.h"

///////////////////////////////
//    Parse expressions
//////////////////////////////

static Expr* parse_expr_unary(Parser* parser);

static MemberInitializer* parse_nonindex_member_initializer(Parser* parser)
{
    ProgPos start = parser->token.range.start;
    Designator designator = {0};

    // Try to parse the expression (assuming that it is not named).
    Expr* expr = parse_expr(parser);

    if (!expr) {
        return NULL;
    }

    // If we find a `=` token, then we mistakenly parsed a name as an expression.
    if (match_token(parser, TKN_ASSIGN)) {
        if (expr->kind != CST_ExprIdent) {
            parser_on_error(parser, expr->range, "Initializer designator name must be alphanumeric");
            return NULL;
        }

        ExprIdent* e = (ExprIdent*)expr;

        if (e->ns_ident.num_idents > 1) {
            parser_on_error(parser, expr->range, "Initializer designator name cannot be prefixed by a namespace.");
            return NULL;
        }

        // Save initializer name.
        IdentNode* inode = list_entry(e->ns_ident.idents.prev, IdentNode, lnode);
        designator.kind = DESIGNATOR_NAME;
        designator.name = inode->ident;

        // Free memory.
        mem_free(parser->ast_arena, expr);

        // Re-parse expression.
        expr = parse_expr(parser);

        if (!expr) {
            return NULL;
        }
    }

    ProgRange range = {.start = start, .end = expr->range.end};

    return new_member_initializer(parser->ast_arena, expr, designator, range);
}

static MemberInitializer* parse_index_member_initializer(Parser* parser)
{
    const char* err_prefix = "Failed to parse indexed initializer expression";
    ProgRange range = {.start = parser->token.range.start};
    Designator designator = {.kind = DESIGNATOR_INDEX};

    if (!expect_token(parser, TKN_LBRACKET, err_prefix)) {
        return NULL;
    }

    Expr* index = parse_expr(parser);

    if (!index) {
        return NULL;
    }

    if (!expect_token(parser, TKN_RBRACKET, err_prefix) ||
        !expect_token(parser, TKN_ASSIGN, err_prefix)) {
        return NULL;
    }

    Expr* init = parse_expr(parser);

    if (!init) {
        return NULL;
    }

    range.end = init->range.end;
    designator.index = index;

    return new_member_initializer(parser->ast_arena, init, designator, range);
}

static MemberInitializer* parse_member_initializer(Parser* parser)
{
    if (is_token_kind(parser, TKN_LBRACKET)) {
        return parse_index_member_initializer(parser);
    }

    return parse_nonindex_member_initializer(parser);
}

// expr_compound_lit = '{' expr_init_list (':' typespec)? '}'
// expr_init_list = expr_init_item (',' expr_init_item)*
// expr_init_item = (TKN_IDENT '=')? expr
//               | ('[' (TKN_INT | TKN_IDENT) ']' '=')? expr
static Expr* parse_expr_compound_lit(Parser* parser)
{
    Expr* expr = NULL;
    const char* error_prefix = "Failed to parse compound literal expression";

    if (expect_token(parser, TKN_LBRACE, error_prefix)) {
        ProgRange range = {.start = parser->ptoken.range.start};
        size_t num_initzers = 0;
        List initzers = list_head_create(initzers);
        bool bad_initzer = false;

        while (!is_token_kind(parser, TKN_RBRACE) && !is_token_kind(parser, TKN_COLON) &&
               !is_token_kind(parser, TKN_EOF)) {
            MemberInitializer* initzer = parse_member_initializer(parser);

            if (initzer) {
                num_initzers += 1;
                list_add_last(&initzers, &initzer->lnode);
            }
            else {
                bad_initzer = true;
                break;
            }

            if (!match_token(parser, TKN_COMMA))
                break;
        }

        if (!bad_initzer) {
            TypeSpec* typespec = NULL;

            if (match_token(parser, TKN_COLON)) {
                typespec = parse_typespec(parser);

                if (typespec && expect_token(parser, TKN_RBRACE, error_prefix)) {
                    range.end = parser->ptoken.range.end;
                    expr = new_expr_compound_lit(parser->ast_arena, typespec, num_initzers, &initzers, range);
                }
            }
            else if (expect_token(parser, TKN_RBRACE, error_prefix)) {
                range.end = parser->ptoken.range.end;
                expr = new_expr_compound_lit(parser->ast_arena, typespec, num_initzers, &initzers, range);
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
    const char* error_prefix = "Failed to parse #sizeof expression";

    next_token(parser);

    if (expect_token(parser, TKN_LPAREN, error_prefix)) {
        TypeSpec* typespec = parse_typespec(parser);

        if (typespec && expect_token(parser, TKN_RPAREN, error_prefix)) {
            range.end = parser->ptoken.range.end;
            expr = new_expr_sizeof(parser->ast_arena, typespec, range);
        }
    }

    return expr;
}

static Expr* parse_expr_typeid(Parser* parser)
{
    assert(is_keyword(parser, KW_TYPEID));
    Expr* expr = NULL;
    ProgRange range = {.start = parser->token.range.start};
    const char* error_prefix = "Failed to parse #typeid expression";

    next_token(parser);

    if (expect_token(parser, TKN_LPAREN, error_prefix)) {
        TypeSpec* typespec = parse_typespec(parser);

        if (typespec && expect_token(parser, TKN_RPAREN, error_prefix)) {
            range.end = parser->ptoken.range.end;
            expr = new_expr_typeid(parser->ast_arena, typespec, range);
        }
    }

    return expr;
}

static Expr* parse_expr_offsetof(Parser* parser)
{
    assert(is_keyword(parser, KW_OFFSETOF));
    ProgPos start = parser->token.range.start;

    const char* err_prefix = "Failed to parse #offsetof expression";

    next_token(parser);

    if (!expect_token(parser, TKN_LPAREN, err_prefix)) {
        return NULL;
    }

    TypeSpec* obj_ts = parse_typespec(parser);

    if (!obj_ts) {
        return NULL;
    }

    if (!expect_token(parser, TKN_COMMA, err_prefix)) {
        return NULL;
    }

    if (!expect_token(parser, TKN_IDENT, err_prefix)) {
        return NULL;
    }

    Identifier* field_ident = parser->ptoken.as_ident.ident;

    if (!expect_token(parser, TKN_RPAREN, err_prefix)) {
        return NULL;
    }

    ProgRange range = {.start = start, .end = parser->ptoken.range.end};

    return new_expr_offsetof(parser->ast_arena, obj_ts, field_ident, range);
}

static Expr* parse_expr_indexof(Parser* parser)
{
    assert(is_keyword(parser, KW_INDEXOF));
    ProgPos start = parser->token.range.start;
    const char* err_prefix = "Failed to parse #indexof expression";

    next_token(parser);

    if (!expect_token(parser, TKN_LPAREN, err_prefix)) {
        return NULL;
    }

    TypeSpec* obj_ts = parse_typespec(parser);

    if (!obj_ts) {
        return NULL;
    }

    if (!expect_token(parser, TKN_COMMA, err_prefix)) {
        return NULL;
    }

    if (!expect_token(parser, TKN_IDENT, err_prefix)) {
        return NULL;
    }

    Identifier* field_ident = parser->ptoken.as_ident.ident;

    if (!expect_token(parser, TKN_RPAREN, err_prefix)) {
        return NULL;
    }

    ProgRange range = {.start = start, .end = parser->ptoken.range.end};

    return new_expr_indexof(parser->ast_arena, obj_ts, field_ident, range);
}

static Expr* parse_expr_len(Parser* parser)
{
    assert(is_keyword(parser, KW_LENGTH));
    ProgPos start = parser->token.range.start;
    const char* err_prefix = "Failed to parse #len expression";

    next_token(parser);

    if (!expect_token(parser, TKN_LPAREN, err_prefix)) {
        return NULL;
    }

    Expr* arg = parse_expr(parser);

    if (!arg) {
        return NULL;
    }

    if (!expect_token(parser, TKN_RPAREN, err_prefix)) {
        return NULL;
    }

    ProgRange range = {.start = start, .end = parser->ptoken.range.end};

    return new_expr_length(parser->ast_arena, arg, range);
}

// expr_ident = namespaced_ident
static Expr* parse_expr_ident(Parser* parser)
{
    NSIdent ns_ident = {0};

    if (!parse_namespaced_ident(parser, &ns_ident, "Failed to parse expression identifier")) {
        return NULL;
    }

    return new_expr_ident(parser->ast_arena, &ns_ident);
}

// expr_base = TKN_INT
//           | TKN_FLOAT
//           | TKN_STR
//           | KW_TRUE
//           | KW_FALSE
//           | KW_NULL
//           | expr_ident
//           | expr_compound_init
//           | expr_sizeof
//           | expr_typeid
//           | expr_offsetof
//           | expr_indexof
//           | expr_len
//           | '(' expr ')'
//
// expr_sizeof = KW_SIZEOF '('type_spec')'
static Expr* parse_expr_base(Parser* parser)
{
    Token token = parser->token;

    switch (token.kind) {
    case TKN_INT:
        next_token(parser);
        return new_expr_int(parser->ast_arena, token.as_int, token.range);
    case TKN_FLOAT:
        next_token(parser);
        return new_expr_float(parser->ast_arena, token.as_float.kind, token.as_float.value, token.range);
    case TKN_STR:
        next_token(parser);
        return new_expr_str(parser->ast_arena, token.as_str.str_lit, token.range);
    case TKN_LPAREN: {
        next_token(parser);

        Expr* enclosed = parse_expr(parser);

        if (!enclosed || !expect_token(parser, TKN_RPAREN, NULL)) {
            return NULL;
        }

        enclosed->range.start = token.range.start;
        enclosed->range.end = parser->token.range.end;
        return enclosed;
    } break;
    case TKN_LBRACE:
        return parse_expr_compound_lit(parser);
    case TKN_KW: {
        switch (token.as_kw.ident->kw) {
        case KW_TRUE:
            next_token(parser);
            return new_expr_bool_lit(parser->ast_arena, true, token.range);
        case KW_FALSE:
            next_token(parser);
            return new_expr_bool_lit(parser->ast_arena, false, token.range);
        case KW_NULL:
            next_token(parser);
            return new_expr_null_lit(parser->ast_arena, token.range);
        case KW_SIZEOF:
            return parse_expr_sizeof(parser);
        case KW_TYPEID:
            return parse_expr_typeid(parser);
        case KW_OFFSETOF:
            return parse_expr_offsetof(parser);
        case KW_INDEXOF:
            return parse_expr_indexof(parser);
        case KW_LENGTH:
            return parse_expr_len(parser);
        default:
            break;
        }
    } break;
    case TKN_IDENT: {
        return parse_expr_ident(parser);
    }
    default:
        break;
    }

    // If we got here, we have an unexpected token.
    char tmp[32];

    print_token(&token, tmp, sizeof(tmp));
    parser_on_error(parser, token.range, "Unexpected token `%s` in expression", tmp);

    return NULL;
}

// proc_call_arg = (IDENTIFIER '=')? expr
ProcCallArg* parse_proc_call_arg(Parser* parser)
{
    Identifier* name = NULL;
    Expr* expr = parse_expr(parser);

    if (!expr) {
        return NULL;
    }

    if (match_token(parser, TKN_ASSIGN)) {
        if (expr->kind != CST_ExprIdent) {
            parser_on_error(parser, expr->range, "Argument's name must be an alphanumeric identifier");
            return NULL;
        }

        ExprIdent* e = (ExprIdent*)expr;

        if (e->ns_ident.num_idents > 1) {
            parser_on_error(parser, expr->range, "Argument's name cannot be prefixed by a namespace.");
            return NULL;
        }

        IdentNode* inode = list_entry(e->ns_ident.idents.prev, IdentNode, lnode);
        name = inode->ident;
        mem_free(parser->ast_arena, expr);

        expr = parse_expr(parser);

        if (!expr) {
            return NULL;
        }
    }

    return new_proc_call_arg(parser->ast_arena, expr, name);
}

// expr_base_mod = expr_base ('.' ('[' expr ']' | TKN_IDENT) | '[' expr ']' | '(' proc_call_arg_list* ')' | ':>' typespec | ':>>' typespec)*
// proc_call_arg_list = proc_call_arg (',' proc_call_arg)*
static Expr* parse_expr_base_mod(Parser* parser)
{
    Expr* expr = parse_expr_base(parser);

    while (expr && (is_token_kind(parser, TKN_DOT) || is_token_kind(parser, TKN_LBRACKET) ||
                    is_token_kind(parser, TKN_LPAREN) || is_token_kind(parser, TKN_CAST) ||
                    is_token_kind(parser, TKN_BIT_CAST))) {
        if (match_token(parser, TKN_DOT)) {
            //
            // Struct object field access.
            //

            // Indexed field.
            if (match_token(parser, TKN_LBRACKET)) {
                Expr* index = parse_expr(parser);

                if (index && expect_token(parser, TKN_RBRACKET, "Failed to parse indexed field access")) {
                    ProgRange range = {.start = expr->range.start, .end = parser->ptoken.range.end};
                    expr = new_expr_field_index(parser->ast_arena, expr, index, range);
                }
                else {
                    expr = NULL;
                }
            }
            // Named field.
            else if (expect_token(parser, TKN_IDENT, "Failed to parse field access")) {
                Identifier* field = parser->ptoken.as_ident.ident;
                ProgRange range = {.start = expr->range.start, .end = parser->ptoken.range.end};
                expr = new_expr_field(parser->ast_arena, expr, field, range);
            }
            else {
                expr = NULL;
            }
        }
        else if (match_token(parser, TKN_LBRACKET)) {
            //
            // Array index access.
            //

            Expr* index = parse_expr(parser);

            if (index && expect_token(parser, TKN_RBRACKET, "Failed to parse array access")) {
                ProgRange range = {.start = expr->range.start, .end = parser->ptoken.range.end};
                expr = new_expr_index(parser->ast_arena, expr, index, range);
            }
            else {
                expr = NULL;
            }
        }
        else if (match_token(parser, TKN_LPAREN)) {
            //
            // Procedure call.
            //

            size_t num_args = 0;
            List args = list_head_create(args);
            bool bad_arg = false;

            while (!is_token_kind(parser, TKN_RPAREN) && !is_token_kind(parser, TKN_EOF)) {
                ProcCallArg* arg = parse_proc_call_arg(parser);

                if (arg) {
                    num_args += 1;
                    list_add_last(&args, &arg->lnode);
                }
                else {
                    bad_arg = true;
                    break;
                }

                if (!match_token(parser, TKN_COMMA))
                    break;
            }

            if (!bad_arg && expect_token(parser, TKN_RPAREN, "Failed to parse procedure call")) {
                ProgRange range = {.start = expr->range.start, .end = parser->ptoken.range.end};
                expr = new_expr_call(parser->ast_arena, expr, num_args, &args, range);
            }
            else {
                expr = NULL;
            }
        }
        else if (match_token(parser, TKN_CAST)) {
            //
            // Cast expression.
            //

            TypeSpec* typespec = parse_typespec(parser);

            if (typespec) {
                ProgRange range = {.start = expr->range.start, .end = typespec->range.end};
                expr = new_expr_cast(parser->ast_arena, typespec, expr, false, range);
            }
            else {
                expr = NULL;
            }
        }
        else if (match_token(parser, TKN_BIT_CAST)) {
            //
            // Bit-cast expression.
            //

            TypeSpec* typespec = parse_typespec(parser);

            if (typespec) {
                ProgRange range = {.start = expr->range.start, .end = typespec->range.end};
                expr = new_expr_bit_cast(parser->ast_arena, typespec, expr, range);
            }
            else {
                expr = NULL;
            }
        }
        else {
            char tmp[32]; print_token(&parser->token, tmp, sizeof(tmp));
            NIBBLE_FATAL_EXIT("Unexpected token kind (%s) in parse_expr_base_mod()", tmp);
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
            expr = new_expr_unary(parser->ast_arena, op, unary, range);
        }
    }
    else {
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
            expr = new_expr_binary(parser->ast_arena, op, left, right);
        }
        else {
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
            expr = new_expr_binary(parser->ast_arena, op, left, right);
        }
        else {
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
            expr = new_expr_binary(parser->ast_arena, op, left, right);
        }
        else {
            expr = NULL;
        }
    }

    return expr;
}

// expr_and = expr_cmp ('&&' expr_cmp)*
static Expr* parse_expr_and(Parser* parser)
{
    Expr* expr = parse_expr_cmp(parser);

    while (expr && match_token(parser, TKN_LOGIC_AND)) {
        Expr* left = expr;
        Expr* right = parse_expr_cmp(parser);

        if (right) {
            expr = new_expr_binary(parser->ast_arena, TKN_LOGIC_AND, left, right);
        }
        else {
            expr = NULL;
        }
    }

    return expr;
}

// expr_or = expr_and ('||' expr_and)*
static Expr* parse_expr_or(Parser* parser)
{
    Expr* expr = parse_expr_and(parser);

    while (expr && match_token(parser, TKN_LOGIC_OR)) {
        Expr* left = expr;
        Expr* right = parse_expr_and(parser);

        if (right) {
            expr = new_expr_binary(parser->ast_arena, TKN_LOGIC_OR, left, right);
        }
        else {
            expr = NULL;
        }
    }

    return expr;
}

// expr_ternary = expr_or ('?' expr_ternary ':' expr_ternary)?
static Expr* parse_expr_ternary(Parser* parser)
{
    Expr* expr = parse_expr_or(parser);

    if (expr && match_token(parser, TKN_QUESTION)) {
        Expr* then_expr = parse_expr_ternary(parser);

        if (then_expr) {
            if (expect_token(parser, TKN_COLON, "Failed to parse ternary operator expression")) {
                Expr* else_expr = parse_expr_ternary(parser);

                if (else_expr) {
                    expr = new_expr_ternary(parser->ast_arena, expr, then_expr, else_expr);
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

