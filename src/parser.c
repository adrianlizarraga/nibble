#include "parser.h"
#include "ast.h"

#include <string.h>

static void parser_on_error(Parser* parser, ProgRange range, const char* format, ...)
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

void parser_init(Parser* parser, Allocator* ast_arena, Allocator* tmp_arena, const char* str, ProgPos pos,
                 ErrorStream* errors, ProgPos** line_pos)
{
    memset(parser, 0, sizeof(Parser));

    parser->ast_arena = ast_arena;
    parser->errors = errors;
    parser->lexer = lexer_create(str, pos, tmp_arena, errors, line_pos);
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

static void parser_unexpected_token(Parser* parser, TokenKind expected_kind, const char* error_prefix)
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

///////////////////////////////
//    Parse type specifiers
//////////////////////////////

// aggregate_field = (TKN_IDENT ':')? type_spec ';'
static AggregateField* parse_aggregate_field(Parser* parser)
{
    const char* error_prefix = "Failed to parse field";
    ProgPos start = parser->token.range.start;

    Identifier* ident = NULL;
    TypeSpec* typespec = parse_typespec(parser);

    if (!typespec) {
        return NULL;
    }

    // Parsed a typespec, but was actually meant to be the field's name.
    if (match_token(parser, TKN_COLON)) {
        if (typespec->kind != CST_TypeSpecIdent) {
            parser_on_error(parser, typespec->range, "Field name must be a valid alphanumeric identifier.");
            return NULL;
        }

        TypeSpecIdent* t = (TypeSpecIdent*) typespec;

        if (t->ns_ident.num_idents > 1) {
            parser_on_error(parser, typespec->range, "Field name cannot be prefixed by a namespace.");
            return NULL;
        }

        // Save field name identifer.
        IdentNode* inode = list_entry(t->ns_ident.idents.prev, IdentNode, lnode);
        ident = inode->ident;
        mem_free(parser->ast_arena, typespec); // Free memory.

        typespec = parse_typespec(parser); // Re-parse typespec.

        if (!typespec) {
            return NULL;
        }
    }


    if (!expect_token(parser, TKN_SEMICOLON, error_prefix)) {
        return NULL;
    }

    ProgRange range = {.start = start, .end = parser->ptoken.range.end};

    return new_aggregate_field(parser->ast_arena, ident, typespec, range);
}

// aggregate_body  = '{' aggregate_field* '}'
static bool parse_fill_aggregate_body(Parser* parser, List* fields)
{
    list_head_init(fields);

    while (!is_token_kind(parser, TKN_RBRACE) && !is_token_kind(parser, TKN_EOF)) {
        AggregateField* field = parse_aggregate_field(parser);

        if (!field)
            return false;

        list_add_last(fields, &field->lnode);
    }

    return true;
}

static TypeSpec* parse_typespec_aggregate(Parser* parser, const char* error_prefix,
                                          NewTypeSpecAggregateProc* new_typespec_aggregate)
{
    assert(is_keyword(parser, KW_STRUCT) || is_keyword(parser, KW_UNION));
    ProgRange range = {.start = parser->token.range.start};

    next_token(parser);

    if (!expect_token(parser, TKN_LBRACE, error_prefix)) {
        return NULL;
    }

    List fields = {0};

    if (!parse_fill_aggregate_body(parser, &fields) || !expect_token(parser, TKN_RBRACE, error_prefix)) {
        return NULL;
    }

    range.end = parser->ptoken.range.end;

    if (list_empty(&fields)) {
        parser_on_error(parser, range, "%s: must have at least one field", error_prefix);
        return NULL;
    }

    return new_typespec_aggregate(parser->ast_arena, &fields, range);
}

// typespec_proc_param = (name ':')? typespec
static ProcParam* parse_typespec_proc_param(Parser* parser, bool* is_variadic)
{
    const char* error_prefix = "Failed to parse procedure type";
    ProcParam* param = NULL;

    // Parse variadic parameter type without a name. EX: .. typespec
    if (match_token(parser, TKN_ELLIPSIS)) {
        if (*is_variadic) {
            parser_on_error(parser, parser->ptoken.range, "%s: can only specify one variadic parameter", error_prefix);
            return NULL;
        }

        *is_variadic = true;

        TypeSpec* typespec = parse_typespec(parser);

        if (match_token(parser, TKN_COLON)) {
            parser_on_error(parser, parser->ptoken.range, "%s: `..` must appear before parameter type", error_prefix);
            return NULL;
        }

        param = new_proc_param(parser->ast_arena, NULL, typespec, *is_variadic, typespec->range);
    }
    else {
        TypeSpec* typespec = parse_typespec(parser);

        if (!typespec) {
            return NULL;
        }

        // Parse a potentially variadic parameter type with a name. EX: `x : int` or `x : ..int`.
        if (match_token(parser, TKN_COLON)) {
            if (typespec->kind != CST_TypeSpecIdent) {
                parser_on_error(parser, typespec->range, "Parameter's name must be an alphanumeric identifier");
                return NULL;
            }

            ProgPos start = typespec->range.start;
            TypeSpecIdent* tident = (TypeSpecIdent*)typespec;

            if (tident->ns_ident.num_idents > 1) {
                parser_on_error(parser, typespec->range, "Parameter name cannot be prefixed by a namespace.");
                return NULL;
            }

            IdentNode* inode = list_entry(tident->ns_ident.idents.prev, IdentNode, lnode);
            Identifier* name = inode->ident;

            mem_free(parser->ast_arena, typespec);

            if (match_token(parser, TKN_ELLIPSIS)) {
                if (*is_variadic) {
                    ProgRange err_range = {.start = start, .end = parser->token.range.end};
                    parser_on_error(parser, err_range, "%s: can only specify one variadic parameter", error_prefix);
                    return NULL;
                }

                *is_variadic = true;
            }
            else if (*is_variadic) {
                ProgRange err_range = {.start = start, .end = parser->token.range.end};
                parser_on_error(parser, err_range, "%s: variadic parameter must appear last in the parameter list", error_prefix);
                return NULL;
            }

            typespec = parse_typespec(parser);

            if (!typespec) {
                return NULL;
            }

            ProgRange range =  {.start = start, .end = typespec->range.end};
            param = new_proc_param(parser->ast_arena, name, typespec, *is_variadic, range);
        }
        // Parameter type does not have a name and is not variadic.
        else {
            param = new_proc_param(parser->ast_arena, NULL, typespec, false, typespec->range);
        }
    }

    return param;
}

// typespec_proc = 'proc' '(' proc_param_list? ')' ('=>' typespec)?
//
// proc_param_list = typespec_proc_param (',' typespec_proc_param)*
static TypeSpec* parse_typespec_proc(Parser* parser)
{
    assert(is_keyword(parser, KW_PROC));
    ProgRange range = {.start = parser->token.range.start};
    const char* error_prefix = "Failed to parse procedure type specification";

    next_token(parser);

    if (!expect_token(parser, TKN_LPAREN, error_prefix)) {
        return NULL;
    }

    size_t num_params = 0;
    List params = list_head_create(params);
    bool is_variadic = false;

    while (!is_token_kind(parser, TKN_RPAREN) && !is_token_kind(parser, TKN_EOF)) {
        ProcParam* param = parse_typespec_proc_param(parser, &is_variadic);

        if (!param) {
            return NULL;
        }

        num_params += 1;
        list_add_last(&params, &param->lnode);

        if (!match_token(parser, TKN_COMMA))
            break;
    }

    if (!expect_token(parser, TKN_RPAREN, error_prefix)) {
        return NULL;
    }

    TypeSpec* ret = NULL;

    if (match_token(parser, TKN_ARROW)) {
        ret = parse_typespec(parser);

        if (!ret) {
            return NULL;
        }
    }

    range.end = parser->ptoken.range.end;
    return new_typespec_proc(parser->ast_arena, num_params, &params, ret, is_variadic, range);
}

// namespaced_ident = (TKN_IDENT '::')* TKN_IDENT
static bool parse_namespaced_ident(Parser* parser, NSIdent* ns_ident)
{
    ns_ident->range = parser->token.range;
    ns_ident->num_idents = 0;
    list_head_init(&ns_ident->idents);

    // Keep parsing identifiers as long as we see a `::` token.
    do {
        if (!expect_token(parser, TKN_IDENT, "Failed to parse identifier")) {
            return false;
        }

        Token* ptoken = &parser->ptoken;

        IdentNode* inode = alloc_type(parser->ast_arena, IdentNode, true);
        inode->ident = ptoken->as_ident.ident;

        list_add_last(&ns_ident->idents, &inode->lnode);
        ns_ident->num_idents += 1;
        ns_ident->range.end = ptoken->range.end;
    } while (match_token(parser, TKN_DBL_COLON));

    return true;
}

// typespec_ident = namespaced_ident
static TypeSpec* parse_typespec_ident(Parser* parser)
{
    NSIdent ns_ident = {0};

    if (!parse_namespaced_ident(parser, &ns_ident)) {
        return NULL;
    }

    return new_typespec_ident(parser->ast_arena, &ns_ident);
}

static TypeSpec* parse_typespec_typeof(Parser* parser)
{
    assert(is_keyword(parser, KW_TYPEOF));
    ProgRange range = {.start = parser->token.range.start};
    const char* error_prefix = "Failed to parse typeof expression";

    next_token(parser);

    if (!expect_token(parser, TKN_LPAREN, error_prefix)) {
        return NULL;
    }

    Expr* arg = parse_expr(parser);

    if (!arg) {
        return NULL;
    }

    if (!expect_token(parser, TKN_RPAREN, error_prefix)) {
        return NULL;
    }

    range.end = parser->ptoken.range.end;

    return new_typespec_typeof(parser->ast_arena, arg, range);
}

// typespec_base  = typespec_proc
//                | typespec_anon_struct
//                | typespec_anon_union
//                | typespec_typeof
//                | typespec_ident
//                | '(' type_spec ')'
static TypeSpec* parse_typespec_base(Parser* parser)
{
    Token token = parser->token;

    switch (token.kind) {
    case TKN_KW: {
        switch (token.as_kw.ident->kw) {
        case KW_PROC:
            return parse_typespec_proc(parser);
        case KW_STRUCT:
            return parse_typespec_aggregate(parser, "Failed to parse anonymous struct", new_typespec_struct);
        case KW_UNION:
            return parse_typespec_aggregate(parser, "Failed to parse anonymous union", new_typespec_union);
        case KW_TYPEOF:
            return parse_typespec_typeof(parser);
        default:
            break;
        }
    } break;
    case TKN_IDENT:
        return parse_typespec_ident(parser);
    case TKN_LPAREN: {
        next_token(parser);

        TypeSpec* typespec = NULL;
        TypeSpec* enclosed_typespec = parse_typespec(parser);

        if (enclosed_typespec && expect_token(parser, TKN_RPAREN, NULL)) {
            typespec = enclosed_typespec;
        }

        return typespec;
    } break;
    default:
        break;
    }

    // If we got here, we have an unexpected token.
    char tmp[32];

    print_token(&token, tmp, sizeof(tmp));
    parser_on_error(parser, token.range, "Unexpected token `%s` in type specification", tmp);

    return NULL;
}

TypeSpec* parse_array_typespec(Parser* parser)
{
    const char* err_prefix = "Failed to parse array type specification";
    ProgRange range = {.start = parser->token.range.start};

    if (!expect_token(parser, TKN_LBRACKET, err_prefix)) {
        return NULL;
    }

    Expr* len = NULL;
    bool infer_len = false;

    if (match_keyword(parser, KW_UNDERSCORE)) {
        infer_len = true;
    }
    else if (!is_token_kind(parser, TKN_RBRACKET)) {
        len = parse_expr(parser);

        if (!len) {
            return NULL;
        }
    }

    if (!expect_token(parser, TKN_RBRACKET, err_prefix)) {
        return NULL;
    }

    TypeSpec* base = parse_typespec(parser);

    if (!base) {
        return NULL;
    }

    range.end = base->range.end;

    assert((len && !infer_len) || !len); // TODO: Remove

    return new_typespec_array(parser->ast_arena, base, len, infer_len, range);
}

// typespec = ('^' | '[' expr? ']' | KW_CONST) typespec
//          | typespec_base
TypeSpec* parse_typespec(Parser* parser)
{
    TypeSpec* typespec = NULL;

    if (match_token(parser, TKN_CARET)) {
        //
        // Pointer typespec.
        //

        ProgRange range = {.start = parser->ptoken.range.start};
        TypeSpec* base = parse_typespec(parser);

        if (base) {
            range.end = base->range.end;
            typespec = new_typespec_ptr(parser->ast_arena, base, range);
        }
    }
    else if (is_token_kind(parser, TKN_LBRACKET)) {
        //
        // Array typespec.
        //
        typespec = parse_array_typespec(parser);
    }
    else if (match_keyword(parser, KW_CONST)) {
        //
        // Const typespec
        //

        ProgRange range = {.start = parser->ptoken.range.start};
        TypeSpec* base = parse_typespec(parser);

        if (base) {
            range.end = base->range.end;
            typespec = new_typespec_const(parser->ast_arena, base, range);
        }
    }
    else {
        typespec = parse_typespec_base(parser);
    }

    return typespec;
}

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

    if (!parse_namespaced_ident(parser, &ns_ident)) {
        return NULL;
    }

    return new_expr_ident(parser->ast_arena, &ns_ident);
}

// expr_base = TKN_INT
//           | TKN_FLOAT
//           | TKN_STR
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
static ProcCallArg* parse_proc_call_arg(Parser* parser)
{
    Identifier* name = NULL;
    Expr* expr = parse_expr(parser);

    if (!expr) {
        return NULL;
    }

    if (match_token(parser, TKN_ASSIGN)) {
        if (expr->kind != CST_ExprIdent) {
            parser_on_error(parser, expr->range, "Procedure argument's name must be an alphanumeric identifier");
            return NULL;
        }

        ExprIdent* e = (ExprIdent*)expr;

        if (e->ns_ident.num_idents > 1) {
            parser_on_error(parser, expr->range, "Argument name cannot be prefixed by a namespace.");
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

// expr_base_mod = expr_base ('.' ('[' expr ']' | TKN_IDENT) | '[' expr ']' | '(' proc_call_arg_list* ')' | ':>' typespec)*
// proc_call_arg_list = proc_call_arg (',' proc_call_arg)*
static Expr* parse_expr_base_mod(Parser* parser)
{
    Expr* expr = parse_expr_base(parser);

    while (expr && (is_token_kind(parser, TKN_DOT) || is_token_kind(parser, TKN_LBRACKET) ||
                    is_token_kind(parser, TKN_LPAREN) || is_token_kind(parser, TKN_CAST))) {
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
        else {
            //
            // Cast expression.
            //

            assert(is_token_kind(parser, TKN_CAST));
            next_token(parser);

            TypeSpec* typespec = parse_typespec(parser);

            if (typespec) {
                ProgRange range = {.start = expr->range.start, .end = typespec->range.end};
                expr = new_expr_cast(parser->ast_arena, typespec, expr, false, range);
            }
            else {
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

///////////////////////////////
//    Parse statements
//////////////////////////////

typedef struct StmtBlockBody {
    List stmts;
    u32 num_decls;
} StmtBlockBody;

static bool parse_fill_stmt_block_body(Parser* parser, StmtBlockBody* body, const char* err_prefix)
{
    assert(is_token_kind(parser, TKN_LBRACE));

    list_head_init(&body->stmts);
    body->num_decls = 0;

    next_token(parser);

    while (!is_token_kind(parser, TKN_RBRACE) && !is_token_kind(parser, TKN_EOF)) {
        Stmt* item = parse_stmt(parser);

        if (!item)
            return false;

        if (item->kind == CST_StmtDecl)
            body->num_decls += 1;

        list_add_last(&body->stmts, &item->lnode);
    }

    return expect_token(parser, TKN_RBRACE, err_prefix);
}

static Stmt* parse_stmt_block(Parser* parser)
{
    assert(is_token_kind(parser, TKN_LBRACE));
    Stmt* stmt = NULL;
    ProgRange range = {.start = parser->token.range.start};

    StmtBlockBody body = {0};

    if (parse_fill_stmt_block_body(parser, &body, "Failed to parse end of statement block")) {
        range.end = parser->ptoken.range.end;
        stmt = new_stmt_block(parser->ast_arena, &body.stmts, body.num_decls, range);
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

// stmt_if = 'if' '(' expr ')' stmt ('else' stmt)?
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
        bool bad_else = false;
        ElseBlock else_blk = {0};

        if (is_keyword(parser, KW_ELSE))
            bad_else = !parse_fill_else_block(parser, &else_blk);

        if (!bad_else)
            stmt = new_stmt_if(parser->ast_arena, &if_blk, &else_blk, range);
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
    if (!is_token_kind(parser, TKN_COLON)) {
        start = parse_expr(parser);

        if (!start)
            return NULL;

        if (match_token(parser, TKN_ELLIPSIS)) {
            end = parse_expr(parser);

            if (!end)
                return NULL;
        }
    }

    if (!expect_token(parser, TKN_COLON, error_prefix))
        return NULL;

    SwitchCase* swcase = NULL;
    List stmts = list_head_create(stmts);
    bool bad_stmt = false;

    // Parse case statements
    while (!is_keyword(parser, KW_CASE) && !is_token_kind(parser, TKN_RBRACE) && !is_token_kind(parser, TKN_EOF)) {
        Stmt* stmt = parse_stmt(parser);

        if (!stmt) {
            bad_stmt = true;
            break;
        }

        list_add_last(&stmts, &stmt->lnode);
    }

    if (!bad_stmt) {
        range.end = parser->ptoken.range.end;
        swcase = new_switch_case(parser->ast_arena, start, end, &stmts, range);
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
    List cases = list_head_create(cases);
    bool has_default = false;
    bool bad_case = false;

    do {
        SwitchCase* swcase = parse_switch_case(parser);

        if (!swcase) {
            bad_case = true;
            break;
        }

        bool is_default = !swcase->start && !swcase->end;

        if (has_default && is_default) {
            parser_on_error(parser, swcase->range, "Switch statement can have at most one default case");
            bad_case = true;
            break;
        }

        has_default = has_default || is_default;

        list_add_last(&cases, &swcase->lnode);
    } while (is_keyword(parser, KW_CASE));

    if (!bad_case && expect_token(parser, TKN_RBRACE, error_prefix)) {
        range.end = parser->ptoken.range.end;
        stmt = new_stmt_switch(parser->ast_arena, expr, &cases, range);
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

    return new_stmt_while(parser->ast_arena, cond, body, range);
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

    if (body && expect_keyword(parser, KW_WHILE, error_prefix) && expect_token(parser, TKN_LPAREN, error_prefix)) {
        Expr* cond = parse_expr(parser);

        if (cond && expect_token(parser, TKN_RPAREN, error_prefix) &&
            expect_token(parser, TKN_SEMICOLON, error_prefix)) {
            range.end = parser->ptoken.range.end;
            stmt = new_stmt_do_while(parser->ast_arena, cond, body, range);
        }
    }

    return stmt;
}

static Stmt* parse_stmt_decl(Parser* parser)
{
    Stmt* stmt = NULL;
    Decl* decl = parse_decl(parser);

    if (decl)
        stmt = new_stmt_decl(parser->ast_arena, decl);

    return stmt;
}

static Stmt* parse_stmt_expr(Parser* parser, bool terminate)
{
    Stmt* stmt = NULL;
    Expr* expr = parse_expr(parser);

    if (!expr) {
        return NULL;
    }

    ProgRange range = {.start = expr->range.start};
    const char* error_prefix = "Failed to parse expression statement";

    if (expr) {
        if (is_token_prop_kind(parser, OP_ASSIGN)) {
            TokenKind op_assign = parser->token.kind;

            next_token(parser);

            Expr* rexpr = parse_expr(parser);

            if (rexpr && !terminate) {
                range.end = rexpr->range.end;
                stmt = new_stmt_expr_assign(parser->ast_arena, expr, op_assign, rexpr, range);
            }
            else if (rexpr && expect_token(parser, TKN_SEMICOLON, error_prefix)) {
                range.end = parser->ptoken.range.end;
                stmt = new_stmt_expr_assign(parser->ast_arena, expr, op_assign, rexpr, range);
            }
        }
        else if (!terminate) {
            range.end = expr->range.end;
            stmt = new_stmt_expr(parser->ast_arena, expr, range);
        }
        else if (expect_token(parser, TKN_SEMICOLON, error_prefix)) {
            range.end = parser->ptoken.range.end;
            stmt = new_stmt_expr(parser->ast_arena, expr, range);
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

    if (!match_token(parser, TKN_SEMICOLON)) {
        if (is_keyword(parser, KW_VAR) || is_token_kind(parser, TKN_AT))
            init = parse_stmt_decl(parser);
        else
            init = parse_stmt_expr(parser, true);

        if (!init)
            return NULL;
    }

    Expr* cond = NULL;

    if (!match_token(parser, TKN_SEMICOLON)) {
        cond = parse_expr(parser);

        if (!cond || !expect_token(parser, TKN_SEMICOLON, error_prefix))
            return NULL;
    }

    Stmt* next = NULL;

    if (!is_token_kind(parser, TKN_RPAREN)) {
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

    return new_stmt_for(parser->ast_arena, init, cond, next, body, range);
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
    if (is_token_kind(parser, TKN_RBRACE)) {
        parser_on_error(parser, parser->token.range,
                        "Failed to parse return statement: wanted `;` or expression, but got `}`");

        return NULL;
    }

    if (!is_token_kind(parser, TKN_SEMICOLON)) {
        expr = parse_expr(parser);

        if (!expr)
            return NULL;
    }

    if (!expect_token(parser, TKN_SEMICOLON, "Failed to parse return statement"))
        return NULL;

    range.end = parser->ptoken.range.end;

    return new_stmt_return(parser->ast_arena, expr, range);
}

// stmt_break = 'break' TKN_IDENT? ';'
static Stmt* parse_stmt_break(Parser* parser)
{
    assert(is_keyword(parser, KW_BREAK));
    ProgRange range = {.start = parser->token.range.start};
    const char* label = NULL;

    next_token(parser);

    if (match_token(parser, TKN_IDENT))
        label = parser->ptoken.as_ident.ident->str;

    if (!expect_token(parser, TKN_SEMICOLON, "Failed to parse break statement"))
        return NULL;

    range.end = parser->ptoken.range.end;

    return new_stmt_break(parser->ast_arena, label, range);
}

// stmt_continue = 'continue' TKN_IDENT? ';'
static Stmt* parse_stmt_continue(Parser* parser)
{
    assert(is_keyword(parser, KW_CONTINUE));
    ProgRange range = {.start = parser->token.range.start};
    const char* label = NULL;

    next_token(parser);

    if (match_token(parser, TKN_IDENT))
        label = parser->ptoken.as_ident.ident->str;

    if (!expect_token(parser, TKN_SEMICOLON, "Failed to parse continue statement"))
        return NULL;

    range.end = parser->ptoken.range.end;

    return new_stmt_continue(parser->ast_arena, label, range);
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

    label = parser->ptoken.as_ident.ident->str;

    if (!expect_token(parser, TKN_SEMICOLON, error_prefix))
        return NULL;

    range.end = parser->ptoken.range.end;

    return new_stmt_goto(parser->ast_arena, label, range);
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

    const char* label = parser->ptoken.as_ident.ident->str;

    if (!expect_token(parser, TKN_COLON, error_prefix))
        return NULL;

    Stmt* stmt = parse_stmt(parser);

    if (!stmt) {
        return NULL;
    }

    range.end = stmt->range.end;

    return new_stmt_label(parser->ast_arena, label, stmt, range);
}

// stmt_static_assert = '#static_assert' '(' expr (',' TKN_STR)? ')' ';'
static Stmt* parse_stmt_static_assert(Parser* parser)
{
    assert(is_keyword(parser, KW_STATIC_ASSERT));
    ProgRange range = {.start = parser->token.range.start};
    const char* error_prefix = "Failed to parse #static_assert";

    next_token(parser);

    if (!expect_token(parser, TKN_LPAREN, error_prefix)) {
        return NULL;
    }

    Expr* cond = parse_expr(parser);

    if (!cond) {
        return NULL;
    }

    StrLit* msg = NULL;

    if (match_token(parser, TKN_COMMA)) {
        if (!expect_token(parser, TKN_STR, error_prefix)) {
            return NULL;
        }

        msg = parser->ptoken.as_str.str_lit;
    }

    if (!expect_token(parser, TKN_RPAREN, error_prefix)) {
        return NULL;
    }

    if (!expect_token(parser, TKN_SEMICOLON, error_prefix)) {
        return NULL;
    }

    range.end = parser->ptoken.range.end;

    return new_stmt_static_assert(parser->ast_arena, cond, msg, range);
}

static bool is_mod_path_relative(const char* path, size_t len)
{
    assert(path);

    if (len < 3) {
        return false;
    }

    char c0 = path[0];
    char c1 = path[1];
    char c2 = path[2];

    return (c0 == '/') || (c0 == '.' && ((c1 == '/') || (c1 == '.' && c2 == '/')));
}

// port_sym = TKN_IDENT ('as' TKN_IDENT)?
static PortSymbol* parse_port_symbol(Parser* parser)
{
    ProgRange range = parser->token.range;
    const char* error_prefix = "Failed to parse import symbol";

    if (!expect_token(parser, TKN_IDENT, error_prefix)) {
        return NULL;
    }

    Identifier* name = parser->ptoken.as_ident.ident;
    Identifier* rename = NULL;

    if (match_keyword(parser, KW_AS)) {
        if (!expect_token(parser, TKN_IDENT, error_prefix)) {
            return NULL;
        }

        Token* ptoken = &parser->ptoken;

        rename = ptoken->as_ident.ident;
        range.end = ptoken->range.end;
    }

    return new_port_symbol(parser->ast_arena, name, rename, range);
}

// stmt_export = 'export' '{' export_syms '}' ';'
// export_syms = port_sym (',' port_sym)*
static Stmt* parse_stmt_export(Parser* parser)
{
    assert(is_keyword(parser, KW_EXPORT));
    ProgRange range = {.start = parser->token.range.start};
    const char* error_prefix = "Failed to parse export statement";

    next_token(parser);

    // Parse export syms
    List export_syms = list_head_create(export_syms);

    if (!expect_token(parser, TKN_LBRACE, error_prefix)) {
        return NULL;
    }

    // Parse the first export symbol.
    PortSymbol* esym_1 = parse_port_symbol(parser);

    if (!esym_1) {
        return NULL;
    }

    list_add_last(&export_syms, &esym_1->lnode);
    size_t num_exports = 1;

    // Parse the rest, if any.
    while (match_token(parser, TKN_COMMA)) {
        PortSymbol* esym = parse_port_symbol(parser);

        if (!esym) {
            return NULL;
        }

        list_add_last(&export_syms, &esym->lnode);
        num_exports += 1;
    }

    if (!expect_token(parser, TKN_RBRACE, error_prefix)) {
        return NULL;
    }

    if (!expect_token(parser, TKN_SEMICOLON, error_prefix)) {
        return NULL;
    }

    range.end = parser->ptoken.range.end;

    return new_stmt_export(parser->ast_arena, num_exports, &export_syms, range);
}

// stmt_import = 'import' ('{' import_syms '}' 'from' )? TKN_STR ('as' TKN_IDENT)? ';'
// import_syms = port_sym (',' port_sym)*
static Stmt* parse_stmt_import(Parser* parser)
{
    assert(is_keyword(parser, KW_IMPORT));
    ProgRange range = {.start = parser->token.range.start};
    const char* error_prefix = "Failed to parse import statement";

    next_token(parser);

    // Parse import syms
    List import_syms = list_head_create(import_syms);
    size_t num_imports = 0;

    if (match_token(parser, TKN_LBRACE)) {
        // Parse the first import symbol.
        PortSymbol* isym_1 = parse_port_symbol(parser);

        if (!isym_1) {
            return NULL;
        }

        list_add_last(&import_syms, &isym_1->lnode);
        num_imports += 1;

        // Parse the rest, if any.
        while (match_token(parser, TKN_COMMA)) {
            PortSymbol* isym = parse_port_symbol(parser);

            if (!isym) {
                return NULL;
            }

            list_add_last(&import_syms, &isym->lnode);
            num_imports += 1;
        }

        if (!expect_token(parser, TKN_RBRACE, error_prefix)) {
            return NULL;
        }

        if (!expect_keyword(parser, KW_FROM, error_prefix)) {
            return NULL;
        }
    }

    if (!expect_token(parser, TKN_STR, error_prefix)) {
        return NULL;
    }

    StrLit* mod_pathname = parser->ptoken.as_str.str_lit;

    // TODO: Support non-relative module paths (when support a node_modules-like module repository).
    if (!is_mod_path_relative(mod_pathname->str, mod_pathname->len)) {
        parser_on_error(parser, parser->ptoken.range,
                        "Module import path must be relative (i.e., starts with `./`, `../`, or `/`)");
        return NULL;
    }

    Identifier* mod_namespace = NULL;

    if (match_keyword(parser, KW_AS)) {
        if (!expect_token(parser, TKN_IDENT, error_prefix)) {
            return NULL;
        }

        mod_namespace = parser->ptoken.as_ident.ident;
    }

    if (!expect_token(parser, TKN_SEMICOLON, error_prefix)) {
        return NULL;
    }

    range.end = parser->ptoken.range.end;

    return new_stmt_import(parser->ast_arena, num_imports, &import_syms, mod_pathname, mod_namespace, range);
}

// stmt_include = '#include' TKN_STR ';'
static Stmt* parse_stmt_include(Parser* parser)
{
    assert(is_keyword(parser, KW_INCLUDE));
    ProgRange range = {.start = parser->token.range.start};
    const char* error_prefix = "Failed to parse include statement";

    next_token(parser);

    if (!expect_token(parser, TKN_STR, error_prefix)) {
        return NULL;
    }

    StrLit* file_pathname = parser->ptoken.as_str.str_lit;

    if (!is_mod_path_relative(file_pathname->str, file_pathname->len)) {
        parser_on_error(parser, parser->ptoken.range,
                        "Include path must be relative (i.e., starts with `./`, `../`, or `/`)");
        return NULL;
    }

    if (!expect_token(parser, TKN_SEMICOLON, error_prefix)) {
        return NULL;
    }

    range.end = parser->ptoken.range.end;

    return new_stmt_include(parser->ast_arena, file_pathname, range);
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
//      | stmt_static_assert
//      | stmt_block
//      | expr ';'
//      | expr_assign ';'
Stmt* parse_stmt(Parser* parser)
{
    Token token = parser->token;

    switch (token.kind) {
    case TKN_SEMICOLON:
        next_token(parser);
        return new_stmt_noop(parser->ast_arena, token.range);
    case TKN_LBRACE:
        return parse_stmt_block(parser);
    case TKN_KW:
        switch (token.as_kw.ident->kw) {
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
        case KW_STATIC_ASSERT:
            return parse_stmt_static_assert(parser);
        default:
            return parse_stmt_decl(parser);
        }
    default:
        return parse_stmt_expr(parser, true);
    }
}

// global_stmt = stmt_static_assert
//             | stmt_import
//             | stmt_export
//             | stmt_include
//             | stmt_decl
Stmt* parse_global_stmt(Parser* parser)
{
    switch (parser->token.kind) {
    case TKN_KW:
        switch (parser->token.as_kw.ident->kw) {
        case KW_STATIC_ASSERT:
            return parse_stmt_static_assert(parser);
        case KW_IMPORT:
            return parse_stmt_import(parser);
        case KW_EXPORT:
            return parse_stmt_export(parser);
        case KW_INCLUDE:
            return parse_stmt_include(parser);
        default:
            return parse_stmt_decl(parser);
        }
    case TKN_AT:
        return parse_stmt_decl(parser);
    default:
        break;
    }

    char tmp[32];

    print_token(&parser->token, tmp, sizeof(tmp));
    parser_on_error(parser, parser->token.range,
                    "Only declarations or compile-time statements are allowed at global scope."
                    " Found token `%s`", tmp);

    return NULL;
}

///////////////////////////////
//    Parse declarations
//////////////////////////////

static DeclAnnotation* parse_annotation(Parser* parser)
{
    assert(is_token_kind(parser, TKN_AT));
    ProgRange range = {.start = parser->token.range.start};

    next_token(parser);

    if (!expect_token(parser, TKN_IDENT, "Failed to parse annotation")) {
        return NULL;
    }

    Identifier* name = parser->ptoken.as_ident.ident;
    range.end = parser->ptoken.range.end;

    return new_annotation(parser->ast_arena, name, range);
}

static bool parse_annotations(Parser* parser, List* annotations)
{
    list_head_init(annotations);

    while (is_token_kind(parser, TKN_AT)) {
        DeclAnnotation* a = parse_annotation(parser);

        if (!a) {
            return false;
        }

        list_add_last(annotations, &a->lnode);
    }

    return true;
}

// decl_var = KW_VAR TKN_IDENT ':' type_spec? ('=' expr)? ';'
//
// Ex 1: var x : int = 0;
// Ex 2: var x := 0;
// Ex 3: var x :int;
static Decl* parse_decl_var(Parser* parser)
{
    ProgRange range = {.start = parser->token.range.start};
    const char* error_prefix = "Failed to parse var declaration";

    // Parse 'var' keyword.
    if (!expect_keyword(parser, KW_VAR, error_prefix)) {
        return NULL;
    }

    // Parse variable name.
    if (!expect_token(parser, TKN_IDENT, error_prefix)) {
        return NULL;
    }

    Identifier* name = parser->ptoken.as_ident.ident;

    // Parse ':'
    if (!expect_token(parser, TKN_COLON, error_prefix)) {
        return NULL;
    }

    TypeSpec* typespec = NULL;
    Expr* expr = NULL;
    bool uninit = false;

    // Parse type
    if (!is_token_kind(parser, TKN_ASSIGN) && !is_token_kind(parser, TKN_SEMICOLON)) {
        typespec = parse_typespec(parser);

        if (!typespec) {
            return NULL;
        }
    }

    // Parse '=' and initial value.
    if (match_token(parser, TKN_ASSIGN)) {
        if (match_token(parser, TKN_UNINIT)) {
            uninit = true;
        }
        else {
            expr = parse_expr(parser);

            if (!expr) {
                return NULL;
            }
        }
    }

    if (!typespec && !expr && !uninit) {
        range.end = parser->ptoken.range.end;
        parser_on_error(parser, range, "A var declaration must have either a type or an initial value");
        return NULL;
    }

    // Parse ending ';'
    if (!expect_token(parser, TKN_SEMICOLON, error_prefix)) {
        return NULL;
    }

    range.end = parser->ptoken.range.end;
    unsigned flags = uninit ? DECL_VAR_IS_UNINIT : 0;

    return new_decl_var(parser->ast_arena, name, typespec, expr, flags, range);
}

// proc_param = TKN_IDENT ':' type_spec
static Decl* parse_proc_param(Parser* parser, bool* is_variadic)
{
    const char* error_prefix = "Failed to parse procedure parameter";

    if (!expect_token(parser, TKN_IDENT, error_prefix))
        return NULL;

    Identifier* name = parser->ptoken.as_ident.ident;
    ProgPos start = parser->ptoken.range.start;

    if (!expect_token(parser, TKN_COLON, error_prefix))
        return NULL;

    if (match_token(parser, TKN_ELLIPSIS)) {
        if (*is_variadic) {
            ProgRange err_range = {.start = start, .end = parser->token.range.end};
            parser_on_error(parser, err_range, "%s: can only specify one variadic parameter", error_prefix);
            return NULL;
        }


        *is_variadic = true;
    }
    else if (*is_variadic) {
        ProgRange err_range = {.start = start, .end = parser->token.range.end};
        parser_on_error(parser, err_range, "%s: variadic parameter must appear last in the parameter list", error_prefix);
        return NULL;
    }

    TypeSpec* typespec = parse_typespec(parser);

    if (!typespec) {
        return NULL;
    }

    ProgRange range = {.start = start, .end = typespec->range.end};
    unsigned flags = *is_variadic ? DECL_VAR_IS_VARIADIC : 0;

    return new_decl_var(parser->ast_arena, name, typespec, NULL, flags, range);
}

// decl_proc  = 'proc' TKN_IDENT '(' param_list ')' ('=>' typespec)? stmt_block
// proc_param_list = proc_param (',' proc_param)*
static Decl* parse_decl_proc(Parser* parser)
{
    assert(is_keyword(parser, KW_PROC));
    ProgRange range = {.start = parser->token.range.start};
    const char* error_prefix = "Failed to parse procedure declaration";

    next_token(parser);

    // Parse procedure name
    if (!expect_token(parser, TKN_IDENT, error_prefix)) {
        return NULL;
    }

    Identifier* name = parser->ptoken.as_ident.ident;

    // Parse opening parenthesis.
    if (!expect_token(parser, TKN_LPAREN, error_prefix)) {
        return NULL;
    }

    // Parse parameters
    u32 num_params = 0;
    bool is_variadic = false;
    List params = list_head_create(params);

    while (!is_token_kind(parser, TKN_RPAREN) && !is_token_kind(parser, TKN_EOF)) {
        Decl* param = parse_proc_param(parser, &is_variadic);

        if (!param) {
            return NULL;
        }

        num_params += 1;
        list_add_last(&params, &param->lnode);

        if (!match_token(parser, TKN_COMMA)) {
            break;
        }
    }

    // Parse closing parenthesis.
    if (!expect_token(parser, TKN_RPAREN, error_prefix)) {
        return NULL;
    }

    // Parse return value type.
    TypeSpec* ret = NULL;

    if (match_token(parser, TKN_ARROW)) {
        ret = parse_typespec(parser);

        if (!ret) {
            return NULL;
        }
    }

    Decl* decl = NULL;
    StmtBlockBody body = {0};

    // Parse procedure body.
    if (is_token_kind(parser, TKN_LBRACE)) {
        if (parse_fill_stmt_block_body(parser, &body, error_prefix)) {
            range.end = parser->ptoken.range.end;
            decl = new_decl_proc(parser->ast_arena, name, num_params, &params, ret, &body.stmts,
                                 body.num_decls, false, is_variadic, range);
        }
    }
    // Parse end of incomplete procedure.
    else if (match_token(parser, TKN_SEMICOLON)) {
        list_head_init(&body.stmts);
        range.end = parser->ptoken.range.end;
        decl = new_decl_proc(parser->ast_arena, name, num_params, &params, ret, &body.stmts,
                             body.num_decls, true, is_variadic, range);
    }
    // Unexpected token error.
    else {
        parser_unexpected_token(parser, TKN_LBRACE, error_prefix);
    }

    return decl;
}

// decl_union  = 'union' aggregate_body
// decl_struct = 'struct' aggregate_body
static Decl* parse_decl_aggregate(Parser* parser, const char* error_prefix, NewDeclAggregateProc* new_decl_aggregate)
{
    assert(is_keyword(parser, KW_STRUCT) || is_keyword(parser, KW_UNION));
    ProgRange range = {.start = parser->token.range.start};

    next_token(parser);

    if (!expect_token(parser, TKN_IDENT, error_prefix)) {
        return NULL;
    }

    Identifier* name = parser->ptoken.as_ident.ident;

    if (!expect_token(parser, TKN_LBRACE, error_prefix)) {
        return NULL;
    }

    List fields = {0};

    if (!parse_fill_aggregate_body(parser, &fields) || !expect_token(parser, TKN_RBRACE, error_prefix)) {
        return NULL;
    }

    range.end = parser->ptoken.range.end;

    if (list_empty(&fields)) {
        parser_on_error(parser, range, "%s: must have at least one field", error_prefix);
        return NULL;
    }

    return new_decl_aggregate(parser->ast_arena, name, &fields, range);
}

// enum_item  = TKN_IDENT ('=' expr)?
static EnumItem* parse_enum_item(Parser* parser)
{
    if (!expect_token(parser, TKN_IDENT, "Failed to parse enum value")) {
        return NULL;
    }

    ProgRange range = parser->ptoken.range;
    Identifier* name = parser->ptoken.as_ident.ident;
    Expr* value = NULL;

    if (match_token(parser, TKN_ASSIGN)) {
        value = parse_expr(parser);

        if (!value) {
            return NULL;
        }

        range.end = parser->ptoken.range.end;
    }

    return new_enum_item(parser->ast_arena, name, value, range);
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

    if (expect_token(parser, TKN_IDENT, error_prefix)) {
        Identifier* name = parser->ptoken.as_ident.ident;
        TypeSpec* typespec = NULL;
        bool bad_type = false;

        if (match_token(parser, TKN_COLON)) {
            typespec = parse_typespec(parser);
            bad_type = !typespec;
        }

        if (!bad_type && expect_token(parser, TKN_LBRACE, error_prefix)) {
            size_t num_items = 0;
            List items = list_head_create(items);
            bool bad_item = false;

            while (!is_token_kind(parser, TKN_RBRACE) && !is_token_kind(parser, TKN_EOF)) {
                EnumItem* item = parse_enum_item(parser);

                if (item) {
                    num_items += 1;
                    list_add_last(&items, &item->lnode);
                }
                else {
                    bad_item = true;
                    break;
                }

                if (!match_token(parser, TKN_COMMA))
                    break;
            }

            if (!bad_item && expect_token(parser, TKN_RBRACE, error_prefix)) {
                range.end = parser->ptoken.range.end;

                if (num_items) {
                    decl = new_decl_enum(parser->ast_arena, name, typespec, num_items, &items, range);
                }
                else {
                    parser_on_error(parser, range,
                                    "%s: must have at least one enumeration constant", error_prefix);
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

    if (expect_token(parser, TKN_IDENT, error_prefix)) {
        Identifier* name = parser->ptoken.as_ident.ident;

        if (expect_token(parser, TKN_ASSIGN, error_prefix)) {
            TypeSpec* typespec = parse_typespec(parser);

            if (typespec && expect_token(parser, TKN_SEMICOLON, error_prefix)) {
                range.end = parser->ptoken.range.end;
                decl = new_decl_typedef(parser->ast_arena, name, typespec, range);
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

    if (expect_token(parser, TKN_IDENT, error_prefix)) {
        Identifier* name = parser->ptoken.as_ident.ident;

        if (expect_token(parser, TKN_COLON, error_prefix)) {
            TypeSpec* typespec = NULL;
            bool bad_type = false;

            if (!is_token_kind(parser, TKN_ASSIGN)) {
                typespec = parse_typespec(parser);
                bad_type = typespec == NULL;
            }

            if (!bad_type && expect_token(parser, TKN_ASSIGN, error_prefix)) {
                Expr* expr = parse_expr(parser);

                if (expr && expect_token(parser, TKN_SEMICOLON, error_prefix)) {
                    range.end = parser->ptoken.range.end;
                    decl = new_decl_const(parser->ast_arena, name, typespec, expr, range);
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
static Decl* parse_decl_no_annotations(Parser* parser)
{
    if (is_token_kind(parser, TKN_KW)) {
        Token token = parser->token;

        switch (token.as_kw.ident->kw) {
        case KW_CONST:
            return parse_decl_const(parser);
        case KW_TYPEDEF:
            return parse_decl_typedef(parser);
        case KW_VAR:
            return parse_decl_var(parser);
        case KW_ENUM:
            return parse_decl_enum(parser);
        case KW_STRUCT:
            return parse_decl_aggregate(parser, "Failed to parse struct declaration", new_decl_struct);
        case KW_UNION:
            return parse_decl_aggregate(parser, "Failed to parse union declaration", new_decl_union);
        case KW_PROC:
            return parse_decl_proc(parser);
        default:
            break;
        }
    }

    // If we got here, we have an unexpected token.
    char tmp[32];

    print_token(&parser->token, tmp, sizeof(tmp));
    parser_on_error(parser, parser->token.range, "Unexpected token: wanted a declaration keyword, but got `%s`", tmp);

    return NULL;
}

Decl* parse_decl(Parser* parser)
{
    List annotations = {0};

    if (!parse_annotations(parser, &annotations)) {
        return NULL;
    }

    Decl* decl = parse_decl_no_annotations(parser);

    if (!decl) {
        return NULL;
    }

    list_replace(&annotations, &decl->annotations);

    // Initialize decl flags based on some builtin annotations.
    {
        List* head = &decl->annotations;
        List* it = head->next;

        while (it != head) {
            DeclAnnotation* a = list_entry(it, DeclAnnotation, lnode);
            const char* name = a->ident->str;

            if (name == annotation_names[ANNOTATION_FOREIGN]) {
                if (decl->flags & DECL_IS_FOREIGN) {
                    parser_on_error(parser, a->range, "Duplicate @foreign annotations");
                    return NULL;
                }

                decl->flags |= DECL_IS_FOREIGN;
            }
            else if (name == annotation_names[ANNOTATION_EXPORTED]) {
                if (decl->flags & DECL_IS_EXPORTED) {
                    parser_on_error(parser, a->range, "Duplicate @exported annotations");
                    return NULL;
                }

                decl->flags |= DECL_IS_EXPORTED;
            }

            it = it->next;
        }
    }

    return decl;
}

