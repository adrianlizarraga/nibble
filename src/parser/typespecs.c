#include <assert.h>
#include "parser/module.h"

///////////////////////////////
//    Parse type specifiers
//////////////////////////////

// aggregate_field = (TKN_IDENT ':')? type_spec
static AggregateField* parse_aggregate_field(Parser* parser)
{
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

    ProgRange range = {.start = start, .end = parser->ptoken.range.end};

    return new_aggregate_field(parser->ast_arena, ident, typespec, range);
}

// aggregate_body  = '{' aggregate_fields+ '}'
// aggregate_fields = aggregate_field (';' aggregate_field)* ';'?
static bool parse_aggregate_fields(Parser* parser, List* fields)
{
    list_head_init(fields);

    while (!is_token_kind(parser, TKN_RBRACE) && !is_token_kind(parser, TKN_EOF)) {
        AggregateField* field = parse_aggregate_field(parser);

        if (!field)
            return false;

        list_add_last(fields, &field->lnode);

        if (!match_token(parser, TKN_SEMICOLON)) {
            break;
        }
    }

    return true;
}

bool parse_aggregate_body(Parser* parser, List* fields, ProgPos start, const char* err_prefix)
{
    if (!expect_token(parser, TKN_LBRACE, err_prefix)) {
        return false;
    }

    if (!parse_aggregate_fields(parser, fields)) {
        return false;
    }

    if (!expect_token(parser, TKN_RBRACE, err_prefix)) {
        return false;
    }

    if (list_empty(fields)) {
        ProgRange range = {.start = start, .end = parser->ptoken.range.end};
        parser_on_error(parser, range, "%s: must have at least one field", err_prefix);
        return false;
    }

    return true;
}

// typespec_anon_struct = KW_STRUCT? '{' aggregate_fields+ '}'
static TypeSpec* parse_typespec_struct(Parser* parser)
{
    const char* err_prefix = "Failed to parse anonymous struct type";
    ProgPos start = parser->token.range.start;

    match_keyword(parser, KW_STRUCT);

    List fields = {0};

    if (!parse_aggregate_body(parser, &fields, start, err_prefix)) {
        return NULL;
    }

    ProgRange range = {.start = start, .end = parser->ptoken.range.end};

    return new_typespec_struct(parser->ast_arena, &fields, range);
}

// typespec_anon_union  = KW_UNION '{' aggregate_fields+ '}'
static TypeSpec* parse_typespec_union(Parser* parser)
{
    const char* err_prefix = "Failed to parse anonymous union type";
    ProgPos start = parser->token.range.start;

    if (!expect_keyword(parser, KW_UNION, err_prefix)) {
        return NULL;
    }

    List fields = {0};

    if (!parse_aggregate_body(parser, &fields, start, err_prefix)) {
        return NULL;
    }

    ProgRange range = {.start = start, .end = parser->ptoken.range.end};

    return new_typespec_union(parser->ast_arena, &fields, range);
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
bool parse_namespaced_ident(Parser* parser, NSIdent* ns_ident, const char* err_prefix)
{
    ns_ident->range = parser->token.range;
    ns_ident->num_idents = 0;
    list_head_init(&ns_ident->idents);

    // Keep parsing identifiers as long as we see a `::` token.
    do {
        if (!expect_token(parser, TKN_IDENT, err_prefix)) {
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

    if (!parse_namespaced_ident(parser, &ns_ident, "Failed to parse typespec identifier")) {
        return NULL;
    }

    return new_typespec_ident(parser->ast_arena, &ns_ident);
}

// typespec_typeof = KW_TYPEOF '(' expr ')'
static TypeSpec* parse_typespec_typeof(Parser* parser)
{
    assert(is_keyword(parser, KW_TYPEOF));
    ProgRange range = {.start = parser->token.range.start};
    const char* error_prefix = "Failed to parse #typeof";

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

// typespec_ret_type = KW_RET_TYPE ('(' expr ')')?
static TypeSpec* parse_typespec_ret_type(Parser* parser)
{
    const char* err_prefix = "Failed to parse #ret_type";
    ProgPos start = parser->token.range.start;

    if (!expect_keyword(parser, KW_RET_TYPE, err_prefix)) {
        return NULL;
    }

    Expr* proc_expr = NULL;

    // NOTE: Parentheses and proc expression are optional.
    // If omitted, assumes current procedure. Otherwise, will return the return type corresponding to the indicated procedure.
    if (match_token(parser, TKN_LPAREN)) {
        proc_expr = parse_expr(parser);

        if (!proc_expr) {
            return NULL;
        }

        if (!expect_token(parser, TKN_RPAREN, err_prefix)) {
            return NULL;
        }
    }

    ProgRange range = {.start = start, .end = parser->ptoken.range.end};

    return new_typespec_ret_type(parser->ast_arena, proc_expr, range);
}

// typespec_base  = typespec_proc
//                | typespec_anon_struct
//                | typespec_anon_union
//                | typespec_typeof
//                | typespec_ret_type
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
            return parse_typespec_struct(parser);
        case KW_UNION:
            return parse_typespec_union(parser);
        case KW_TYPEOF:
            return parse_typespec_typeof(parser);
        case KW_RET_TYPE:
            return parse_typespec_ret_type(parser);
        default:
            break;
        }
    } break;
    case TKN_LBRACE:
        return parse_typespec_struct(parser);
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

