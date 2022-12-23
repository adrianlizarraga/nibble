#include <assert.h>
#include "parser/module.h"

///////////////////////////////
//    Parse declarations
//////////////////////////////

static DeclAnnotation* parse_annotation(Parser* parser)
{
    assert(is_token_kind(parser, TKN_AT));
    const char* err_prefix = "Failed to parse annotation";
    ProgRange range = {.start = parser->token.range.start};

    next_token(parser);

    // Parse annotation name
    if (!expect_token(parser, TKN_IDENT, err_prefix)) {
        return NULL;
    }

    Identifier* name = parser->ptoken.as_ident.ident;

    // Parse annotation arguments
    u32 num_args = 0;
    List args = list_head_create(args);

    if (match_token(parser, TKN_LPAREN)) {
        while (!is_token_kind(parser, TKN_RPAREN) && !is_token_kind(parser, TKN_EOF)) {
            ProcCallArg* arg = parse_proc_call_arg(parser);

            if (!arg) {
                return NULL;
            }

            num_args += 1;
            list_add_last(&args, &arg->lnode);

            if (!match_token(parser, TKN_COMMA)) {
                break;
            }
        }

        // Parse closing parenthesis.
        if (!expect_token(parser, TKN_RPAREN, err_prefix)) {
            return NULL;
        }
    }

    range.end = parser->ptoken.range.end;

    return new_annotation(parser->ast_arena, name, num_args, &args, range);
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
    List fields = {0};

    if (!parse_aggregate_body(parser, &fields, range.start, error_prefix)) {
        return NULL;
    }

    range.end = parser->ptoken.range.end;

    return new_decl_aggregate(parser->ast_arena, name, &fields, range);
}

// decl_enum_item  = TKN_IDENT ('=' expr)?
static DeclEnumItem* parse_decl_enum_item(Parser* parser)
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

    return new_decl_enum_item(parser->ast_arena, name, value, range);
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
                DeclEnumItem* item = parse_decl_enum_item(parser);

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

