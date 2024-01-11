#include <assert.h>
#include "parser/internal.h"

///////////////////////////////
//    Parse statements
//////////////////////////////

bool parse_fill_stmt_block_body(Parser* parser, StmtBlockBody* body, const char* err_prefix)
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

// switch_case = 'case' (expr ('..' expr)?)? ':' stmt
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

    if (!is_token_kind(parser, TKN_LBRACE)) {
        parser_on_error(parser, parser->token.range, "Switch case statements must be enclosed in curly brackets `{ ... }`");
        return NULL;
    }

    Stmt* body = parse_stmt(parser);

    if (!body) {
        return NULL;
    }

    range.end = parser->ptoken.range.end;
    return new_switch_case(parser->ast_arena, start, end, body, range);
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

        if (cond && expect_token(parser, TKN_RPAREN, error_prefix) && expect_token(parser, TKN_SEMICOLON, error_prefix)) {
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
        parser_on_error(parser, parser->token.range, "Failed to parse return statement: wanted `;` or expression, but got `}`");

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

// import_sym = TKN_IDENT ('as' TKN_IDENT)?
static ImportSymbol* parse_import_symbol(Parser* parser)
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

    return new_import_symbol(parser->ast_arena, name, rename, range);
}

// export_sym = namespaced_ident ('as' TKN_IDENT)?
static ExportSymbol* parse_export_symbol(Parser* parser)
{
    ProgRange range = parser->token.range;
    const char* error_prefix = "Failed to parse export symbol";
    NSIdent ns_ident = {0};

    if (!parse_namespaced_ident(parser, &ns_ident, error_prefix)) {
        return NULL;
    }

    Identifier* rename = NULL;

    if (match_keyword(parser, KW_AS)) {
        if (!expect_token(parser, TKN_IDENT, error_prefix)) {
            return NULL;
        }

        rename = parser->ptoken.as_ident.ident;
    }

    range.end = parser->ptoken.range.end;

    return new_export_symbol(parser->ast_arena, &ns_ident, rename, range);
}

// stmt_export = 'export' '{' export_syms '}' ';'
// export_syms = port_sym (',' port_sym)*
static Stmt* parse_stmt_export(Parser* parser)
{
    ProgRange range = {.start = parser->token.range.start};
    const char* error_prefix = "Failed to parse export statement";

    if (!expect_keyword(parser, KW_EXPORT, error_prefix) || !expect_token(parser, TKN_LBRACE, error_prefix)) {
        return NULL;
    }

    // Parse export symbols
    List export_syms = list_head_create(export_syms);
    size_t num_exports = 0;

    do {
        ExportSymbol* esym = parse_export_symbol(parser);

        if (!esym) {
            return NULL;
        }

        list_add_last(&export_syms, &esym->lnode);
        num_exports += 1;
    } while (match_token(parser, TKN_COMMA));

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
        do {
            ImportSymbol* isym = parse_import_symbol(parser);

            if (!isym) {
                return NULL;
            }

            list_add_last(&import_syms, &isym->lnode);
            num_imports += 1;
        } while (match_token(parser, TKN_COMMA));

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
                    " Found token `%s`",
                    tmp);

    return NULL;
}
