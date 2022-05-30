
char* ftprint_ns_ident(Allocator* allocator, NSIdent* ns_ident)
{
    char* dstr = array_create(allocator, char, 16);
    List* head = &ns_ident->idents;
    List* it = head->next;

    // Print namespaces.
    while (it->next != head) {
        IdentNode* inode = list_entry(it, IdentNode, lnode);

        ftprint_char_array(&dstr, false, "%s::", inode->ident->str);

        it = it->next;
    }

    // Print name.
    assert(it != head);
    IdentNode* inode = list_entry(it, IdentNode, lnode);

    ftprint_char_array(&dstr, true, "%s", inode->ident->str);

    return dstr;
}

char* ftprint_typespec(Allocator* allocator, TypeSpec* typespec)
{
    char* dstr = NULL;

    if (typespec) {
        switch (typespec->kind) {
        case CST_TYPE_SPEC_NONE: {
            assert(0);
        } break;
        case CST_TypeSpecIdent: {
            TypeSpecIdent* t = (TypeSpecIdent*)typespec;
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "(:ident %s)", ftprint_ns_ident(allocator, &t->ns_ident));
        } break;
        case CST_TypeSpecTypeof: {
            TypeSpecTypeof* t = (TypeSpecTypeof*)typespec;
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "(:typeof %s)", ftprint_expr(allocator, t->expr));
        } break;
        case CST_TypeSpecProc: {
            TypeSpecProc* t = (TypeSpecProc*)typespec;
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(:proc =>%s", ftprint_typespec(allocator, t->ret));

            size_t num_params = t->num_params;

            if (num_params) {
                ftprint_char_array(&dstr, false, " ");

                ListNode* head = &t->params;

                for (ListNode* it = head->next; it != head; it = it->next) {
                    ProcParam* param = list_entry(it, ProcParam, lnode);

                    if (param->name)
                        ftprint_char_array(&dstr, false, "(%s %s)", param->name->str, ftprint_typespec(allocator, param->typespec));
                    else
                        ftprint_char_array(&dstr, false, "%s", ftprint_typespec(allocator, param->typespec));

                    if (it->next != head)
                        ftprint_char_array(&dstr, false, " ");
                }
            }

            ftprint_char_array(&dstr, false, ")");
        } break;
        case CST_TypeSpecStruct:
        case CST_TypeSpecUnion: {
            dstr = array_create(allocator, char, 32);
            bool is_struct = typespec->kind == CST_TypeSpecStruct;

            ftprint_char_array(&dstr, false, "(:%s", (is_struct ? "struct" : "union"));

            TypeSpecAggregate* aggregate = (TypeSpecAggregate*)typespec;

            if (!list_empty(&aggregate->fields)) {
                ftprint_char_array(&dstr, false, " ");

                ListNode* head = &aggregate->fields;

                for (ListNode* it = head->next; it != head; it = it->next) {
                    AggregateField* field = list_entry(it, AggregateField, lnode);
                    const char* field_name = field->name ? field->name->str : "_anon_";

                    ftprint_char_array(&dstr, false, "(%s %s)", field_name, ftprint_typespec(allocator, field->typespec));

                    if (it->next != head)
                        ftprint_char_array(&dstr, false, " ");
                }
            }

            ftprint_char_array(&dstr, false, ")");
        } break;
        case CST_TypeSpecPtr: {
            TypeSpecPtr* t = (TypeSpecPtr*)typespec;
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(:ptr %s)", ftprint_typespec(allocator, t->base));
        } break;
        case CST_TypeSpecConst: {
            TypeSpecConst* t = (TypeSpecConst*)typespec;
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(:const %s)", ftprint_typespec(allocator, t->base));
        } break;
        case CST_TypeSpecArray: {
            TypeSpecArray* t = (TypeSpecArray*)typespec;
            dstr = array_create(allocator, char, 32);

            if (t->len) {
                ftprint_char_array(&dstr, false, "(:arr %s %s)", ftprint_expr(allocator, t->len),
                                   ftprint_typespec(allocator, t->base));
            }
            else if (t->infer_len) {
                ftprint_char_array(&dstr, false, "(:arr _ %s)", ftprint_typespec(allocator, t->base));
            }
            else {
                ftprint_char_array(&dstr, false, "(:arr_slice %s)", ftprint_typespec(allocator, t->base));
            }
        } break;
        default: {
            ftprint_err("Unknown typespec kind: %d\n", typespec->kind);
            assert(0);
        } break;
        }
    }
    else {
        dstr = array_create(allocator, char, 1);
    }

    array_push(dstr, '\0');

    return dstr;
}

char* ftprint_expr(Allocator* allocator, Expr* expr)
{
    char* dstr = NULL;

    if (expr) {
        switch (expr->kind) {
        case CST_EXPR_NONE: {
            assert(0);
        } break;
        case CST_ExprTernary: {
            ExprTernary* e = (ExprTernary*)expr;
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(? %s %s %s)", ftprint_expr(allocator, e->cond), ftprint_expr(allocator, e->then_expr),
                               ftprint_expr(allocator, e->else_expr));
        } break;
        case CST_ExprBinary: {
            ExprBinary* e = (ExprBinary*)expr;
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(%s %s %s)", token_kind_names[e->op], ftprint_expr(allocator, e->left),
                               ftprint_expr(allocator, e->right));
        } break;
        case CST_ExprUnary: {
            ExprUnary* e = (ExprUnary*)expr;
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "(%s %s)", token_kind_names[e->op], ftprint_expr(allocator, e->expr));
        } break;
        case CST_ExprCall: {
            ExprCall* e = (ExprCall*)expr;
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(call %s", ftprint_expr(allocator, e->proc));

            size_t num_args = e->num_args;

            if (num_args) {
                ftprint_char_array(&dstr, false, " ");

                List* head = &e->args;

                for (List* it = head->next; it != head; it = it->next) {
                    ProcCallArg* arg = list_entry(it, ProcCallArg, lnode);

                    if (arg->name)
                        ftprint_char_array(&dstr, false, "%s=", arg->name->str);

                    ftprint_char_array(&dstr, false, "%s", ftprint_expr(allocator, arg->expr));

                    if (it->next != head)
                        ftprint_char_array(&dstr, false, " ");
                }
            }

            ftprint_char_array(&dstr, false, ")");
        } break;
        case CST_ExprIndex: {
            ExprIndex* e = (ExprIndex*)expr;
            dstr = array_create(allocator, char, 8);
            ftprint_char_array(&dstr, false, "(index %s %s)", ftprint_expr(allocator, e->array), ftprint_expr(allocator, e->index));
        } break;
        case CST_ExprField: {
            ExprField* e = (ExprField*)expr;
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "(field %s %s)", ftprint_expr(allocator, e->object), e->field->str);
        } break;
        case CST_ExprFieldIndex: {
            ExprFieldIndex* e = (ExprFieldIndex*)expr;
            dstr = array_create(allocator, char, 8);
            ftprint_char_array(&dstr, false, "(field-index %s %s)", ftprint_expr(allocator, e->object),
                               ftprint_expr(allocator, e->index));
        } break;
        case CST_ExprInt: {
            ExprInt* e = (ExprInt*)expr;
            dstr = array_create(allocator, char, 8);
            ftprint_char_array(&dstr, false, "%lu", e->token.value);
        } break;
        case CST_ExprFloat: {
            ExprFloat* e = (ExprFloat*)expr;
            dstr = array_create(allocator, char, 8);

            if (e->fkind == FLOAT_F64)
                ftprint_char_array(&dstr, false, "%lf", e->value._f64);
            else
                ftprint_char_array(&dstr, false, "%lf", e->value._f32);
        } break;
        case CST_ExprStr: {
            ExprStr* e = (ExprStr*)expr;
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "\"%s\"", e->str_lit->str);
        } break;
        case CST_ExprIdent: {
            ExprIdent* e = (ExprIdent*)expr;
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "%s", ftprint_ns_ident(allocator, &e->ns_ident));
        } break;
        case CST_ExprCast: {
            ExprCast* e = (ExprCast*)expr;
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "(cast %s %s)", ftprint_typespec(allocator, e->typespec),
                               ftprint_expr(allocator, e->expr));
        } break;
        case CST_ExprSizeof: {
            ExprSizeof* e = (ExprSizeof*)expr;
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "(sizeof %s)", ftprint_typespec(allocator, e->typespec));
        } break;
        case CST_ExprTypeid: {
            ExprTypeid* e = (ExprTypeid*)expr;
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "(typeid %s)", ftprint_typespec(allocator, e->typespec));
        } break;
        case CST_ExprOffsetof: {
            ExprOffsetof* e = (ExprOffsetof*)expr;
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "(offsetof %s %s)", ftprint_typespec(allocator, e->obj_ts), e->field_ident->str);
        } break;
        case CST_ExprIndexof: {
            ExprIndexof* e = (ExprIndexof*)expr;
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "(indexof %s %s)", ftprint_typespec(allocator, e->obj_ts), e->field_ident->str);
        } break;
        case CST_ExprLength: {
            ExprLength* e = (ExprLength*)expr;
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "(len %s)", ftprint_expr(allocator, e->arg));
        } break;
        case CST_ExprCompoundLit: {
            ExprCompoundLit* e = (ExprCompoundLit*)expr;
            dstr = array_create(allocator, char, 32);

            ftprint_char_array(&dstr, false, "(compound ");

            if (e->typespec)
                ftprint_char_array(&dstr, false, "%s ", ftprint_typespec(allocator, e->typespec));

            ftprint_char_array(&dstr, false, "{");

            if (e->num_initzers) {
                List* head = &e->initzers;

                for (List* it = head->next; it != head; it = it->next) {
                    MemberInitializer* initzer = list_entry(it, MemberInitializer, lnode);

                    if (initzer->designator.kind == DESIGNATOR_NAME)
                        ftprint_char_array(&dstr, false, "%s = ", initzer->designator.name);
                    else if (initzer->designator.kind == DESIGNATOR_INDEX)
                        ftprint_char_array(&dstr, false, "[%s] = ", ftprint_expr(allocator, initzer->designator.index));

                    ftprint_char_array(&dstr, false, "%s", ftprint_expr(allocator, initzer->init));

                    if (it->next != head)
                        ftprint_char_array(&dstr, false, " ");
                }
            }

            ftprint_char_array(&dstr, false, "})");
        } break;
        case CST_ExprBoolLit: {
            ExprBoolLit* e = (ExprBoolLit*)expr;
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "%s", e->val ? "true" : "false");
        } break;
        case CST_ExprNullLit: {
            dstr = array_create(allocator, char, 6);
            ftprint_char_array(&dstr, false, "null");
        } break;
        default: {
            ftprint_err("Unknown expr kind: %d\n", expr->kind);
            assert(0);
        } break;
        }
    }
    else {
        dstr = array_create(allocator, char, 1);
    }

    array_push(dstr, '\0');

    return dstr;
}

static char* ftprint_stmt_list(Allocator* allocator, List* stmts)
{
    char* dstr = NULL;

    if (!list_empty(stmts)) {
        dstr = array_create(allocator, char, 32);
        List* head = stmts;

        for (List* it = head->next; it != head; it = it->next) {
            Stmt* s = list_entry(it, Stmt, lnode);

            ftprint_char_array(&dstr, false, "%s", ftprint_stmt(allocator, s));

            if (it->next != head)
                ftprint_char_array(&dstr, false, " ");
        }
    }
    else {
        dstr = array_create(allocator, char, 1);
    }

    array_push(dstr, '\0');

    return dstr;
}

char* ftprint_stmt(Allocator* allocator, Stmt* stmt)
{
    char* dstr = NULL;

    if (stmt) {
        switch (stmt->kind) {
        case CST_STMT_NONE: {
            assert(0);
        } break;
        case CST_StmtNoOp: {
            dstr = array_create(allocator, char, 6);
            ftprint_char_array(&dstr, false, "no-op");
        } break;
        case CST_StmtDecl: {
            StmtDecl* s = (StmtDecl*)stmt;
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "%s", ftprint_decl(allocator, s->decl));
        } break;
        case CST_StmtBlock: {
            StmtBlock* s = (StmtBlock*)stmt;
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(stmt-block");

            if (!list_empty(&s->stmts))
                ftprint_char_array(&dstr, false, " %s)", ftprint_stmt_list(allocator, &s->stmts));
            else
                ftprint_char_array(&dstr, false, ")");
        } break;
        case CST_StmtExpr: {
            StmtExpr* s = (StmtExpr*)stmt;
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "%s", ftprint_expr(allocator, s->expr));
        } break;
        case CST_StmtExprAssign: {
            StmtExprAssign* s = (StmtExprAssign*)stmt;
            dstr = array_create(allocator, char, 32);
            const char* op = token_kind_names[s->op_assign];

            ftprint_char_array(&dstr, false, "(%s %s %s)", op, ftprint_expr(allocator, s->left), ftprint_expr(allocator, s->right));
        } break;
        case CST_StmtWhile: {
            StmtWhile* s = (StmtWhile*)stmt;
            dstr = array_create(allocator, char, 32);

            ftprint_char_array(&dstr, false, "(while %s %s)", ftprint_expr(allocator, s->cond), ftprint_stmt(allocator, s->body));
        } break;
        case CST_StmtDoWhile: {
            StmtDoWhile* s = (StmtDoWhile*)stmt;
            dstr = array_create(allocator, char, 32);

            ftprint_char_array(&dstr, false, "(do-while %s %s)", ftprint_expr(allocator, s->cond), ftprint_stmt(allocator, s->body));
        } break;
        case CST_StmtFor: {
            StmtFor* s = (StmtFor*)stmt;
            dstr = array_create(allocator, char, 32);

            ftprint_char_array(&dstr, false, "(for ");

            if (s->init)
                ftprint_char_array(&dstr, false, "%s; ", ftprint_stmt(allocator, s->init));
            else
                ftprint_char_array(&dstr, false, "; ");

            if (s->cond)
                ftprint_char_array(&dstr, false, "%s; ", ftprint_expr(allocator, s->cond));
            else
                ftprint_char_array(&dstr, false, "; ");

            if (s->next)
                ftprint_char_array(&dstr, false, "%s ", ftprint_stmt(allocator, s->next));
            else
                ftprint_char_array(&dstr, false, " ");

            ftprint_char_array(&dstr, false, "%s)", ftprint_stmt(allocator, s->body));
        } break;
        case CST_StmtIf: {
            StmtIf* s = (StmtIf*)stmt;
            dstr = array_create(allocator, char, 64);

            ftprint_char_array(&dstr, false, "(if %s %s", ftprint_expr(allocator, s->if_blk.cond),
                               ftprint_stmt(allocator, s->if_blk.body));

            if (s->else_blk.body)
                ftprint_char_array(&dstr, false, " (else %s)", ftprint_stmt(allocator, s->else_blk.body));

            ftprint_char_array(&dstr, false, ")");
        } break;
        case CST_StmtSwitch: {
            StmtSwitch* s = (StmtSwitch*)stmt;
            dstr = array_create(allocator, char, 64);

            ftprint_char_array(&dstr, false, "(switch %s ", ftprint_expr(allocator, s->expr));

            List* head = &s->cases;

            for (List* it = head->next; it != head; it = it->next) {
                SwitchCase* swcase = list_entry(it, SwitchCase, lnode);

                ftprint_char_array(&dstr, false, "(case");

                if (swcase->start) {
                    ftprint_char_array(&dstr, false, " %s", ftprint_expr(allocator, swcase->start));

                    if (swcase->end)
                        ftprint_char_array(&dstr, false, "..%s", ftprint_expr(allocator, swcase->end));
                }

                if (!list_empty(&swcase->stmts))
                    ftprint_char_array(&dstr, false, " (stmt-list %s))", ftprint_stmt_list(allocator, &swcase->stmts));
                else
                    ftprint_char_array(&dstr, false, " (stmt-list))");

                if (it->next != head)
                    ftprint_char_array(&dstr, false, " ");
            }

            ftprint_char_array(&dstr, false, ")");
        } break;
        case CST_StmtReturn: {
            StmtReturn* s = (StmtReturn*)stmt;
            dstr = array_create(allocator, char, 16);

            ftprint_char_array(&dstr, false, "(return");

            if (s->expr) {
                ftprint_char_array(&dstr, false, " %s", ftprint_expr(allocator, s->expr));
            }

            ftprint_char_array(&dstr, false, ")");
        } break;
        case CST_StmtBreak: {
            StmtBreak* s = (StmtBreak*)stmt;
            dstr = array_create(allocator, char, 16);

            ftprint_char_array(&dstr, false, "(break");

            if (s->label) {
                ftprint_char_array(&dstr, false, " %s)", s->label);
            }
            else {
                ftprint_char_array(&dstr, false, ")");
            }
        } break;
        case CST_StmtContinue: {
            StmtContinue* s = (StmtContinue*)stmt;
            dstr = array_create(allocator, char, 16);

            ftprint_char_array(&dstr, false, "(continue");

            if (s->label) {
                ftprint_char_array(&dstr, false, " %s)", s->label);
            }
            else {
                ftprint_char_array(&dstr, false, ")");
            }
        } break;
        case CST_StmtGoto: {
            StmtGoto* s = (StmtGoto*)stmt;
            dstr = array_create(allocator, char, 16);

            ftprint_char_array(&dstr, false, "(goto %s)", s->label);
        } break;
        case CST_StmtLabel: {
            StmtLabel* s = (StmtLabel*)stmt;
            dstr = array_create(allocator, char, 32);

            ftprint_char_array(&dstr, false, "(label %s %s)", s->label, ftprint_stmt(allocator, s->target));
        } break;
        case CST_StmtStaticAssert: {
            StmtStaticAssert* s = (StmtStaticAssert*)stmt;
            dstr = array_create(allocator, char, 32);

            ftprint_char_array(&dstr, false, "(#static_assert %s", ftprint_expr(allocator, s->cond));

            if (s->msg) {
                ftprint_char_array(&dstr, false, " %s)", s->msg->str);
            }
            else {
                ftprint_char_array(&dstr, false, ")");
            }
        } break;
        case CST_StmtInclude: {
            StmtInclude* s = (StmtInclude*)stmt;
            dstr = array_create(allocator, char, 32);

            ftprint_char_array(&dstr, false, "(include \"%s\")", s->file_pathname->str);
        } break;
        case CST_StmtImport: {
            StmtImport* s = (StmtImport*)stmt;
            dstr = array_create(allocator, char, 32);

            ftprint_char_array(&dstr, false, "(import ");

            // Print imported syms
            if (!list_empty(&s->import_syms)) {
                ftprint_char_array(&dstr, false, "{");

                List* head = &s->import_syms;
                List* it = head->next;

                while (it != head) {
                    ImportSymbol* entity = list_entry(it, ImportSymbol, lnode);
                    const char* suffix = (it->next == head) ? "} from " : ", ";

                    ftprint_char_array(&dstr, false, "%s%s", entity->name->str, suffix);
                    it = it->next;
                }
            }

            ftprint_char_array(&dstr, false, "\"%s\"", s->mod_pathname->str);

            if (s->mod_namespace) {
                ftprint_char_array(&dstr, false, " as %s)", s->mod_namespace->str);
            }
            else {
                ftprint_char_array(&dstr, false, ")");
            }

        } break;
        case CST_StmtExport: {
            StmtExport* s = (StmtExport*)stmt;
            dstr = array_create(allocator, char, 32);

            ftprint_char_array(&dstr, false, "(export ");

            // Print exported syms
            if (!list_empty(&s->export_syms)) {
                ftprint_char_array(&dstr, false, "{");

                List* head = &s->export_syms;
                List* it = head->next;

                while (it != head) {
                    ExportSymbol* entity = list_entry(it, ExportSymbol, lnode);
                    const char* suffix = (it->next == head) ? "}" : ", ";

                    ftprint_char_array(&dstr, false, "%s%s", ftprint_ns_ident(allocator, &entity->ns_ident), suffix);
                    it = it->next;
                }
            }

            ftprint_char_array(&dstr, false, ")");
        } break;
        default: {
            ftprint_err("Unknown stmt kind: %d\n", stmt->kind);
            assert(0);
        } break;
        }
    }
    else {
        dstr = array_create(allocator, char, 1);
    }

    array_push(dstr, '\0');

    return dstr;
}

char* ftprint_decl(Allocator* allocator, Decl* decl)
{
    char* dstr = NULL;

    if (decl) {
        switch (decl->kind) {
        case CST_DECL_NONE: {
            assert(0);
        } break;
        case CST_DeclVar: {
            DeclVar* d = (DeclVar*)decl;
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(var %s", decl->name->str);

            if (d->typespec) {
                ftprint_char_array(&dstr, false, " %s", ftprint_typespec(allocator, d->typespec));
            }

            if (d->init) {
                ftprint_char_array(&dstr, false, " %s", ftprint_expr(allocator, d->init));
            }

            ftprint_char_array(&dstr, false, ")");
        } break;
        case CST_DeclConst: {
            DeclConst* d = (DeclConst*)decl;
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(const %s", decl->name->str);

            if (d->typespec) {
                ftprint_char_array(&dstr, false, " %s", ftprint_typespec(allocator, d->typespec));
            }

            if (d->init) {
                ftprint_char_array(&dstr, false, " %s", ftprint_expr(allocator, d->init));
            }

            ftprint_char_array(&dstr, false, ")");
        } break;
        case CST_DeclTypedef: {
            DeclTypedef* d = (DeclTypedef*)decl;
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(typedef %s %s)", decl->name->str, ftprint_typespec(allocator, d->typespec));
        } break;
        case CST_DeclEnum: {
            DeclEnum* d = (DeclEnum*)decl;
            dstr = array_create(allocator, char, 32);

            ftprint_char_array(&dstr, false, "(enum %s", decl->name->str);

            if (d->typespec)
                ftprint_char_array(&dstr, false, " %s", ftprint_typespec(allocator, d->typespec));

            if (!list_empty(&d->items)) {
                ftprint_char_array(&dstr, false, " ");

                List* head = &d->items;

                for (List* it = head->next; it != head; it = it->next) {
                    DeclEnumItem* item = list_entry(it, DeclEnumItem, lnode);

                    ftprint_char_array(&dstr, false, "%s", item->super.name->str);

                    if (item->value)
                        ftprint_char_array(&dstr, false, "=%s", ftprint_expr(allocator, item->value));

                    if (it->next != head)
                        ftprint_char_array(&dstr, false, " ");
                }
            }

            ftprint_char_array(&dstr, false, ")");
        } break;
        case CST_DeclStruct:
        case CST_DeclUnion: {
            DeclAggregate* d = (DeclAggregate*)decl;
            dstr = array_create(allocator, char, 32);

            ftprint_char_array(&dstr, false, "(%s %s", (decl->kind == CST_DeclStruct ? "struct" : "union"), decl->name->str);

            if (!list_empty(&d->fields)) {
                ftprint_char_array(&dstr, false, " ");

                List* head = &d->fields;

                for (List* it = head->next; it != head; it = it->next) {
                    AggregateField* field = list_entry(it, AggregateField, lnode);

                    ftprint_char_array(&dstr, false, "(%s %s)", field->name->str, ftprint_typespec(allocator, field->typespec));

                    if (it->next != head)
                        ftprint_char_array(&dstr, false, " ");
                }
            }

            ftprint_char_array(&dstr, false, ")");
        } break;
        case CST_DeclProc: {
            DeclProc* proc = (DeclProc*)decl;
            dstr = array_create(allocator, char, 32);

            ftprint_char_array(&dstr, false, "(proc %s (", decl->name->str);

            if (!list_empty(&proc->params)) {
                List* head = &proc->params;

                for (List* it = head->next; it != head; it = it->next) {
                    Decl* param = list_entry(it, Decl, lnode);

                    ftprint_char_array(&dstr, false, "%s", ftprint_decl(allocator, param));

                    if (it->next != head)
                        ftprint_char_array(&dstr, false, " ");
                }
            }

            ftprint_char_array(&dstr, false, ") =>%s ", ftprint_typespec(allocator, proc->ret));

            if (list_empty(&proc->stmts))
                ftprint_char_array(&dstr, false, "(stmt-block))");
            else
                ftprint_char_array(&dstr, false, "(stmt-block %s))", ftprint_stmt_list(allocator, &proc->stmts));
        } break;
        default: {
            ftprint_err("Unknown decl kind: %d\n", decl->kind);
            assert(0);
        } break;
        }
    }
    else {
        dstr = array_create(allocator, char, 1);
    }

    array_push(dstr, '\0');

    return dstr;
}

char* ftprint_decls(Allocator* allocator, size_t num_decls, Decl** decls)
{
    assert(decls);

    char* dstr = array_create(allocator, char, 64);

    for (size_t i = 0; i < num_decls; i += 1) {
        ftprint_char_array(&dstr, false, "%s\n", ftprint_decl(allocator, decls[i]));
    }

    array_push(dstr, '\0');

    return dstr;
}
