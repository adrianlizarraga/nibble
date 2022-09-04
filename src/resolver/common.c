#include "resolver/module.h"
#include "parser/module.h"

static bool resolve_decl_var(Resolver* resolver, Symbol* sym);
static bool resolve_decl_const(Resolver* resolver, Symbol* sym);
static bool resolve_decl_proc(Resolver* resolver, Symbol* sym);
static bool resolve_decl_enum(Resolver* resolver, Symbol* sym);
static bool resolve_decl_typedef(Resolver* resolver, Symbol* sym);
static bool resolve_global_proc_body(Resolver* resolver, Symbol* sym);
static bool resolve_expr(Resolver* resolver, Expr* expr, Type* expected_type);
static Type* resolve_typespec(Resolver* resolver, TypeSpec* typespec);
static unsigned resolve_stmt(Resolver* resolver, Stmt* stmt, Type* ret_type, unsigned flags);

static void resolver_on_error(Resolver* resolver, ProgRange range, const char* format, ...)
{
    char buf[MAX_ERROR_LEN];
    size_t size = 0;
    va_list vargs;

    va_start(vargs, format);
    size = vsnprintf(buf, MAX_ERROR_LEN, format, vargs) + 1;
    va_end(vargs);

    error_stream_add(&resolver->ctx->errors, range, buf, size > sizeof(buf) ? sizeof(buf) : size);
}

static ModuleState enter_module(Resolver* resolver, Module* mod)
{
    ModuleState old_state = resolver->state;

    resolver->state.mod = mod;
    resolver->state.proc = NULL;
    resolver->state.scope = &mod->scope;

    return old_state;
}

static void exit_module(Resolver* resolver, ModuleState state)
{
    resolver->state = state;
}

static void set_scope(Resolver* resolver, Scope* scope)
{
    resolver->state.scope = scope;
}

static Scope* push_scope(Resolver* resolver, size_t num_syms)
{
    Scope* prev_scope = resolver->state.scope;
    Scope* scope = new_scope(&resolver->ctx->ast_mem, num_syms + num_syms);

    scope->parent = prev_scope;

    list_add_last(&prev_scope->children, &scope->lnode);
    set_scope(resolver, scope);

    return scope;
}

static void pop_scope(Resolver* resolver)
{
    resolver->state.scope = resolver->state.scope->parent;
}

static ModuleState enter_proc(Resolver* resolver, Symbol* sym)
{
    assert(sym->kind == SYMBOL_PROC);
    ModuleState mod_state = enter_module(resolver, sym->home);

    DeclProc* dproc = (DeclProc*)(sym->decl);
    set_scope(resolver, dproc->scope);

    resolver->state.proc = sym;

    return mod_state;
}

static void exit_proc(Resolver* resolver, ModuleState state)
{
    pop_scope(resolver);
    resolver->state.proc = NULL;
    exit_module(resolver, state);
}

static bool resolve_symbol(Resolver* resolver, Symbol* sym)
{
    if (sym->status == SYMBOL_STATUS_RESOLVED)
        return true;

    if (sym->status == SYMBOL_STATUS_RESOLVING) {
        assert(sym->decl);
        resolver_on_error(resolver, sym->decl->range, "Cannot resolve symbol `%s` due to cyclic dependency", sym->name->str);
        return false;
    }

    assert(sym->status == SYMBOL_STATUS_UNRESOLVED);

    bool success = false;
    bool is_global = !sym->is_local;

    ModuleState mod_state = enter_module(resolver, sym->home);

    sym->status = SYMBOL_STATUS_RESOLVING;

    switch (sym->kind) {
    case SYMBOL_VAR:
        success = resolve_decl_var(resolver, sym);

        if (is_global) {
            add_global_data(&resolver->ctx->vars, sym, sym->type->size);
        }
        break;
    case SYMBOL_CONST:
        assert(sym->decl->kind == CST_DeclConst);
        success = resolve_decl_const(resolver, sym);
        break;
    case SYMBOL_PROC:
        success = resolve_decl_proc(resolver, sym);

        if (is_global) {
            bucket_list_add_elem(&resolver->ctx->procs, sym);
        }
        break;
    case SYMBOL_TYPE: {
        assert(sym->decl);
        Decl* decl = sym->decl;

        if (decl->kind == CST_DeclTypedef) {
            success = resolve_decl_typedef(resolver, sym);
        }
        else if (decl->kind == CST_DeclEnum) {
            success = resolve_decl_enum(resolver, sym);
        }
        else {
            assert(decl->kind == CST_DeclStruct || decl->kind == CST_DeclUnion);

            sym->type = type_incomplete_aggregate(&resolver->ctx->ast_mem, sym);
            sym->status = SYMBOL_STATUS_RESOLVED;

            if (is_global) {
                bucket_list_add_elem(&resolver->ctx->aggregate_types, sym);
            }

            success = true;
        }

        break;
    }
    default:
        NIBBLE_FATAL_EXIT("Unhandled symbol kind `%d`\n", sym->kind);
        break;
    }

    exit_module(resolver, mod_state);

    return success;
}

static Symbol* resolve_name(Resolver* resolver, Identifier* name)
{
    Symbol* sym = lookup_symbol(resolver->state.scope, name);

    if (!sym) {
        return NULL;
    }

    if (!resolve_symbol(resolver, sym)) {
        return NULL;
    }

    return sym;
}

static Symbol* resolve_export_name(Resolver* resolver, Identifier* name)
{
    Symbol* sym = module_get_export_sym(resolver->state.mod, name);

    if (!sym) {
        return NULL;
    }

    if (!resolve_symbol(resolver, sym)) {
        return NULL;
    }

    return sym;
}

static Symbol* lookup_ident(Resolver* resolver, NSIdent* ns_ident)
{
    //
    // Tries to lookup a symbol for an identifier in the form <module_namespace>::...::<identifier_name>
    //

    List* head = &ns_ident->idents;
    List* it = head->next;

    IdentNode* inode = list_entry(it, IdentNode, lnode);
    Symbol* sym = resolve_name(resolver, inode->ident);

    it = it->next;

    while (it != head) {
        if (!sym) {
            resolver_on_error(resolver, ns_ident->range, "Unknown namespace `%s`.", inode->ident->str);
            return NULL;
        }

        inode = list_entry(it, IdentNode, lnode);

        if (sym->kind == SYMBOL_MODULE) {
            StmtImport* stmt = (StmtImport*)sym->as_mod.stmt;
            Identifier* sym_name = get_import_sym_name(stmt, inode->ident);

            if (!sym_name) {
                resolver_on_error(resolver, ns_ident->range,
                                  "Identifier `%s` is not among the imported symbols in module namespace `%s`", inode->ident->str,
                                  sym->name->str);
                return NULL;
            }

            // Enter the namespace's module, and then try to lookup the identifier with its native name.
            ModuleState mod_state = enter_module(resolver, sym->as_mod.mod);
            sym = resolve_export_name(resolver, sym_name);
            exit_module(resolver, mod_state);
        }
        else if ((sym->kind == SYMBOL_TYPE) && (sym->decl->kind == CST_DeclEnum)) {
            Symbol** enum_items = sym->as_enum.items;
            size_t num_enum_items = sym->as_enum.num_items;
            Symbol* enum_item_sym = NULL;

            // Find enum item symbol.
            for (size_t ii = 0; ii < num_enum_items; ii++) {
                if (enum_items[ii]->name == inode->ident) {
                    enum_item_sym = enum_items[ii];
                    break;
                }
            }

            if (!enum_item_sym) {
                resolver_on_error(resolver, ns_ident->range, "Identifier `%s` is not a valid enum item of `%s`.", inode->ident->str,
                                  type_name(sym->type));
                return NULL;
            }

            sym = enum_item_sym;
        }
        else {
            resolver_on_error(resolver, ns_ident->range, "Symbol `%s` is not a valid namespace.", inode->ident->str);
            return NULL;
        }

        it = it->next;
    }

    return sym;
}

