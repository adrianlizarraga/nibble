#include "resolver/common.c"
#include "resolver/typespecs.c"
#include "resolver/exprs.c"
#include "resolver/stmts.c"
#include "resolver/decls.c"

bool resolve_module(Resolver* resolver, Module* mod)
{
    ModuleState mod_state = enter_module(resolver, mod);

    // Resolve declaration "headers". Will not resolve procedure bodies or complete aggregate types.
    List* sym_head = &mod->scope.sym_list;

    for (List* it = sym_head->next; it != sym_head; it = it->next) {
        Symbol* sym = list_entry(it, Symbol, lnode);

        assert(sym->home == mod);

        if (!resolve_symbol(resolver, sym))
            return false;
    }

    // Resolve global statements (e.g., #static_assert)
    List* head = &mod->stmts;

    for (List* it = head->next; it != head; it = it->next) {
        Stmt* stmt = list_entry(it, Stmt, lnode);

        if (stmt->kind != CST_StmtDecl) {
            if (!resolve_global_stmt(resolver, stmt))
                return false;
        }
    }

    exit_module(resolver, mod_state);

    return true;
}

bool resolve_reachable_sym_defs(Resolver* resolver)
{
    BucketList* procs = &resolver->ctx->procs;
    BucketList* aggregate_types = &resolver->ctx->aggregate_types;

    // NOTE: The procs bucket-list may grow during iteration if new proc symbols are encountered
    // while resolving proc/struct/union bodies.
    //
    // Therefore, _DO NOT CACHE_ procs->num_elems into a local variable.
    for (size_t i = 0; i < procs->num_elems; i += 1) {
        void** sym_ptr = bucket_list_get_elem_packed(procs, i);
        assert(sym_ptr);
        Symbol* sym = (Symbol*)(*sym_ptr);
        assert(sym->kind == SYMBOL_PROC);

        if (!resolve_global_proc_body(resolver, sym)) {
            return false;
        }
    }

    for (size_t i = 0; i < aggregate_types->num_elems; i += 1) {
        void** sym_ptr = bucket_list_get_elem_packed(aggregate_types, i);
        assert(sym_ptr);
        Symbol* sym = (Symbol*)(*sym_ptr);
        assert(sym->kind == SYMBOL_TYPE);

        if (!try_complete_aggregate_type(resolver, sym->type)) {
            return false;
        }
    }

    return true;
}
