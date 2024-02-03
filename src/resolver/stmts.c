#include <assert.h>
#include "allocator.h"
#include "array.h"
#include "ast/module.h"
#include "resolver/internal.h"

enum ResolveStmtRetFlags {
    RESOLVE_STMT_SUCCESS = 0x1,
    RESOLVE_STMT_RETURNS = 0x2,
    RESOLVE_STMT_LOOP_EXITS = 0x4,
};

enum ResolveStmtInFlags {
    RESOLVE_STMT_BREAK_CONTINUE_ALLOWED = 0x1,
    RESOLVE_STMT_VAR_DECL_DISALLOWED = 0x2, // Like in single-statement blocks, Ex: if (cond) var y : int = 1;
};

static unsigned resolve_stmt_block_body(Resolver* resolver, List* stmts, Type* ret_type, unsigned flags)
{
    unsigned ret_success = RESOLVE_STMT_SUCCESS;
    List* head = stmts;

    for (List* it = head->next; it != head; it = it->next) {
        Stmt* child_stmt = list_entry(it, Stmt, lnode);

        // Check for statement after return.
        if (ret_success & RESOLVE_STMT_RETURNS) {
            resolver_on_error(resolver, child_stmt->range, "Statement will never execute because all previous control paths return");

            ret_success &= ~RESOLVE_STMT_SUCCESS;
            break;
        }

        // Check for statement after break/continue
        if (ret_success & RESOLVE_STMT_LOOP_EXITS) {
            resolver_on_error(resolver, child_stmt->range,
                              "Statement will never execute because all previous control paths break or continue the loop");

            ret_success &= ~RESOLVE_STMT_SUCCESS;
            break;
        }

        unsigned r = resolve_stmt(resolver, child_stmt, ret_type, flags);

        // NOTE: Track whether any statement in the block returns from the parent procedure.
        ret_success = (r & RESOLVE_STMT_RETURNS) | (r & RESOLVE_STMT_LOOP_EXITS) | ret_success;

        if (!(r & RESOLVE_STMT_SUCCESS)) {
            ret_success &= ~RESOLVE_STMT_SUCCESS;
            break;
        }
    }

    return ret_success;
}

bool resolve_global_proc_body(Resolver* resolver, Symbol* sym)
{
    assert(sym->kind == SYMBOL_PROC);
    DeclProc* dproc = (DeclProc*)(sym->decl);

    if (dproc->is_incomplete) {
        return true;
    }

    ModuleState mod_state = enter_proc(resolver, sym);

    Type* ret_type = sym->type->as_proc.ret;
    unsigned r = resolve_stmt_block_body(resolver, &dproc->stmts, ret_type, 0);
    bool returns = r & RESOLVE_STMT_RETURNS;
    bool success = r & RESOLVE_STMT_SUCCESS;

    assert(!success || !(r & RESOLVE_STMT_LOOP_EXITS));

    dproc->returns = returns;

    if ((ret_type != builtin_types[BUILTIN_TYPE_VOID].type) && !returns && success) {
        resolver_on_error(resolver, dproc->super.range, "Not all code paths in procedure `%s` return a value", dproc->super.name->str);
        return false;
    }

    exit_proc(resolver, mod_state);

    return success;
}

static unsigned resolve_stmt_block(Resolver* resolver, Stmt* stmt, Type* ret_type, unsigned flags)
{
    StmtBlock* sblock = (StmtBlock*)stmt;

    sblock->scope = push_scope(resolver, sblock->num_decls);
    flags &= ~RESOLVE_STMT_VAR_DECL_DISALLOWED;
    unsigned ret_success = resolve_stmt_block_body(resolver, &sblock->stmts, ret_type, flags);

    pop_scope(resolver);

    return ret_success;
}

static bool resolve_cond_expr(Resolver* resolver, Expr* expr, ExprOperand* expr_eop)
{
    if (!resolve_expr(resolver, expr, NULL))
        return false;

    // TODO: THIS IS ERROR-PRONE. Will be buggy when add new fields.
    expr_eop->type = expr->type;
    expr_eop->is_constexpr = expr->is_constexpr;
    expr_eop->is_imm = expr->is_imm;
    expr_eop->is_lvalue = expr->is_lvalue;
    expr_eop->imm = expr->imm;

    CastResult r = convert_eop(expr_eop, builtin_types[BUILTIN_TYPE_BOOL].type, false);

    if (!r.success) {
        resolver_cast_error(resolver, r, expr->range, "Invalid condition type", expr_eop->type, builtin_types[BUILTIN_TYPE_BOOL].type);
        return false;
    }

    return true;
}

static unsigned resolve_stmt_if(Resolver* resolver, Stmt* stmt, Type* ret_type, unsigned flags)
{
    StmtIf* sif = (StmtIf*)stmt;

    // Resolve if block.
    ExprOperand cond_eop = {0};

    if (!resolve_cond_expr(resolver, sif->if_blk.cond, &cond_eop))
        return 0;

    sif->if_blk.cond = try_wrap_cast_expr(resolver, &cond_eop, sif->if_blk.cond);
    flags |= RESOLVE_STMT_VAR_DECL_DISALLOWED; // NOTE: Don't allow if (cond) var x:int = 1;
                                               // If the body is a {} block, this flag will be disabled.

    unsigned ret = resolve_stmt(resolver, sif->if_blk.body, ret_type, flags);

    if (!(ret & RESOLVE_STMT_SUCCESS)) {
        return 0;
    }

    // Resolve else block.
    if (sif->else_blk.body) {
        ret &= resolve_stmt(resolver, sif->else_blk.body, ret_type, flags);
    }
    else {
        ret &= ~RESOLVE_STMT_RETURNS;
        ret &= ~RESOLVE_STMT_LOOP_EXITS;
    }

    return ret;
}

static bool resolve_switch_case_expr(Resolver* resolver, Expr** expr_ptr, Type* switch_type)
{
    if (!resolve_expr(resolver, *expr_ptr, NULL)) {
        return false;
    }

    if (!(*expr_ptr)->is_imm || !(*expr_ptr)->is_constexpr) {
        resolver_on_error(resolver, (*expr_ptr)->range, "Case expression must be a compile-time constant");
        return false;
    }

    // If switching on an enum type, expect all cases to be enums.
    if (switch_type->kind == TYPE_ENUM) {
        if (switch_type != (*expr_ptr)->type) {
            // TODO: Print the enum type name.
            resolver_on_error(resolver, (*expr_ptr)->range, "Expected case expression to be an enum type");
            return false;
        }
    }
    // Otherwise, just cast the constant case expression to the expected integer type.
    else {
        ExprOperand eop = OP_FROM_EXPR((*expr_ptr));
        CastResult r = convert_eop(&eop, switch_type, false);
        assert(r.success); // Must be able to convert between integer types.
        *expr_ptr = try_wrap_cast_expr(resolver, &eop, *expr_ptr);
    }

    // Cast to either s64 or u64 depending on the switch expressions type/sign.
    const bool is_signed = type_is_signed(switch_type);
    Type* type_64bit = is_signed ? builtin_types[BUILTIN_TYPE_S64].type : builtin_types[BUILTIN_TYPE_U64].type;
    ExprOperand eop = OP_FROM_EXPR((*expr_ptr));
    CastResult r = convert_eop(&eop, type_64bit, false);
    assert(r.success); // Must be able to convert between integer types.
    *expr_ptr = try_wrap_cast_expr(resolver, &eop, *expr_ptr);
    return true;
}

static inline void qsort_swap_case(CaseInfo* cases, s64 i, s64 j)
{
    CaseInfo tmp = cases[i];
    cases[i] = cases[j];
    cases[j] = tmp;
}

static s64 qsort_partition_cases(CaseInfo* cases, s64 lo, s64 hi)
{
    // Pick the middle as a pivot because we assume case expressions are mostly sorted already.
    // Note: we then move the pivot to the end of the array.
    const s64 mid = lo + (hi - lo) / 2;
    qsort_swap_case(cases, mid, hi);

    CaseInfo pivot = cases[hi];
    const bool is_signed = pivot.is_signed;

    s64 i = lo;
    for (s64 j = lo; j < hi; j++) {
        // if cases[j].start <= pivot.start
        if (eval_binary_logical_op_64bit(TKN_LTEQ, is_signed, cases[j].start, pivot.start)) {
            qsort_swap_case(cases, j, i);
            i++;
        }
    }

    qsort_swap_case(cases, hi, i);
    return i;
}

static void qsort_cases(CaseInfo* cases, s64 lo, s64 hi)
{
    if (lo < 0 || lo >= hi) {
        return;
    }

    s64 pivot_index = qsort_partition_cases(cases, lo, hi);

    // Recursively sort the two partitions.
    qsort_cases(cases, lo, pivot_index - 1);
    qsort_cases(cases, pivot_index + 1, hi);
}

static bool switch_stmt_is_exhaustive(const StmtSwitch* stmt, Array(const CaseInfo) case_infos)
{
    if (stmt->has_default_case) {
        return true;
    }

    const Type* type = stmt->expr->type;
    NIBBLE_UNUSED_VAR(case_infos);

    if (type->kind == TYPE_INTEGER) {
        // TODO: Check if cases cover the entire integer range.
        return false;
    }

    assert(type->kind == TYPE_ENUM);
    // TODO: Check if cases cover all possible enum values.

    return false;
}

static unsigned resolve_stmt_switch(Resolver* resolver, StmtSwitch* stmt, Type* ret_type, unsigned flags)
{
    if (!resolve_expr(resolver, stmt->expr, NULL)) {
        return 0;
    }

    if (!type_is_integer_like(stmt->expr->type)) {
        resolver_on_error(resolver, stmt->expr->range, "Switch statement expression must be of an integer type");
        return 0;
    }

    if (stmt->num_cases == 0) {
        resolver_on_error(resolver, stmt->super.range, "Switch statement must case at least one case");
        return 0;
    }

    // Can't have more cases than can fit in a u32.
    if (stmt->num_cases > int_kind_max[INTEGER_U32]) {
        resolver_on_error(resolver, stmt->super.range, "Switch statement is too large. Cannot have more cases than can fit in a u32.");
        return 0;
    }

    Allocator* tmp_arena = &resolver->ctx->tmp_mem;
    AllocatorState mem_state = allocator_get_state(tmp_arena);

    const bool is_signed = type_is_signed(stmt->expr->type);
    Array(CaseInfo) case_infos = array_create(tmp_arena, CaseInfo, stmt->num_cases);
    bool any_case_no_return = false;
    bool any_case_no_loop_exit = false;

    // Type-check cases
    for (u32 i = 0; i < stmt->num_cases; i++) {
        SwitchCase* scase = stmt->cases[i];

        // Type check case's start and end expressions.
        if (scase->start) {
            if (!resolve_switch_case_expr(resolver, &scase->start, stmt->expr->type)) {
                allocator_restore_state(mem_state);
                return 0;
            }

            const Scalar start_val = scase->start->imm;
            Scalar end_val = start_val;

            if (scase->end) {
                if (!resolve_switch_case_expr(resolver, &scase->end, stmt->expr->type)) {
                    allocator_restore_state(mem_state);
                    return 0;
                }

                assert(scase->start->type == scase->end->type);
                end_val = scase->end->imm;

                // Check that end > start
                if (eval_binary_logical_op_64bit(TKN_LTEQ, is_signed, end_val, start_val)) { // end_val <= start_val
                    ProgRange range = merge_ranges(scase->start->range, scase->end->range);
                    resolver_on_error(resolver, range, "Case `end` expression must be strictly greater than `start`");
                    allocator_restore_state(mem_state);
                    return 0;
                }
            }

            array_push(case_infos, (CaseInfo){.start = start_val, .end = end_val, .is_signed = is_signed, .index = i});
        }

        // TODO: Error if a switch on an enum does not cover all possible values.

        scase->scope = push_scope(resolver, scase->num_decls);

        unsigned r = resolve_stmt_block_body(resolver, &scase->stmts, ret_type, flags);

        if (!(r & RESOLVE_STMT_RETURNS)) {
            any_case_no_return = true;
        }

        if (!(r & RESOLVE_STMT_LOOP_EXITS)) {
            any_case_no_loop_exit = true;
        }

        pop_scope(resolver);

        if (!(r & RESOLVE_STMT_SUCCESS)) {
            allocator_restore_state(mem_state);
            return 0;
        }
    }

    // Sort cases by start value and check for duplicates and/or intersecting ranges.
    if (array_len(case_infos) > 0) {
        qsort_cases(case_infos, 0, array_len(case_infos) - 1);

        const CaseInfo* prev = &case_infos[0];
        for (u32 i = 1; i < array_len(case_infos); i++) {
            const CaseInfo* curr = &case_infos[i];

            // if curr->start <= prev->end
            if (eval_binary_logical_op_64bit(TKN_LTEQ, is_signed, curr->start, prev->end)) { // Intersection!
                resolver_on_error(resolver, stmt->cases[curr->index]->range, "Repeated switch case value");
                allocator_restore_state(mem_state);
                return 0;
            }

            // if curr->end > prev->end
            if (eval_binary_logical_op_64bit(TKN_GT, is_signed, curr->end, prev->end)) {
                prev = curr;
            }
        }
    }

    const bool is_exhaustive = switch_stmt_is_exhaustive(stmt, case_infos);
    unsigned ret = RESOLVE_STMT_SUCCESS;

    if (is_exhaustive && !any_case_no_return) {
        ret |= RESOLVE_STMT_RETURNS;
    }

    if (is_exhaustive && !any_case_no_loop_exit) {
        ret |= RESOLVE_STMT_LOOP_EXITS;
    }

    // Store information for sorted cases.
    stmt->num_case_infos = array_len(case_infos);
    stmt->case_infos = alloc_array(&resolver->ctx->ast_mem, CaseInfo, stmt->num_case_infos, false);
    memcpy(stmt->case_infos, case_infos, stmt->num_case_infos * sizeof(CaseInfo));

    allocator_restore_state(mem_state);
    return ret;
}

static unsigned resolve_stmt_for(Resolver* resolver, StmtFor* stmt_for, Type* ret_type, unsigned flags)
{
    stmt_for->scope = push_scope(resolver, 2); // At most 1 variable declaration in for-loop's init statement.

    unsigned flags_no_break = flags;
    flags_no_break &= ~RESOLVE_STMT_BREAK_CONTINUE_ALLOWED;

    unsigned ret = RESOLVE_STMT_SUCCESS;

    // Init statement.
    if (stmt_for->init) {
        ret &= resolve_stmt(resolver, stmt_for->init, ret_type, flags_no_break);

        if (!(ret & RESOLVE_STMT_SUCCESS)) {
            return 0;
        }

        // Throw an error if the init statement returns.
        if (ret & RESOLVE_STMT_RETURNS) {
            resolver_on_error(resolver, stmt_for->init->range, "For-loop body will never execute");
            return 0;
        }
    }

    // Condition expression.
    if (stmt_for->cond) {
        ExprOperand cond_eop = {0};

        if (!resolve_cond_expr(resolver, stmt_for->cond, &cond_eop)) {
            return 0;
        }

        stmt_for->cond = try_wrap_cast_expr(resolver, &cond_eop, stmt_for->cond);
    }

    // Loop body.
    ret &= resolve_stmt(resolver, stmt_for->body, ret_type,
                        flags | RESOLVE_STMT_BREAK_CONTINUE_ALLOWED | RESOLVE_STMT_VAR_DECL_DISALLOWED);

    if (!(ret & RESOLVE_STMT_SUCCESS)) {
        return 0;
    }

    // Next iteration statement.
    if (stmt_for->next) {
        ret &= resolve_stmt(resolver, stmt_for->next, ret_type, flags_no_break);
    }

    // NOTE: Because for loops don't have an "else" path, we can't say that all control paths return.
    // TODO: Add else to for-loop!!
    ret &= ~RESOLVE_STMT_RETURNS;
    ret &= ~RESOLVE_STMT_LOOP_EXITS; // Break/continue do not propagate out from loops.

    pop_scope(resolver);

    return ret;
}

static unsigned resolve_stmt_while(Resolver* resolver, Stmt* stmt, Type* ret_type, unsigned flags)
{
    StmtWhile* swhile = (StmtWhile*)stmt;
    ExprOperand cond_eop = {0};

    // Resolve condition expression.
    if (!resolve_cond_expr(resolver, swhile->cond, &cond_eop))
        return 0;

    swhile->cond = try_wrap_cast_expr(resolver, &cond_eop, swhile->cond);

    flags |= (RESOLVE_STMT_VAR_DECL_DISALLOWED | RESOLVE_STMT_BREAK_CONTINUE_ALLOWED);

    // Resolve loop body.
    unsigned ret = resolve_stmt(resolver, swhile->body, ret_type, flags);

    // NOTE: Because while loops don't have an "else" path, we can't say that all control paths return.
    // TODO: Add else to while loop!!
    ret &= ~RESOLVE_STMT_RETURNS;
    ret &= ~RESOLVE_STMT_LOOP_EXITS; // Break/continue do not propagate out from loops.

    return ret;
}

static unsigned resolve_stmt_do_while(Resolver* resolver, Stmt* stmt, Type* ret_type, unsigned flags)
{
    StmtDoWhile* swhile = (StmtDoWhile*)stmt;
    ExprOperand cond_eop = {0};

    // Resolve condition expression.
    if (!resolve_cond_expr(resolver, swhile->cond, &cond_eop))
        return 0;

    swhile->cond = try_wrap_cast_expr(resolver, &cond_eop, swhile->cond);

    flags |= (RESOLVE_STMT_VAR_DECL_DISALLOWED | RESOLVE_STMT_BREAK_CONTINUE_ALLOWED);

    // Resolve loop body.
    unsigned ret = resolve_stmt(resolver, swhile->body, ret_type, flags);

    // Report an error if the do-while loop always returns before the condition check.
    if (ret & RESOLVE_STMT_RETURNS) {
        resolver_on_error(resolver, swhile->cond->range, "All paths in do-while loop's body return before condition check.");
        ret &= ~RESOLVE_STMT_SUCCESS;
    }

    // Report an error if the do-while loop always breaks out before condition check.
    // TODO: Continue should be ok?
    if (ret & RESOLVE_STMT_LOOP_EXITS) {
        resolver_on_error(resolver, swhile->cond->range,
                          "All paths in do-while loop's body break or continue before condition check.");
        ret &= ~RESOLVE_STMT_SUCCESS;
    }

    return ret;
}

static unsigned resolve_stmt_expr_assign(Resolver* resolver, Stmt* stmt)
{
    StmtExprAssign* sassign = (StmtExprAssign*)stmt;
    Expr* lhs_expr = sassign->left;
    Expr* rhs_expr = sassign->right;

    if (!resolve_expr(resolver, lhs_expr, NULL))
        return 0;

    if (!resolve_expr(resolver, rhs_expr, NULL))
        return 0;

    if (!lhs_expr->is_lvalue) {
        resolver_on_error(resolver, lhs_expr->range, "Left side of assignment statement must be an l-value");
        return 0;
    }

    TokenKind op_assign = sassign->op_assign;

    switch (op_assign) {
    case TKN_ASSIGN: {
        ExprOperand rhs_eop = OP_FROM_EXPR(rhs_expr);
        CastResult r = convert_eop(&rhs_eop, lhs_expr->type, true);

        if (!r.success) {
            resolver_cast_error(resolver, r, rhs_expr->range, "Invalid assignment statement", rhs_eop.type, lhs_expr->type);
            return 0;
        }

        sassign->right = try_wrap_cast_expr(resolver, &rhs_eop, sassign->right);
        break;
    }
    case TKN_ADD_ASSIGN:
    case TKN_SUB_ASSIGN: {
        ExprOperand left_op = OP_FROM_EXPR(lhs_expr);
        ExprOperand right_op = OP_FROM_EXPR(rhs_expr);
        ExprOperand binary_op = {0};

        // Initialize strings used for error messages.
        const char* op_name;
        const char* prep_str;
        if (op_assign == TKN_ADD_ASSIGN) {
            op_name = "add";
            prep_str = "to";
        }
        else {
            op_name = "subtract";
            prep_str = "from";
        }

        // Resolve left and right operands of a "+" or "-" expression.
        if (type_is_arithmetic(left_op.type) && type_is_arithmetic(right_op.type)) {
            resolve_non_const_binary_eop(&binary_op, &left_op, &right_op);
        }
        else if ((left_op.type->kind == TYPE_PTR) && type_is_integer_like(right_op.type)) {
            if (!try_complete_aggregate_type(resolver, left_op.type->as_ptr.base)) {
                return 0;
            }

            if (!resolve_ptr_int_arith(resolver, &binary_op, &left_op, &right_op)) {
                resolver_on_error(resolver, lhs_expr->range, "Cannot %s %s a pointer with a base type (%s) of zero size", op_name,
                                  prep_str, type_name(left_op.type->as_ptr.base));

                return 0;
            }
        }
        else {
            resolver_on_error(resolver, stmt->range, "Cannot %s a value of type `%s` %s a `%s` in a compound assignment statement.",
                              op_name, type_name(right_op.type), prep_str, type_name(left_op.type));
            return 0;
        }

        // Ensure that binary operation's result can be implicitly converted to lhs's type.
        CastResult r = convert_eop(&binary_op, lhs_expr->type, true);

        if (!r.success) {
            resolver_cast_error(resolver, r, stmt->range, "Invalid compound assignment statement", binary_op.type, lhs_expr->type);
            return 0;
        }

        // Only cast right subexpression. Bytecode generator will manually cast a copy of lhs to the same type (if necessary).
        sassign->right = try_wrap_cast_expr(resolver, &right_op, rhs_expr);
        break;
    }
    case TKN_MUL_ASSIGN:
    case TKN_DIV_ASSIGN:
    case TKN_MOD_ASSIGN: {
        ExprOperand left_op = OP_FROM_EXPR(lhs_expr);
        ExprOperand right_op = OP_FROM_EXPR(rhs_expr);
        ExprOperand binary_op = {0};

        // Resolve left and right operands of a binary "*", "/", or "%" expression.
        if (!type_is_arithmetic(left_op.type)) {
            resolver_on_error(resolver, lhs_expr->range, "Left-hand side of operator `%s` must be an arithmetic type, not type `%s`",
                              token_kind_names[op_assign], type_name(left_op.type));
            return 0;
        }

        if (!type_is_arithmetic(right_op.type)) {
            resolver_on_error(resolver, rhs_expr->range, "Right-hand side of operator `%s` must be an arithmetic type, not type `%s`",
                              token_kind_names[op_assign], type_name(right_op.type));
            return 0;
        }

        resolve_non_const_binary_eop(&binary_op, &left_op, &right_op);

        // Ensure that binary operation's result can be implicitly converted to lhs's type.
        CastResult r = convert_eop(&binary_op, lhs_expr->type, true);

        if (!r.success) {
            resolver_cast_error(resolver, r, stmt->range, "Invalid compound assignment statement", binary_op.type, lhs_expr->type);
            return 0;
        }

        // Only cast right subexpression. Bytecode generator will manually cast a copy of lhs to the same type (if necessary).
        sassign->right = try_wrap_cast_expr(resolver, &right_op, rhs_expr);
        break;
    }
    case TKN_AND_ASSIGN:
    case TKN_OR_ASSIGN:
    case TKN_XOR_ASSIGN: {
        ExprOperand left_op = OP_FROM_EXPR(lhs_expr);
        ExprOperand right_op = OP_FROM_EXPR(rhs_expr);
        ExprOperand binary_op = {0};

        // Resolve left and right operands of a binary "*", "/", or "%" expression.
        if (!type_is_integer_like(left_op.type)) {
            resolver_on_error(resolver, lhs_expr->range, "Left-hand side of operator `%s` must be an integer type, not type `%s`",
                              token_kind_names[op_assign], type_name(left_op.type));
            return 0;
        }

        if (!type_is_integer_like(right_op.type)) {
            resolver_on_error(resolver, rhs_expr->range, "Right-hand side of operator `%s` must be an integer type, not type `%s`",
                              token_kind_names[op_assign], type_name(right_op.type));
            return 0;
        }

        resolve_non_const_binary_eop(&binary_op, &left_op, &right_op);

        // Ensure that binary operation's result can be implicitly converted to lhs's type.
        CastResult r = convert_eop(&binary_op, lhs_expr->type, true);

        if (!r.success) {
            resolver_cast_error(resolver, r, stmt->range, "Invalid compound assignment statement", binary_op.type, lhs_expr->type);
            return 0;
        }

        // Only cast right subexpression. Bytecode generator will manually cast a copy of lhs to the same type (if necessary).
        sassign->right = try_wrap_cast_expr(resolver, &right_op, rhs_expr);
        break;
    }
    case TKN_RSHIFT_ASSIGN:
    case TKN_LSHIFT_ASSIGN: {
        ExprOperand left_op = OP_FROM_EXPR(lhs_expr);
        ExprOperand right_op = OP_FROM_EXPR(rhs_expr);

        if (left_op.type->kind != TYPE_INTEGER) {
            resolver_on_error(resolver, lhs_expr->range, "Left-hand side of operator `%s` must be an integer type, not type `%s`",
                              token_kind_names[op_assign], type_name(left_op.type));
            return 0;
        }

        if (right_op.type->kind != TYPE_INTEGER) {
            resolver_on_error(resolver, rhs_expr->range, "Right-hand side of operator `%s` must be an integer type, not type `%s`",
                              token_kind_names[op_assign], type_name(right_op.type));
            return 0;
        }

        break;
    }
    default:
        resolver_on_error(resolver, stmt->range, "Sorry! Only the `=` assignment operator is currently supported. Soon!");
        return 0;
    }

    return RESOLVE_STMT_SUCCESS;
}

static bool resolve_static_assert(Resolver* resolver, StmtStaticAssert* sassert)
{
    if (!resolve_expr(resolver, sassert->cond, NULL)) {
        return false;
    }

    if (!(sassert->cond->is_constexpr && sassert->cond->is_imm)) {
        resolver_on_error(resolver, sassert->cond->range, "#static_assert condition must be a compile-time constant expression");
        return false;
    }

    if (sassert->cond->imm.as_int._u32 == 0) {
        const char* msg_pre = "static assertion failed";

        if (sassert->msg)
            resolver_on_error(resolver, sassert->super.range, "%s: %s", msg_pre, sassert->msg->str);
        else
            resolver_on_error(resolver, sassert->super.range, "%s", msg_pre);

        return false;
    }

    return true;
}

bool resolve_global_stmt(Resolver* resolver, Stmt* stmt)
{
    switch (stmt->kind) {
    case CST_StmtStaticAssert: {
        StmtStaticAssert* sassert = (StmtStaticAssert*)stmt;

        return resolve_static_assert(resolver, sassert);
    }
    default:
        assert(0);
        break;
    }

    return false;
}

static unsigned resolve_stmt_decl(Resolver* resolver, StmtDecl* sdecl, unsigned flags)
{
    Decl* decl = sdecl->decl;
    Scope* scope = resolver->state.scope;

    // Check that is an allowed declaration kind.
    // TODO: Support other declaration kinds.
    if (decl->kind != CST_DeclVar && decl->kind != CST_DeclConst) {
        resolver_on_error(resolver, sdecl->super.range, "Only variable and type declarations are supported inside procedures");
        return 0;
    }

    if (flags & RESOLVE_STMT_VAR_DECL_DISALLOWED) {
        resolver_on_error(resolver, sdecl->super.range,
                          "Variable declarations not allowed in single-statement bodies. "
                          "Consider using a `{}` block.");
        return 0;
    }

    // Resolve the symbol.
    Symbol* sym = add_unresolved_symbol(&resolver->ctx->ast_mem, scope, resolver->state.mod, decl);

    if (!sym) {
        resolver_on_error(resolver, sdecl->super.range, "Identifier `%s` shadows a previous local declaration", decl->name->str);
        return 0;
    }

    if ((decl->kind == CST_DeclVar) && resolve_decl_var(resolver, sym)) {
        return RESOLVE_STMT_SUCCESS;
    }

    if ((decl->kind == CST_DeclConst) && resolve_decl_const(resolver, sym)) {
        return RESOLVE_STMT_SUCCESS;
    }

    return 0; // Failed to resolve/type-check.
}

unsigned resolve_stmt(Resolver* resolver, Stmt* stmt, Type* ret_type, unsigned flags)
{
    unsigned ret = 0;
    bool break_continue_allowed = flags & RESOLVE_STMT_BREAK_CONTINUE_ALLOWED;

    switch (stmt->kind) {
    case CST_StmtNoOp: {
        ret = RESOLVE_STMT_SUCCESS;
        break;
    }
    case CST_StmtStaticAssert: {
        StmtStaticAssert* sassert = (StmtStaticAssert*)stmt;

        ret = resolve_static_assert(resolver, sassert) ? RESOLVE_STMT_SUCCESS : 0;
        break;
    }
    case CST_StmtReturn: {
        ret = RESOLVE_STMT_RETURNS;
        StmtReturn* sret = (StmtReturn*)stmt;
        Type* type_void = builtin_types[BUILTIN_TYPE_VOID].type;

        if (!sret->expr && (ret_type != type_void)) {
            resolver_on_error(resolver, stmt->range, "Return statement is missing a return value of type `%s`", type_name(ret_type));
            break;
        }

        if (sret->expr && (ret_type == type_void)) {
            resolver_on_error(resolver, stmt->range, "Procedure with a `void` return type cannot return a value");
            break;
        }

        if (sret->expr) {
            if (!resolve_expr(resolver, sret->expr, ret_type))
                break;

            ExprOperand ret_eop = OP_FROM_EXPR(sret->expr);

            CastResult r = convert_eop(&ret_eop, ret_type, true);

            if (!r.success) {
                resolver_cast_error(resolver, r, sret->expr->range, "Invalid return type", ret_eop.type, ret_type);
                break;
            }

            sret->expr = try_wrap_cast_expr(resolver, &ret_eop, sret->expr);
        }

        ret |= RESOLVE_STMT_SUCCESS;
        break;
    }
    case CST_StmtBreak: {
        if (break_continue_allowed)
            ret = RESOLVE_STMT_SUCCESS | RESOLVE_STMT_LOOP_EXITS;
        else
            resolver_on_error(resolver, stmt->range, "Illegal break statement");

        break;
    }
    case CST_StmtContinue: {
        if (break_continue_allowed)
            ret = RESOLVE_STMT_SUCCESS | RESOLVE_STMT_LOOP_EXITS;
        else
            resolver_on_error(resolver, stmt->range, "Illegal continue statement");

        break;
    }
    case CST_StmtIf: {
        ret = resolve_stmt_if(resolver, stmt, ret_type, flags);
        break;
    }
    case CST_StmtFor: {
        ret = resolve_stmt_for(resolver, (StmtFor*)stmt, ret_type, flags);
        break;
    }
    case CST_StmtWhile: {
        ret = resolve_stmt_while(resolver, stmt, ret_type, flags);
        break;
    }
    case CST_StmtDoWhile: {
        ret = resolve_stmt_do_while(resolver, stmt, ret_type, flags);
        break;
    }
    case CST_StmtExpr: {
        StmtExpr* sexpr = (StmtExpr*)stmt;

        if (resolve_expr(resolver, sexpr->expr, NULL))
            ret = RESOLVE_STMT_SUCCESS;

        break;
    }
    case CST_StmtExprAssign: {
        ret = resolve_stmt_expr_assign(resolver, stmt);
        break;
    }
    case CST_StmtDecl: {
        ret = resolve_stmt_decl(resolver, (StmtDecl*)stmt, flags);
        break;
    }
    case CST_StmtBlock: {
        ret = resolve_stmt_block(resolver, stmt, ret_type, flags);
        break;
    }
    case CST_StmtSwitch: {
        ret = resolve_stmt_switch(resolver, (StmtSwitch*)stmt, ret_type, flags);
        break;
    }
    default:
        NIBBLE_FATAL_EXIT("Unsupported statement kind `%d` in Resolver.", stmt->kind);
        break;
    }

    return ret;
}
