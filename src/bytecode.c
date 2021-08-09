#include "ast.h"
#include "bytecode.h"
#include "stream.h"
#include "print_ir.h"

//////////////////////////////////////////////////////
//
//         Create IR instructions
//
//////////////////////////////////////////////////////
static void IR_add_instr(IR_Builder* builder, IR_Instr* instr)
{
    array_push(builder->curr_proc->as_proc.instrs, instr);
}

static IR_Instr* IR_new_instr(Allocator* arena, IR_InstrKind kind)
{
    IR_Instr* instr = alloc_type(arena, IR_Instr, true);
    instr->kind = kind;

    return instr;
}

static void IR_emit_instr_add(IR_Builder* builder, Type* type, IR_Reg out_reg, IR_InstrArg arg1, IR_InstrArg arg2)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_ADD);
    instr->_add.type = type;
    instr->_add.out_reg = out_reg;
    instr->_add.arg1 = arg1;
    instr->_add.arg2 = arg2;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_sub(IR_Builder* builder, Type* type, IR_Reg out_reg, IR_InstrArg arg1, IR_InstrArg arg2)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_SUB);
    instr->_sub.type = type;
    instr->_sub.out_reg = out_reg;
    instr->_sub.arg1 = arg1;
    instr->_sub.arg2 = arg2;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_shr(IR_Builder* builder, Type* type, IR_Reg out_reg, IR_InstrArg arg1, IR_InstrArg arg2)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_SHR);
    instr->_shr.type = type;
    instr->_shr.out_reg = out_reg;
    instr->_shr.arg1 = arg1;
    instr->_shr.arg2 = arg2;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_store(IR_Builder* builder, Type* type, IR_SIBDAddr addr, IR_InstrArg arg)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_STORE);
    instr->_store.type = type;
    instr->_store.addr = addr;
    instr->_store.arg = arg;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_load(IR_Builder* builder, Type* type, IR_Reg out_reg, IR_SIBDAddr addr)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_LOAD);
    instr->_load.type = type;
    instr->_load.out_reg = out_reg;
    instr->_load.addr = addr;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_laddr(IR_Builder* builder, Type* type, IR_Reg out_reg, IR_SIBDAddr addr)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_LADDR);
    instr->_laddr.type = type;
    instr->_laddr.out_reg = out_reg;
    instr->_laddr.addr = addr;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_laddr_var(IR_Builder* builder, IR_Reg out_reg, Symbol* sym)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_LADDR_VAR);
    instr->_laddr_var.out_reg = out_reg;
    instr->_laddr_var.sym = sym;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_ret(IR_Builder* builder, Type* type, IR_InstrArg ret_arg)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_RET);
    instr->_ret.type = type;
    instr->_ret.ret_arg = ret_arg;

    IR_add_instr(builder, instr);
}

static void IR_emit_instr_cmp(IR_Builder* builder, IR_ConditionKind cmp_kind, Type* type, IR_Reg out_reg, IR_InstrArg arg1,
                              IR_InstrArg arg2)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_CMP);
    instr->_cmp.kind = cmp_kind;
    instr->_cmp.type = type;
    instr->_cmp.out_reg = out_reg;
    instr->_cmp.arg1 = arg1;
    instr->_cmp.arg2 = arg2;

    IR_add_instr(builder, instr);
}

static IR_Instr* IR_emit_instr_cjmp(IR_Builder* builder, IR_Reg cond_reg, u32 jmp_target)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_CJMP);
    instr->_cjmp.cond_reg = cond_reg;
    instr->_cjmp.jmp_target = jmp_target;

    IR_add_instr(builder, instr);

    return instr;
}

static IR_Instr* IR_emit_instr_jmp(IR_Builder* builder, u32 jmp_target)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_JMP);
    instr->_jmp.jmp_target = jmp_target;

    IR_add_instr(builder, instr);

    return instr;
}

static void IR_patch_jmp_instr(IR_Instr* jmp_instr, u32 jmp_target)
{
    switch (jmp_instr->kind)
    {
        case IR_INSTR_JMP:
            jmp_instr->_jmp.jmp_target = jmp_target;
            break;
        case IR_INSTR_CJMP:
            jmp_instr->_cjmp.jmp_target = jmp_target;
            break;
        default:
            assert(0);
            break;
    }
}

static u32 IR_get_jmp_target(IR_Builder* builder)
{
    return (u32)array_len(builder->curr_proc->as_proc.instrs);
}

//////////////////////////////////////////////////////
//
//      Utils for traversing AST to emit IR.
//
//////////////////////////////////////////////////////

typedef enum IR_OperandKind {
    IR_OPERAND_NONE,
    IR_OPERAND_IMM,
    IR_OPERAND_REG,
    IR_OPERAND_SIBD_ADDR,
    IR_OPERAND_DEREF_ADDR,
    IR_OPERAND_VAR,
    IR_OPERAND_PROC,
} IR_OperandKind;

typedef struct IR_Operand {
    IR_OperandKind kind;
    Type* type;

    union {
        Scalar imm;
        IR_Reg reg;
        IR_SIBDAddr addr;
        Symbol* sym;
    };
} IR_Operand;

static IR_Reg IR_next_reg(IR_Builder* builder)
{
    builder->curr_proc->as_proc.num_regs += 1;
    return builder->curr_proc->as_proc.num_regs;
}

static void IR_execute_deref(IR_Builder* builder, IR_Operand* operand)
{
    assert(operand->kind == IR_OPERAND_DEREF_ADDR);

    IR_Reg out_reg = IR_next_reg(builder);

    IR_emit_instr_load(builder, operand->type, out_reg, operand->addr);

    operand->kind = IR_OPERAND_REG;
    operand->reg = out_reg;
}

static void IR_execute_lea(IR_Builder* builder, IR_Operand* operand)
{
    assert(operand->kind == IR_OPERAND_SIBD_ADDR);

    // The operand currently holds a memory address.
    // This function executes the "load-effective-address" call into the one of the registers that held the address.
    IR_SIBDAddr addr = operand->addr;

    bool has_base = addr.base_reg > 0;
    bool has_index = addr.scale && (addr.index_reg > 0);
    bool has_disp = addr.disp != 0;
    IR_Reg dst_reg;

    assert(has_base || has_index);

    if (has_base && !has_index && !has_disp)
    {
        // No need to emit any instructions. Just keep address in base register.
        dst_reg = operand->addr.base_reg;
    }
    else
    {
        IR_Reg dst_reg = IR_next_reg(builder);

        IR_emit_instr_laddr(builder, operand->type, dst_reg, addr);
    }

    operand->kind = IR_OPERAND_REG;
    operand->reg = dst_reg;
}

static void IR_commit_indirections(IR_Builder* builder, IR_Operand* operand)
{
    if (operand->kind == IR_OPERAND_DEREF_ADDR)
        IR_execute_deref(builder, operand);
    else if (operand->kind == IR_OPERAND_SIBD_ADDR)
        IR_execute_lea(builder, operand);
}

static void IR_ensure_operand_in_reg(IR_Builder* builder, IR_Operand* operand, bool commit_ptr)
{
    if (commit_ptr && (operand->kind == IR_OPERAND_SIBD_ADDR))
    {
        IR_execute_lea(builder, operand);
    }
    if (operand->kind == IR_OPERAND_DEREF_ADDR)
    {
        IR_execute_deref(builder, operand);
    }
    else if (operand->kind != IR_OPERAND_REG)
    {
        assert(operand->kind == IR_OPERAND_VAR);

        if (!commit_ptr && (operand->type->kind == TYPE_PTR))
        {
            if (operand->kind != IR_OPERAND_SIBD_ADDR)
            {
                // Assume sym->as_var.reg is a register that contains the result from an
                // alloca instruction (i.e., contains an address to the var).
                IR_SIBDAddr var_addr = {.base_reg = operand->sym->as_var.reg};
                IR_Reg base_reg = IR_next_reg(builder);
                IR_emit_instr_load(builder, operand->type, base_reg, var_addr);

                operand->kind = IR_OPERAND_SIBD_ADDR;
                operand->addr.base_reg = base_reg;
                operand->addr.index_reg = 0;
                operand->addr.scale = 0;
                operand->addr.disp = 0;
            }
        }
        else
        {
            IR_SIBDAddr var_addr = {.base_reg = operand->sym->as_var.reg};
            IR_Reg reg = IR_next_reg(builder);
            IR_emit_instr_load(builder, operand->type, reg, var_addr);

            operand->kind = IR_OPERAND_REG;
            operand->reg = reg;
        }
    }
}

static IR_SIBDAddr IR_get_var_addr(IR_Builder* builder, Symbol* sym)
{
    assert(sym->kind == SYMBOL_VAR);

    IR_SIBDAddr var_addr = {0};

    if (!sym->as_var.reg)
    {
        sym->as_var.reg = IR_next_reg(builder);

        IR_emit_instr_laddr_var(builder, sym->as_var.reg, sym);
    }

    var_addr.base_reg = sym->as_var.reg;

    return var_addr;
}

static void IR_arg_from_operand(IR_Builder* builder, IR_InstrArg* arg, IR_Operand* op)
{
    switch (op->kind)
    {
        case IR_OPERAND_IMM:
            arg->kind = IR_ARG_IMM;
            arg->imm = op->imm;
            break;
        case IR_OPERAND_REG:
            arg->kind = IR_ARG_REG;
            arg->reg = op->reg;
            break;
        case IR_OPERAND_VAR:
        {
            // Assume sym->as_var.reg is a register that contains the result from an
            // alloca instruction (i.e., contains an address to the var).
            IR_SIBDAddr addr = {.base_reg = op->sym->as_var.reg};
            IR_Reg out_reg = IR_next_reg(builder);
            IR_emit_instr_load(builder, op->type, out_reg, addr);

            op->kind = IR_OPERAND_REG;
            op->reg = out_reg;

            arg->kind = IR_ARG_REG;
            arg->reg = out_reg;
            break;
        }
        case IR_OPERAND_DEREF_ADDR:
            IR_execute_deref(builder, op);

            arg->kind = IR_ARG_REG;
            arg->reg = op->reg;
            break;
        default:
            assert(!0);
            break;
    }
}

static void IR_next_reg_operand(IR_Builder* builder, IR_Operand* dst, Type* type)
{
    dst->kind = IR_OPERAND_REG;
    dst->type = type;
    dst->reg = IR_next_reg(builder);
}

//////////////////////////////////////////////////////
//
//      Traverse AST to emit IR.
//
//////////////////////////////////////////////////////
static void IR_operand_from_sym(IR_Operand* op, Symbol* sym)
{
    if (sym->kind == SYMBOL_VAR)
    {
        op->kind = IR_OPERAND_VAR;
        op->type = sym->type;
        op->sym = sym;
    }
    else if (sym->kind == SYMBOL_PROC)
    {
        op->kind = IR_OPERAND_PROC;
        op->type = sym->type;
        op->sym = sym;
    }
    else
    {
        assert(0);
    }
}

static void IR_emit_expr(IR_Builder* builder, Expr* expr, IR_Operand* dst);

static void IR_emit_expr_ident(IR_Builder* builder, ExprIdent* eident, IR_Operand* dst)
{
    Symbol* sym = lookup_symbol(builder->curr_scope, eident->name);
    IR_operand_from_sym(dst, sym);
}

static void IR_emit_ptr_int_add(IR_Builder* builder, IR_Operand* dst, IR_Operand* ptr_op, IR_Operand* int_op, bool add)
{
    u64 base_size = ptr_op->type->as_ptr.base->size;

    // TODO: Replace the following with a call to just load ptr to reg and
    // get rid of boolean argument in IR_ensure_operand_in_reg()
    IR_ensure_operand_in_reg(builder, ptr_op, false);

    if (int_op->kind == IR_OPERAND_IMM)
    {
        if (add)
            ptr_op->addr.disp += base_size * int_op->imm.as_int._u64;
        else
            ptr_op->addr.disp -= base_size * int_op->imm.as_int._u64;
    }
    else
    {
        if (ptr_op->addr.scale)
        {
            IR_Reg index_reg = IR_next_reg(builder);
            IR_InstrArg a = {.kind = IR_ARG_REG, .reg = ptr_op->addr.index_reg};
            IR_InstrArg b = {0};

            IR_arg_from_operand(builder, &b, int_op);

            if (add)
                IR_emit_instr_add(builder, type_s64, index_reg, a, b);
            else
                IR_emit_instr_sub(builder, type_s64, index_reg, a, b);

            ptr_op->addr.index_reg = index_reg;
        }
        else
        {
            IR_ensure_operand_in_reg(builder, int_op, true);

            if (!add)
            {
                IR_Reg index_reg = IR_next_reg(builder);
                IR_InstrArg zero = {.kind = IR_ARG_IMM, .imm.as_int._u64 = 0};
                IR_InstrArg arg = {.kind = IR_ARG_REG, .reg = int_op->reg};

                IR_emit_instr_sub(builder, int_op->type, index_reg, zero, arg);

                int_op->reg = index_reg;
            }

            ptr_op->addr.scale = base_size;
            ptr_op->addr.index_reg = int_op->reg;
        }
    }

    *dst = *ptr_op;
}

static void IR_emit_expr_binary(IR_Builder* builder, ExprBinary* expr, IR_Operand* dst)
{
    IR_Operand left = {0};
    IR_Operand right = {0};

    switch (expr->op)
    {
        case TKN_PLUS:
        {
            IR_emit_expr(builder, expr->left, &left);
            IR_emit_expr(builder, expr->right, &right);

            bool left_is_ptr = left.type->kind == TYPE_PTR;
            bool right_is_ptr = right.type->kind == TYPE_PTR;

            if (left_is_ptr)
            {
                IR_emit_ptr_int_add(builder, dst, &left, &right, true);
            }
            else if (right_is_ptr)
            {
                IR_emit_ptr_int_add(builder, dst, &right, &left, true);
            }
            else
            {
                assert(left.type == right.type);
                assert(!(left.kind == IR_OPERAND_IMM && right.kind == IR_OPERAND_IMM));

                IR_InstrArg a = {0};
                IR_InstrArg b = {0};

                IR_arg_from_operand(builder, &a, &left);
                IR_arg_from_operand(builder, &b, &right);
                IR_next_reg_operand(builder, dst, left.type);
                IR_emit_instr_add(builder, dst->type, dst->reg, a, b);
            }
            break;
        }
        case TKN_MINUS:
        {
            IR_emit_expr(builder, expr->left, &left);
            IR_emit_expr(builder, expr->right, &right);

            bool left_is_ptr = left.type->kind == TYPE_PTR;
            bool right_is_ptr = right.type->kind == TYPE_PTR;

            // ptr - int => ptr
            if (left_is_ptr && !right_is_ptr)
            {
                IR_emit_ptr_int_add(builder, dst, &left, &right, false);
            }
            // ptr - ptr => s64
            else if (left_is_ptr && right_is_ptr)
            {
                Type* result_type = expr->super.type;
                u64 base_size = left.type->as_ptr.base->size;
                u32 base_size_log2 = (u32)clp2(base_size);

                IR_InstrArg a = {0};
                IR_InstrArg b = {0};

                IR_arg_from_operand(builder, &a, &left);
                IR_arg_from_operand(builder, &b, &right);
                IR_next_reg_operand(builder, dst, result_type);
                IR_emit_instr_sub(builder, result_type, dst->reg, a, b);

                if (base_size_log2)
                {
                    IR_InstrArg arg1 = {.kind = IR_ARG_REG, .reg = dst->reg};
                    IR_InstrArg shift_amount = {.kind = IR_ARG_IMM, .imm.as_int._u32 = base_size_log2};
                    IR_Reg new_dst_reg = IR_next_reg(builder);

                    IR_emit_instr_shr(builder, result_type, new_dst_reg, arg1, shift_amount);

                    dst->reg = new_dst_reg;
                }
            }
            // int - int => int
            else
            {
                assert(left.type == right.type);
                assert(!(left.kind == IR_OPERAND_IMM && right.kind == IR_OPERAND_IMM));

                IR_InstrArg a = {0};
                IR_InstrArg b = {0};

                IR_arg_from_operand(builder, &a, &left);
                IR_arg_from_operand(builder, &b, &right);
                IR_next_reg_operand(builder, dst, left.type);
                IR_emit_instr_sub(builder, dst->type, dst->reg, a, b);
            }
            break;
        }
        default:
            assert(0);
            break;
    }
}

static void IR_emit_expr_unary(IR_Builder* builder, ExprUnary* expr, IR_Operand* dst)
{
    switch (expr->op)
    {
        case TKN_MINUS: // Two's compliment negation.
        {
            Type* result_type = expr->super.type;
            IR_Operand inner_op = {0};
            IR_InstrArg inner_arg = {0};
            IR_InstrArg zero = {.kind = IR_ARG_IMM, .imm.as_int._u64 = 0};

            IR_emit_expr(builder, expr->expr, &inner_op);
            IR_arg_from_operand(builder, &inner_arg, &inner_op);
            IR_next_reg_operand(builder, dst, result_type);
            IR_emit_instr_sub(builder, result_type, dst->reg, zero, inner_arg);
            break;
        }
        default:
            assert(0);
            break;
    }
}

static void IR_emit_expr(IR_Builder* builder, Expr* expr, IR_Operand* dst)
{
    if (expr->is_const)
    {
        dst->kind = IR_OPERAND_IMM;
        dst->type = expr->type;
        dst->imm = expr->const_val;

        return;
    }

    switch (expr->kind)
    {
        case CST_ExprIdent:
            IR_emit_expr_ident(builder, (ExprIdent*)expr, dst);
            break;
        case CST_ExprCall:
            // IR_emit_expr_call(builder, (ExprCall*)expr, dst);
            break;
        case CST_ExprCast:
            // IR_emit_expr_cast(builder, (ExprCast*)expr, dst);
            break;
        case CST_ExprBinary:
            IR_emit_expr_binary(builder, (ExprBinary*)expr, dst);
            break;
        case CST_ExprUnary:
            IR_emit_expr_unary(builder, (ExprUnary*)expr, dst);
            break;
        case CST_ExprIndex:
            // IR_emit_expr_index(builder, (ExprIndex*)expr, dst);
            break;
        default:
            ftprint_err("Unsupported expr kind %d during code generation\n", expr->kind);
            assert(0);
            break;
    }
}

// Forward declare
static void IR_emit_stmt(IR_Builder* builder, Stmt* stmt);

static void IR_push_scope(IR_Builder* builder, Scope* scope)
{
    builder->curr_scope = scope;
}

static void IR_pop_scope(IR_Builder* builder)
{
    builder->curr_scope = builder->curr_scope->parent;
}

static void IR_emit_assign(IR_Builder* builder, IR_Operand* lhs, IR_Operand* rhs)
{
    IR_SIBDAddr lhs_addr = {0};

    switch (lhs->kind)
    {
        case IR_OPERAND_VAR:
        {
            lhs_addr = IR_get_var_addr(builder, lhs->sym);
            break;
        }
        case IR_OPERAND_DEREF_ADDR:
        {
            lhs_addr = lhs->addr;
            break;
        }
        default:
            assert(0);
            break;
    }

    IR_InstrArg rhs_arg = {0};
    IR_arg_from_operand(builder, &rhs_arg, rhs);
    IR_emit_instr_store(builder, lhs->sym->type, lhs_addr, rhs_arg);
}

static void IR_emit_stmt_block_body(IR_Builder* builder, List* stmts)
{
    List* head = stmts;
    List* it = head->next;

    while (it != head)
    {
        Stmt* s = list_entry(it, Stmt, lnode);

        IR_emit_stmt(builder, s);

        it = it->next;
    }
}

static void IR_emit_stmt_block(IR_Builder* builder, StmtBlock* sblock)
{
    IR_push_scope(builder, sblock->scope);
    IR_emit_stmt_block_body(builder, &sblock->stmts);
    IR_pop_scope(builder);
}

static void IR_emit_stmt_return(IR_Builder* builder, StmtReturn* sret)
{
    IR_Operand expr_op = {0};
    IR_emit_expr(builder, sret->expr, &expr_op);

    IR_InstrArg arg = {0};
    IR_arg_from_operand(builder, &arg, &expr_op);

    // TODO: This currently assumes that return value fits in a register!
    IR_emit_instr_ret(builder, expr_op.type, arg);
}

static void IR_emit_stmt_expr(IR_Builder* builder, StmtExpr* sexpr)
{
    IR_Operand expr_op = {0};
    IR_emit_expr(builder, sexpr->expr, &expr_op);
    IR_commit_indirections(builder, &expr_op);
}

static void IR_emit_stmt_expr_assign(IR_Builder* builder, StmtExprAssign* stmt)
{
    switch (stmt->op_assign)
    {
        case TKN_ASSIGN:
        {
            IR_Operand lhs_op = {0};
            IR_Operand rhs_op = {0};

            IR_emit_expr(builder, stmt->left, &lhs_op);
            IR_emit_expr(builder, stmt->right, &rhs_op);

            IR_emit_assign(builder, &lhs_op, &rhs_op);

            break;
        }
        default:
            assert(!"Unsupported assignment operator in IR generation");
            break;
    }
}

static void IR_emit_stmt_decl(IR_Builder* builder, StmtDecl* sdecl)
{
    assert(sdecl->decl->kind == CST_DeclVar);

    DeclVar* dvar = (DeclVar*)sdecl->decl;
    Symbol* sym = lookup_symbol(builder->curr_scope, dvar->name);
    IR_get_var_addr(builder, sym); // NOTE: Call will preemptively
                                   // store the variable's address in a register.

    if (dvar->init)
    {
        IR_Operand rhs_op = {0};
        IR_Operand lhs_op = {0};

        IR_emit_expr(builder, dvar->init, &rhs_op);
        IR_operand_from_sym(&lhs_op, sym);
        IR_emit_assign(builder, &lhs_op, &rhs_op);
    }
}

static void IR_emit_stmt_if(IR_Builder* builder, StmtIf* stmt)
{
    Expr* cond_expr = stmt->if_blk.cond;
    Stmt* if_body = stmt->if_blk.body;
    Stmt* else_body = stmt->else_blk.body;

    // If expr is a compile-time constant, do not generate the unneeded branch!!
    if (cond_expr->is_const)
    {
        bool cond_val = cond_expr->const_val.as_int._u64 != 0;

        if (cond_val)
            IR_emit_stmt(builder, if_body);
        else
            IR_emit_stmt(builder, else_body);
    }
    else
    {
        IR_Operand cond_op = {0};
        IR_InstrArg cond_arg = {0};
        IR_InstrArg zero_arg = {.kind = IR_ARG_IMM, .imm.as_int._u64 = 0};

        IR_emit_expr(builder, cond_expr, &cond_op);
        IR_arg_from_operand(builder, &cond_arg, &cond_op);

        // Compare condition expression to zero and store result in a register.
        IR_Reg cmp_result_reg = IR_next_reg(builder);
        IR_emit_instr_cmp(builder, IR_COND_EQ, cond_op.type, cmp_result_reg, cond_arg, zero_arg);

        // Emit conditional jump without a jump target. The jump target will be filled in below.
        IR_Instr* cjmp_instr = IR_emit_instr_cjmp(builder, cmp_result_reg, 0);

        // Emit instructions for if-block body.
        IR_emit_stmt(builder, if_body);

        if (else_body)
        {
            // Code path from if-block needs to jump to the end.
            // The jump target is patched below.
            IR_Instr* jmp_instr = IR_emit_instr_jmp(builder, 0);

            // Patch conditional jmp instruction to jump here if the condition is false.
            IR_patch_jmp_instr(cjmp_instr, IR_get_jmp_target(builder));

            // Emit instructions for else-block body.
            IR_emit_stmt(builder, else_body);

            // Patch jmp instruction to jump to the end of the else-block.
            IR_patch_jmp_instr(jmp_instr, IR_get_jmp_target(builder));
        }
        else
        {
            IR_patch_jmp_instr(cjmp_instr, IR_get_jmp_target(builder));
        }
    }
}

static void IR_emit_stmt_while(IR_Builder* builder, StmtWhile* stmt)
{
    Expr* cond_expr = stmt->cond;
    Stmt* body = stmt->body;

    if (cond_expr->is_const)
    {
        bool cond_val = cond_expr->const_val.as_int._u64 != 0;

        // Emit infinite loop
        if (cond_val)
        {
            u32 loop_top = IR_get_jmp_target(builder);

            // Emit loop body statements.
            IR_emit_stmt(builder, body);

            // Jump back to the top of the loop.
            IR_emit_instr_jmp(builder, loop_top);
        }
    }
    else
    {
        // Emit jmp instruction to the loop's condition check. Target adddress
        // will be patched.
        IR_Instr* jmp_instr = IR_emit_instr_jmp(builder, 0);

        // Save the current instruction index to enable jumps to the top of the loop.
        u32 loop_top = IR_get_jmp_target(builder);

        // Emit instructions for the loop body.
        IR_emit_stmt(builder, body);

        // Patch initial jmp instruction with the location of the condition check.
        u32 loop_cond_check = IR_get_jmp_target(builder);
        IR_patch_jmp_instr(jmp_instr, loop_cond_check);

        IR_Operand cond_op = {0};
        IR_InstrArg cond_arg = {0};
        IR_InstrArg zero_arg = {.kind = IR_ARG_IMM, .imm.as_int._u64 = 0};

        // Emit condition expression.
        IR_emit_expr(builder, cond_expr, &cond_op);
        IR_arg_from_operand(builder, &cond_arg, &cond_op);

        // Compare condition expression to zero and store result in a register.
        IR_Reg cmp_result_reg = IR_next_reg(builder);
        IR_emit_instr_cmp(builder, IR_COND_NEQ, cond_op.type, cmp_result_reg, cond_arg, zero_arg);

        // Emit conditional jump to the top of the loop.
        IR_emit_instr_cjmp(builder, cmp_result_reg, loop_top);
    }
}

static void IR_emit_stmt_do_while(IR_Builder* builder, StmtDoWhile* stmt)
{
    Expr* cond_expr = stmt->cond;
    Stmt* body = stmt->body;

    if (cond_expr->is_const)
    {
        bool cond_val = cond_expr->const_val.as_int._u64 != 0;

        // Emit infinite loop
        if (cond_val)
        {
            u32 loop_top = IR_get_jmp_target(builder);

            // Emit loop body statements.
            IR_emit_stmt(builder, body);

            // Jump back to the top of the loop.
            IR_emit_instr_jmp(builder, loop_top);
        }
    }
    else
    {
        // Save the current instruction index to enable jumps to the top of the loop.
        u32 loop_top = IR_get_jmp_target(builder);

        // Emit instructions for the loop body.
        IR_emit_stmt(builder, body);

        IR_Operand cond_op = {0};
        IR_InstrArg cond_arg = {0};
        IR_InstrArg zero_arg = {.kind = IR_ARG_IMM, .imm.as_int._u64 = 0};

        // Emit condition expression.
        IR_emit_expr(builder, cond_expr, &cond_op);
        IR_arg_from_operand(builder, &cond_arg, &cond_op);

        // Compare condition expression to zero and store result in a register.
        IR_Reg cmp_result_reg = IR_next_reg(builder);
        IR_emit_instr_cmp(builder, IR_COND_NEQ, cond_op.type, cmp_result_reg, cond_arg, zero_arg);

        // Emit conditional jump to the top of the loop.
        IR_emit_instr_cjmp(builder, cmp_result_reg, loop_top);
    }
}

static void IR_emit_stmt(IR_Builder* builder, Stmt* stmt)
{
    switch (stmt->kind)
    {
        case CST_StmtBlock:
            IR_emit_stmt_block(builder, (StmtBlock*)stmt);
            break;
        case CST_StmtReturn:
            IR_emit_stmt_return(builder, (StmtReturn*)stmt);
            break;
        case CST_StmtDecl:
            IR_emit_stmt_decl(builder, (StmtDecl*)stmt);
            break;
        case CST_StmtExpr:
            IR_emit_stmt_expr(builder, (StmtExpr*)stmt);
            break;
        case CST_StmtExprAssign:
            IR_emit_stmt_expr_assign(builder, (StmtExprAssign*)stmt);
            break;
        case CST_StmtIf:
            IR_emit_stmt_if(builder, (StmtIf*)stmt);
            break;
        case CST_StmtWhile:
            IR_emit_stmt_while(builder, (StmtWhile*)stmt);
            break;
        case CST_StmtDoWhile:
            IR_emit_stmt_do_while(builder, (StmtDoWhile*)stmt);
            break;
        default:
            break;
    }
}

static u32 IR_assign_scope_var_offsets(Symbol*** proc_vars, Scope* scope, u32 offset)
{
    u32 stack_size = offset;

    //
    // Sum sizes of local variables declared in this scope.
    //
    {
        List* head = &scope->sym_list;
        List* it = head->next;

        while (it != head)
        {
            Symbol* sym = list_entry(it, Symbol, lnode);

            if (sym->kind == SYMBOL_VAR)
            {
                Type* var_type = sym->type;
                size_t var_size = var_type->size;
                size_t var_align = var_type->align;

                stack_size += var_size;
                stack_size = ALIGN_UP(stack_size, var_align);
                sym->as_var.offset = -stack_size;

                array_push(*proc_vars, sym);
            }

            it = it->next;
        }
    }

    //
    // Recursively compute stack sizes for child scopes. Take the largest.
    //
    {
        List* head = &scope->children;
        List* it = head->next;
        size_t child_offset = stack_size;

        while (it != head)
        {
            Scope* child_scope = list_entry(it, Scope, lnode);
            u32 child_size = IR_assign_scope_var_offsets(proc_vars, child_scope, child_offset);

            if (child_size > stack_size)
                stack_size = child_size;

            it = it->next;
        }
    }

    return ALIGN_UP(stack_size, IR_STACK_ALIGN);
}

static void IR_assign_proc_var_offsets(IR_Builder* builder, Symbol* proc_sym)
{
    u32 stack_size = 0;
    DeclProc* dproc = (DeclProc*)proc_sym->decl;
    Scope* scope = dproc->scope;

    // Save allocator state so that the temporary arena's state can
    // be restored after building the temporary args array.
    AllocatorState arena_state = allocator_get_state(builder->tmp_arena);
    {
        // Create temporary array that will hold all args and local variables that belong to
        // this procedure. This includes variables declared in nested scopes.
        Symbol** proc_vars = array_create(builder->tmp_arena, Symbol*, dproc->num_params << 1);

        // Recursively assign a stack offset to each local variable (including proc arguments).
        // Variables declared in sibling scopes will be assigned to overlapping stack offsets
        // to use "less" stack space.
        stack_size = IR_assign_scope_var_offsets(&proc_vars, scope, stack_size);

        // Save the computed stack size and the vars array into the procedure symbol.
        u32 num_vars = (u32)array_len(proc_vars);

        proc_sym->as_proc.min_stack_size = ALIGN_UP(stack_size, IR_STACK_ALIGN);
        proc_sym->as_proc.num_vars = num_vars;
        proc_sym->as_proc.vars = alloc_array(builder->arena, Symbol*, num_vars, false);

        memcpy(proc_sym->as_proc.vars, proc_vars, num_vars * sizeof(Symbol*));
    }
    allocator_restore_state(arena_state);
}

static bool IR_build_proc(IR_Builder* builder, Symbol* sym)
{
    DeclProc* dproc = (DeclProc*)sym->decl;

    // Set procedure as the current scope.
    IR_push_scope(builder, dproc->scope);
    builder->curr_proc = sym;

    // Assign stack offsets to params and local vars.
    IR_assign_proc_var_offsets(builder, sym);

    sym->as_proc.instrs = array_create(builder->arena, IR_Instr*, 32);
    IR_emit_stmt_block_body(builder, &dproc->stmts);

    IR_pop_scope(builder);
    builder->curr_proc = NULL;

#ifndef NDEBUG
    IR_print_out_proc(builder->tmp_arena, sym);
#endif

    return true;
}

IR_Module* IR_build_module(Allocator* arena, Allocator* tmp_arena, Scope* global_scope)
{
    IR_Module* module = alloc_type(arena, IR_Module, true);

    if (!module)
        return NULL;

    IR_Builder builder = {
        .arena = arena, .tmp_arena = tmp_arena, .curr_proc = NULL, .curr_scope = global_scope, .module = module};

    // Create global IR vars/procs arrays.
    module->num_vars = global_scope->sym_kind_counts[SYMBOL_VAR];
    module->vars = alloc_array(arena, Symbol*, module->num_vars, false);

    module->num_procs = global_scope->sym_kind_counts[SYMBOL_PROC];
    module->procs = alloc_array(arena, Symbol*, module->num_procs, false);

    // Iterate through all global declarations and create IR structures for
    // global variables and procedures.
    size_t var_index = 0;
    size_t proc_index = 0;

    List* head = &global_scope->sym_list;
    List* it = head->next;

    while (it != head)
    {
        Symbol* sym = list_entry(it, Symbol, lnode);

        if (sym->kind == SYMBOL_VAR)
        {
            module->vars[var_index] = sym;
            var_index += 1;
        }
        else if (sym->kind == SYMBOL_PROC)
        {
            module->procs[proc_index] = sym;
            proc_index += 1;
        }

        it = it->next;
    }

    assert(var_index == module->num_vars);
    assert(proc_index == module->num_procs);

    // Iterate through all procedures and generate IR instructions.
    for (size_t i = 0; i < module->num_procs; i += 1)
    {
        IR_build_proc(&builder, module->procs[i]);
    }

    return module;
}
