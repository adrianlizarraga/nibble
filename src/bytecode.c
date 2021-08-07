#include "bytecode.h"
#include "ast.h"
#include "stream.h"

//////////////////////////////////////////////////////
//
//         Create IR instructions
//
//////////////////////////////////////////////////////
static IR_Type ir_int_types[] = {
    [INTEGER_U8] = IR_TYPE_INT8,
    [INTEGER_S8] = IR_TYPE_INT8,
    [INTEGER_U16] = IR_TYPE_INT16,
    [INTEGER_S16] = IR_TYPE_INT16,
    [INTEGER_U32] = IR_TYPE_INT32,
    [INTEGER_S32] = IR_TYPE_INT32,
    [INTEGER_U64] = IR_TYPE_INT64,
    [INTEGER_S64] = IR_TYPE_INT64,
};

static IR_Type ir_float_types[] = {
    [FLOAT_F64] = IR_TYPE_F64,
    [FLOAT_F32] = IR_TYPE_F32,
};

static IR_Type IR_get_type(Type* type)
{
    IR_Type ir_type = IR_TYPE_VOID;

    switch (type->kind)
    {
        case TYPE_INTEGER:
            ir_type = ir_int_types[type->as_integer.kind];
            break;
        case TYPE_PTR:
            ir_type = IR_TYPE_PTR;
            break;
        case TYPE_FLOAT:
            ir_type = ir_float_types[type->as_float.kind];
            break;
        default:
            assert(0);
            break;
    }

    return ir_type;
}

IR_Instr** IR_get_instr(IR_Builder* builder, size_t index)
{
    return (IR_Instr**)bucket_list_get_elem(builder->curr_proc->as_proc.instrs, index);
}

IR_Instr** IR_add_instr(IR_Builder* builder, IR_Instr* instr)
{
    return (IR_Instr**)bucket_list_add_elem(builder->curr_proc->as_proc.instrs, instr);
}

IR_Instr* IR_new_instr(Allocator* arena, IR_InstrKind kind)
{
    IR_Instr* instr = alloc_type(arena, IR_Instr, true);
    instr->kind = kind;

    return instr;
}

void IR_emit_instr_add(IR_Builder* builder, IR_Type type, IR_Reg dst_reg, IR_InstrArg a, IR_InstrArg b)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_ADD); 
    instr->option.bytes[0] = type;
    instr->r = dst_reg;
    instr->a = a;
    instr->b = b;

    IR_add_instr(builder, instr);
}

void IR_emit_instr_sub(IR_Builder* builder, IR_Type type, IR_Reg dst_reg, IR_InstrArg a, IR_InstrArg b)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_SUB); 
    instr->option.bytes[0] = type;
    instr->r = dst_reg;
    instr->a = a;
    instr->b = b;

    IR_add_instr(builder, instr);
}

void IR_emit_instr_neg(IR_Builder* builder, IR_Type type, IR_Reg dst_reg, IR_InstrArg src)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_NEG); 
    instr->option.bytes[0] = type;
    instr->r = dst_reg;
    instr->a = src;

    IR_add_instr(builder, instr);
}

void IR_emit_instr_load(IR_Builder* builder, IR_Type type, IR_Reg dst_reg, IR_SIBDAddr addr)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_LOAD);
    instr->r = dst_reg;
    instr->a.reg0 = addr.base_reg;
    instr->a.reg1 = addr.index_reg;
    instr->option.bytes[0] = type;
    instr->option.bytes[1] = addr.scale;
    instr->b.imm.as_int._u64 = addr.disp;

    IR_add_instr(builder, instr);
}

void IR_emit_instr_laddr(IR_Builder* builder, IR_Reg dst_reg, IR_SIBDAddr addr)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_LADDR);
    instr->r = dst_reg;
    instr->a.kind = IR_ARG_REG;
    instr->a.reg0 = addr.base_reg;
    instr->a.reg1 = addr.index_reg;
    instr->option.bytes[0] = IR_TYPE_PTR;
    instr->option.bytes[1] = addr.scale;
    instr->b.kind = IR_ARG_IMM;
    instr->b.imm.as_int._u64 = addr.disp;

    IR_add_instr(builder, instr);
}

void IR_emit_instr_laddr_var(IR_Builder* builder, IR_Reg dst_reg, Symbol* sym)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_LADDR_VAR);
    instr->r = dst_reg;
    instr->a.kind = IR_ARG_IMM;
    instr->a.imm.as_ptr = sym;

    IR_add_instr(builder, instr);
}

void IR_emit_instr_shr(IR_Builder* builder, IR_Type type, IR_Reg dst_reg, IR_Reg src_reg, u8 shift_bits)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_SHR);
    instr->option.bytes[0] = type;
    instr->r = dst_reg;
    instr->a.kind = IR_ARG_REG;
    instr->a.reg0 = src_reg;
    instr->b.kind = IR_ARG_IMM;
    instr->b.imm.as_int._u8 = shift_bits;

    IR_add_instr(builder, instr);
}

void IR_emit_instr_ret(IR_Builder* builder, IR_Type type, IR_InstrArg ret_arg)
{
    IR_Instr* instr = IR_new_instr(builder->arena, IR_INSTR_RET);
    instr->option.bytes[0] = type;
    instr->a = ret_arg;

    IR_add_instr(builder, instr);
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

    IR_Reg dst_reg = IR_next_reg(builder);

    IR_emit_instr_load(builder, IR_get_type(operand->type), dst_reg,  operand->addr);

    operand->kind = IR_OPERAND_REG;
    operand->reg = dst_reg;
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

        IR_emit_instr_laddr(builder, dst_reg, addr);
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
                IR_emit_instr_load(builder, IR_get_type(operand->type), base_reg, var_addr);

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
            IR_emit_instr_load(builder, IR_get_type(operand->type), reg, var_addr);

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

void IR_arg_from_operand(IR_Builder* builder, IR_InstrArg* arg, IR_Operand* op)
{
    switch (op->kind)
    {
        case IR_OPERAND_IMM:
            arg->kind = IR_ARG_IMM;
            arg->imm = op->imm;
            break;
        case IR_OPERAND_REG:
            arg->kind = IR_ARG_REG;
            arg->reg0 = op->reg;
            arg->reg1 = 0;
            break;
        case IR_OPERAND_VAR:
        {
            // Assume sym->as_var.reg is a register that contains the result from an
            // alloca instruction (i.e., contains an address to the var).
            IR_SIBDAddr addr = {.base_reg = op->sym->as_var.reg};
            IR_Reg result_reg = IR_next_reg(builder);
            IR_emit_instr_load(builder, IR_get_type(op->type), result_reg, addr);

            op->kind = IR_OPERAND_REG;
            op->reg = result_reg;

            arg->kind = IR_ARG_REG;
            arg->reg0 = result_reg;
            arg->reg1 = 0;
            break;
        }
        case IR_OPERAND_DEREF_ADDR:
            IR_execute_deref(builder, op);

            arg->kind = IR_ARG_REG;
            arg->reg0 = op->reg;
            arg->reg1 = 0;
            break;
        default:
            assert(!0);
            break;
    }
}

void IR_next_reg_operand(IR_Builder* builder, IR_Operand* dst, Type* type)
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
void IR_emit_expr(IR_Builder* builder, Expr* expr, IR_Operand* dst);

void IR_emit_expr_ident(IR_Builder* builder, ExprIdent* eident, IR_Operand* dst)
{
    Symbol* sym = lookup_symbol(builder->curr_scope, eident->name);

    if (sym->kind == SYMBOL_VAR)
    {
        dst->kind = IR_OPERAND_VAR;
        dst->type = sym->type;
        dst->sym = sym;
    }
    else if (sym->kind == SYMBOL_PROC)
    {
        dst->kind = IR_OPERAND_PROC;
        dst->type = sym->type;
        dst->sym = sym;
    }
    else
    {
        ftprint_err("INTERNAL ERROR: Unexpected symbol kind %d during code generation for ident expr\n", sym->kind);
        assert(0);
    }
}

void IR_emit_ptr_int_add(IR_Builder* builder, IR_Operand* dst, IR_Operand* ptr_op, IR_Operand* int_op, bool add)
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
            IR_InstrArg a = {.kind = IR_ARG_REG, .reg0 = ptr_op->addr.index_reg};
            IR_InstrArg b = {0};

            IR_arg_from_operand(builder, &b, int_op);

            if (add)
                IR_emit_instr_add(builder, IR_TYPE_INT64, index_reg, a, b);
            else
                IR_emit_instr_sub(builder, IR_TYPE_INT64, index_reg, a, b);

            ptr_op->addr.index_reg = index_reg;
        }
        else
        {
            IR_ensure_operand_in_reg(builder, int_op, true);
            
            if (!add)
            {
                IR_Reg index_reg = IR_next_reg(builder);
                IR_Type type = IR_get_type(int_op->type);
                IR_InstrArg a = {.kind = IR_ARG_REG, .reg0 = int_op->reg};

                IR_emit_instr_neg(builder, type, index_reg, a);

                int_op->reg = index_reg;
            }

            ptr_op->addr.scale = base_size;
            ptr_op->addr.index_reg = int_op->reg;
        }
    }

    *dst = *ptr_op;
}

void IR_emit_expr_binary(IR_Builder* builder, ExprBinary* expr, IR_Operand* dst)
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
                IR_emit_instr_add(builder, IR_get_type(dst->type), dst->reg, a, b);
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
                IR_Type result_ir_type = IR_get_type(result_type);
                u64 base_size = left.type->as_ptr.base->size;
                u32 base_size_log2 = (u32)clp2(base_size);

                IR_InstrArg a = {0};
                IR_InstrArg b = {0};

                IR_arg_from_operand(builder, &a, &left);
                IR_arg_from_operand(builder, &b, &right);
                IR_next_reg_operand(builder, dst, result_type);
                IR_emit_instr_sub(builder, result_ir_type, dst->reg, a, b);

                if (base_size_log2)
                {
                    IR_Reg dst_reg = IR_next_reg(builder);
                    IR_emit_instr_shr(builder, result_ir_type, dst_reg, dst->reg, (u8)base_size_log2);

                    dst->reg = dst_reg;
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
                IR_emit_instr_sub(builder, IR_get_type(dst->type), dst->reg, a, b);
            }
            break;
        }
        default:
            assert(0);
            break;
    }
}

void IR_emit_expr_unary(IR_Builder* builder, ExprUnary* expr, IR_Operand* dst)
{
    switch (expr->op)
    {
        case TKN_MINUS: // Two's compliment negation.
        {
            Type* result_type = expr->super.type;
            IR_Operand inner_op = {0};
            IR_InstrArg inner_arg = {0};

            IR_emit_expr(builder, expr->expr, &inner_op);
            IR_arg_from_operand(builder, &inner_arg, &inner_op);
            IR_next_reg_operand(builder, dst, result_type);
            IR_emit_instr_neg(builder, IR_get_type(result_type), dst->reg, inner_arg);
            break;
        }
        default:
            assert(0);
            break;
    }
}

void IR_emit_expr(IR_Builder* builder, Expr* expr, IR_Operand* dst)
{
    if (expr->is_const)
    {
        dst->kind = IR_OPERAND_IMM;
        dst->type = expr->type;
        dst->imm = expr->const_val;
    }

    switch (expr->kind)
    {
        case CST_ExprIdent:
            IR_emit_expr_ident(builder, (ExprIdent*)expr, dst);
            break;
        case CST_ExprCall:
            //IR_emit_expr_call(builder, (ExprCall*)expr, dst);
            break;
        case CST_ExprCast:
            //IR_emit_expr_cast(builder, (ExprCast*)expr, dst);
            break;
        case CST_ExprBinary:
            IR_emit_expr_binary(builder, (ExprBinary*)expr, dst);
            break;
        case CST_ExprUnary:
            IR_emit_expr_unary(builder, (ExprUnary*)expr, dst);
            break;
        case CST_ExprIndex:
            //IR_emit_expr_index(builder, (ExprIndex*)expr, dst);
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

static void IR_emit_block(builder, StmtBlock* sblock)
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
    IR_emit_instr_ret(builder, IR_get_type(expr_op.type), arg);
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
            IR_Operand loperand = {0};
            IR_Operand roperand = {0};

            IR_emit_expr(builder, stmt->left, &loperand);
            IR_emit_expr(builder, stmt->right, &roperand);

            IR_emit_assign(&lopernad, &roperand);
        }
        default:
            assert(!"Unsupported assignment operator in IR generation");
            break;
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

static u32 IR_assign_scope_var_offsets(Symbol*** proc_vars, Scope* scope, u32 offset)
{
    u32 stack_size = 0;

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
        Symbol** proc_vars = array_create(builder->tmp_arena, Symbol*, num_params << 1);

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

bool IR_build_proc(IR_Builder* builder, Symbol* sym)
{
    DeclProc* dproc = (DeclProc*)sym->decl;

    // Set procedure as the current scope.
    IR_push_scope(builder, dproc->scope);
    builder->curr_proc = sym;

    // Assign stack offsets to params and local vars.
    IR_assign_proc_var_offsets(sym);

    IR_emit_stmt_block_body(builder, &dproc->stmts);

    IR_pop_scope(builder);
    builder->curr_proc = NULL;
    
    return true;
}

IR_Module* IR_build_module(Allocator* arena, Allocator* tmp_arena, Scope* global_scope)
{
    IR_Module* module = alloc_type(arena, IR_Module, true);

    if (!module)
        return NULL;

    IR_Builder builder = {
        .arena = arena,
        .tmp_arena = .tmp_arena,
        .curr_proc = NULL,
        .curr_scope = global_scope,
        .module = module
    };

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
