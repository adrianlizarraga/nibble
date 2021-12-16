#include "ir.h"

#define NIR_REG_COUNT 0xFFFFFFFF

typedef struct NIR_ProcBuilder {
    Allocator* arena;
    Allocator* tmp_arena;
    TypeCache* type_cache;
    Symbol* curr_proc;
    Scope* curr_scope;

    bool next_instr_is_jmp_target;
} NIR_ProcBuilder;

typedef enum NIR_OperandKind {
    NIR_OPERAND_NONE,
    NIR_OPERAND_IMM,
    NIR_OPERAND_REG,
    NIR_OPERAND_MEM_ADDR,
    NIR_OPERAND_DEREF_ADDR,
    NIR_OPERAND_DEFERRED_CMP,
    NIR_OPERAND_ARRAY_INIT,
    NIR_OPERAND_VAR,
    NIR_OPERAND_STR_LIT,
    NIR_OPERAND_PROC,
} NIR_OperandKind;

typedef struct NIR_DeferredJmpcc {
    bool result;
    Instr* jmp; // target needs to be patched.
    Instr* cmp; // Condition may need to be inverted.
    struct NIR_DeferredJmpcc* next;
} NIR_DeferredJmpcc;

typedef struct NIR_DeferredCmp {
    NIR_DeferredJmpcc* first_sc_jmp;
    NIR_DeferredJmpcc* last_sc_jmp;
    NIR_DeferredJmpcc final_jmp;
} NIR_DeferredCmp;

typedef struct NIR_ArrayMemberInitializer NIR_ArrayMemberInitializer;

typedef struct NIR_ArrayInitializer {
    u64 num_initzers;
    NIR_ArrayMemberInitializer* initzers;
} NIR_ArrayInitializer;

typedef struct NIR_Operand {
    NIR_OperandKind kind;
    Type* type;

    union {
        Scalar imm;
        NIR_Reg reg;
        MemAddr addr;
        Symbol* sym;
        NIR_DeferredCmp cmp;
        NIR_ArrayInitializer array_initzer;
        StrLit* str_lit;
    };
} NIR_Operand;

typedef struct NIR_ArrayMemberInitializer {
    u64 index;
    NIR_Operand op;
} NIR_ArrayMemberInitializer;

static NIR_Reg NIR_next_reg(NIR_ProcBuilder* builder)
{
    Symbol* sym = builder->curr_proc;
    NIR_Reg next_reg = sym->as_proc.num_regs++;

    return next_reg;
}

static void NIR_op_to_r(NIR_ProcBuilder* builder, NIR_Operand* operand, bool commit_ptr)
{
    if (commit_ptr && (operand->kind == NIR_OPERAND_MEM_ADDR)) {
        NIR_execute_lea(builder, operand);
    }
    else if (operand->kind == NIR_OPERAND_DEREF_ADDR) {
        NIR_execute_deref(builder, operand);
    }
    else if (operand->kind == NIR_OPERAND_DEFERRED_CMP) {
        //NIR_execute_deferred_cmp(builder, operand);
        assert(0);
    }
    else if (operand->kind != NIR_OPERAND_REG) {
        if (!commit_ptr && (operand->type->kind == TYPE_PTR)) {
            if (operand->kind != NIR_OPERAND_MEM_ADDR) {
                NIR_Reg base_reg = NIR_next_reg(builder);
                NIR_emit_instr_load_sym(builder, operand->type, base_reg, operand->sym);

                operand->kind = NIR_OPERAND_MEM_ADDR;
                operand->addr.base_kind = MEM_BASE_REG;
                operand->addr.base.reg = base_reg;
                operand->addr.index_reg = NIR_REG_COUNT;
                operand->addr.scale = 0;
                operand->addr.disp = 0;
            }
        }
        else if (operand->kind == NIR_OPERAND_IMM) {
            NIR_Reg reg = NIR_next_reg(builder);

            NIR_emit_instr_limm(builder, operand->type, reg, operand->imm);

            operand->kind = NIR_OPERAND_REG;
            operand->reg = reg;
        }
        else {
            NIR_Reg reg = NIR_next_reg(builder);

            if (operand->kind == NIR_OPERAND_PROC) {
                NIR_emit_instr_laddr_sym(builder, reg, operand->type, operand->sym);
            }
            else {
                NIR_emit_instr_load_sym(builder, operand->type, reg, operand->sym);
            }

            operand->kind = NIR_OPERAND_REG;
            operand->reg = reg;
        }
    }
}

static void NIR_push_scope(NIR_ProcBuilder* builder, Scope* scope)
{
    builder->curr_scope = scope;
}

static void NIR_pop_scope(NIR_ProcBuilder* builder)
{
    builder->curr_scope = builder->curr_scope->parent;
}

static void NIR_emit_stmt(NIR_ProcBuilder* builder, Stmt* stmt, u32* break_target, u32* continue_target);

static void NIR_emit_stmt_block_body(NIR_ProcBuilder* builder, List* stmts, u32* break_target, u32* continue_target)
{
    for (List* it = stmts->next; it != stmts; it = it->next) {
        Stmt* s = list_entry(it, Stmt, lnode);
        NIR_emit_stmt(builder, s, break_target, continue_target);
    }
}

static void NIR_emit_stmt_block(NIR_ProcBuilder* builder, StmtBlock* sblock, u32* break_target, u32* continue_target)
{
    IR_push_scope(builder, sblock->scope);
    IR_emit_stmt_block_body(builder, &sblock->stmts, break_target, continue_target);
    IR_pop_scope(builder);
}

static void NIR_emit_stmt_return(NIR_ProcBuilder* builder, StmtReturn* sret)
{
    NIR_Operand expr_op = {0};
    NIR_emit_expr(builder, sret->expr, &expr_op);
    NIR_op_to_r(builder, &expr_op, true);

    NIR_emit_instr_ret(builder, expr_op.type, expr_op.reg);
}

static void NIR_emit_stmt(NIR_ProcBuilder* builder, Stmt* stmt, u32* break_target, u32* continue_target)
{
    switch (stmt->kind) {
    case CST_StmtBlock:
        NIR_emit_stmt_block(builder, (StmtBlock*)stmt, break_target, continue_target);
        break;
    case CST_StmtReturn:
        NIR_emit_stmt_return(builder, (StmtReturn*)stmt);
        break;
    case CST_StmtDecl:
        NIR_emit_stmt_decl(builder, (StmtDecl*)stmt);
        break;
    case CST_StmtExpr:
        NIR_emit_stmt_expr(builder, (StmtExpr*)stmt);
        break;
    case CST_StmtExprAssign:
        NIR_emit_stmt_expr_assign(builder, (StmtExprAssign*)stmt);
        break;
    case CST_StmtIf:
        NIR_emit_stmt_if(builder, (StmtIf*)stmt, break_target, continue_target);
        break;
    case CST_StmtWhile:
        NIR_emit_stmt_while(builder, (StmtWhile*)stmt);
        break;
    case CST_StmtDoWhile:
        NIR_emit_stmt_do_while(builder, (StmtDoWhile*)stmt);
        break;
    case CST_StmtBreak:
        NIR_emit_instr_jmp(builder, break_target);
        break;
    case CST_StmtContinue:
        NIR_emit_instr_jmp(builder, continue_target);
        break;
    case CST_StmtStaticAssert:
        // Do nothing.
        break;
    default:
        ftprint_err("Cannot emit bytecode instruction for statement kind `%d`\n", stmt->kind);
        assert(0);
        break;
    }
}

static void NIR_build_proc(NIR_ProcBuilder* builder, Symbol* sym)
{
    DeclProc* dproc = (DeclProc*)sym->decl;

    if (dproc->is_incomplete) {
        return;
    }

    // Set procedure as the current scope.
    NIR_push_scope(builder, dproc->scope);
    builder->curr_proc = sym;

    sym->as_proc.instrs = array_create(builder->arena, IR_Instr*, 32);
    sym->as_proc.reg_intervals = array_create(builder->arena, LifetimeInterval, 16);

    NIR_emit_stmt_block_body(builder, &dproc->stmts, NULL, NULL);
    assert(builder->free_regs == (u32)-1);

    // If proc doesn't have explicit returns, add one at the end.
    // NOTE: This should only apply to procs that return void. The resolver
    // will catch other cases.
    if (!dproc->returns) {
        assert(sym->type->as_proc.ret == builtin_types[BUILTIN_TYPE_VOID].type);
        NIR_emit_instr_ret(builder, builtin_types[BUILTIN_TYPE_VOID].type, IR_REG_COUNT);
    }

    NIR_pop_scope(builder);
    builder->curr_proc = NULL;
}

static void NIR_build_procs(Allocator* arena, Allocator* tmp_arena, BucketList* procs, TypeCache* type_cache)
{
    NIR_ProcBuilder builder =
        {.arena = arena, .tmp_arena = tmp_arena, .type_cache = type_cache, .curr_proc = NULL, .curr_scope = NULL, .free_regs = -1};

    AllocatorState tmp_mem_state = allocator_get_state(builder.tmp_arena);

    // Iterate through all procedures and generate IR instructions.
    size_t num_procs = procs->num_elems;

    for (size_t i = 0; i < num_procs; i += 1) {
        void** sym_ptr = bucket_list_get_elem_packed(procs, i);
        assert(sym_ptr);
        Symbol* sym = (Symbol*)(*sym_ptr);
        assert(sym->kind == SYMBOL_PROC);

        NIR_build_proc(&builder, sym);
    }

    allocator_restore_state(tmp_mem_state);
}
