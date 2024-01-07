#include <stdio.h>
#include <stdlib.h>
#include "basics.h"
#include "allocator.h"
#include "argv_helper.h"
#include "x64_gen/x64_instrs.h"
#include "x64_gen/machine_code.h"

#include "test_utils.h"

static bool test_add_ri_inf_loop(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_add_ri(&proc0->x64_instrs, 4, X64_RDX, 10);
    X64_emit_instr_add_ri(&proc0->x64_instrs, 4, X64_RDX, 0xd0);
    X64_emit_instr_add_ri(&proc0->x64_instrs, 4, X64_RDX, -120);
    X64_emit_instr_add_ri(&proc0->x64_instrs, 4, X64_R10, 0xd0);
    X64_emit_instr_jmp(&proc0->x64_instrs, 0);

    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    const char* nasm_code = "proc0:\n"
                            " l0: add edx, 0xA\n"
                            " add edx, 0xd0\n"
                            " add edx, -120\n"
                            " add r10d, 0xd0\n"
                            " jmp l0\n";
    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_push_pop_r64(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_push(&proc0->x64_instrs, X64_RAX);
    X64_emit_instr_push(&proc0->x64_instrs, X64_R10); // Extended register uses an prefix extra byte
    X64_emit_instr_pop(&proc0->x64_instrs, X64_R10);
    X64_emit_instr_pop(&proc0->x64_instrs, X64_RAX);

    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    if (!get_nasm_machine_code(&nasm_buffer, "proc0:\npush rax\npush r10\npop r10\npop rax\n", mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_ret(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_ret(&proc0->x64_instrs);

    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    if (!get_nasm_machine_code(&nasm_buffer, "proc0:\nret\n", mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_call(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RAX, 10);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Elf_Test_Proc* proc1 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_call(&proc1->x64_instrs, &proc0->sym); // Call proc0 from proc1
    X64_emit_instr_ret(&proc1->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc1->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    X64_patch_proc_uses(elf_prog.buffer, elf_prog.proc_off_patches, elf_prog.proc_offsets);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    if (!get_nasm_machine_code(&nasm_buffer, "proc0:\nmov eax, 10\nret\n\nproc1:\ncall proc0\nret\n", mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_call_r(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RAX, 10);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Elf_Test_Proc* proc1 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_lea(&proc1->x64_instrs, X64_RAX, (X64_SIBD_Addr){.kind = X64_SIBD_ADDR_GLOBAL, .global = &proc0->sym});
    X64_emit_instr_call_r(&proc1->x64_instrs, X64_RAX); // Call proc0 from proc1 via rax
    X64_emit_instr_ret(&proc1->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc1->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    X64_patch_proc_uses(elf_prog.buffer, elf_prog.proc_off_patches, elf_prog.proc_offsets);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov eax, 10\n"
                            " ret\n\n"
                            "proc1:\n"
                            " lea rax, qword [rel proc0]\n"
                            " call rax\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_call_m(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RAX, 10);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Elf_Test_Proc* proc1 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr mem_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_emit_instr_lea(&proc1->x64_instrs, X64_RAX, (X64_SIBD_Addr){.kind = X64_SIBD_ADDR_GLOBAL, .global = &proc0->sym});
    X64_emit_instr_mov_mr(&proc1->x64_instrs, 8, mem_addr, X64_RAX);
    X64_emit_instr_call_m(&proc1->x64_instrs, mem_addr); // Call proc0 from proc1 via mem addr
    X64_emit_instr_ret(&proc1->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc1->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    X64_patch_proc_uses(elf_prog.buffer, elf_prog.proc_off_patches, elf_prog.proc_offsets);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov eax, 10\n"
                            " ret\n\n"
                            "proc1:\n"
                            " lea rax, qword [rel proc0]\n"
                            " mov qword [rsp - 8], rax\n"
                            " call qword [rsp - 8]\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_sext_ax_to_dx(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RAX, -1);
    X64_emit_instr_sext_ax_into_dx(&proc0->x64_instrs, 2);
    X64_emit_instr_sext_ax_into_dx(&proc0->x64_instrs, 4);
    X64_emit_instr_sext_ax_into_dx(&proc0->x64_instrs, 8);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov eax, -1\n"
                            " cwd\n"
                            " cdq\n"
                            " cqo\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_div_r(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RAX, 32);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RCX, 2);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_R10, 2);
    X64_emit_instr_div_r(&proc0->x64_instrs, 1, X64_RCX);
    X64_emit_instr_div_r(&proc0->x64_instrs, 2, X64_RCX);
    X64_emit_instr_div_r(&proc0->x64_instrs, 4, X64_RCX);
    X64_emit_instr_div_r(&proc0->x64_instrs, 8, X64_RCX);
    X64_emit_instr_div_r(&proc0->x64_instrs, 8, X64_R10);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov eax, 32\n"
                            " mov ecx, 2\n"
                            " mov r10d, 2\n"
                            " div cl\n"
                            " div cx\n"
                            " div ecx\n"
                            " div rcx\n"
                            " div r10\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_div_m(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};

    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RAX, 32);
    X64_emit_instr_mov_mi(&proc0->x64_instrs, 4, rsp_addr, 2);
    X64_emit_instr_mov_rr(&proc0->x64_instrs, 8, X64_R10, X64_RSP);
    X64_emit_instr_mov_mi(&proc0->x64_instrs, 4, r10_addr, 2);

    X64_emit_instr_div_m(&proc0->x64_instrs, 1, rsp_addr);
    X64_emit_instr_div_m(&proc0->x64_instrs, 2, rsp_addr);
    X64_emit_instr_div_m(&proc0->x64_instrs, 4, rsp_addr);
    X64_emit_instr_div_m(&proc0->x64_instrs, 8, rsp_addr);
    X64_emit_instr_div_m(&proc0->x64_instrs, 8, r10_addr);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov eax, 32\n"
                            " mov dword [rsp - 0x8], 2\n"
                            " mov r10, rsp\n"
                            " mov dword [r10 - 0x8], 2\n"
                            " div byte [rsp - 0x8]\n"
                            " div word [rsp - 0x8]\n"
                            " div dword [rsp - 0x8]\n"
                            " div qword [rsp - 0x8]\n"
                            " div qword [r10 - 0x8]\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_idiv_r(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RAX, 32);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RCX, 2);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_R10, 2);
    X64_emit_instr_idiv_r(&proc0->x64_instrs, 1, X64_RCX);
    X64_emit_instr_idiv_r(&proc0->x64_instrs, 2, X64_RCX);
    X64_emit_instr_idiv_r(&proc0->x64_instrs, 4, X64_RCX);
    X64_emit_instr_idiv_r(&proc0->x64_instrs, 8, X64_RCX);
    X64_emit_instr_idiv_r(&proc0->x64_instrs, 8, X64_R10);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov eax, 32\n"
                            " mov ecx, 2\n"
                            " mov r10d, 2\n"
                            " idiv cl\n"
                            " idiv cx\n"
                            " idiv ecx\n"
                            " idiv rcx\n"
                            " idiv r10\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_idiv_m(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};

    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RAX, 32);
    X64_emit_instr_mov_mi(&proc0->x64_instrs, 4, rsp_addr, 2);
    X64_emit_instr_mov_rr(&proc0->x64_instrs, 8, X64_R10, X64_RSP);
    X64_emit_instr_mov_mi(&proc0->x64_instrs, 4, r10_addr, 2);

    X64_emit_instr_idiv_m(&proc0->x64_instrs, 1, rsp_addr);
    X64_emit_instr_idiv_m(&proc0->x64_instrs, 2, rsp_addr);
    X64_emit_instr_idiv_m(&proc0->x64_instrs, 4, rsp_addr);
    X64_emit_instr_idiv_m(&proc0->x64_instrs, 8, rsp_addr);
    X64_emit_instr_idiv_m(&proc0->x64_instrs, 8, r10_addr);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov eax, 32\n"
                            " mov dword [rsp - 0x8], 2\n"
                            " mov r10, rsp\n"
                            " mov dword [r10 - 0x8], 2\n"
                            " idiv byte [rsp - 0x8]\n"
                            " idiv word [rsp - 0x8]\n"
                            " idiv dword [rsp - 0x8]\n"
                            " idiv qword [rsp - 0x8]\n"
                            " idiv qword [r10 - 0x8]\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_mul_r(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RAX, 32);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RCX, 2);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_R10, 2);
    X64_emit_instr_mul_r(&proc0->x64_instrs, 1, X64_RCX);
    X64_emit_instr_mul_r(&proc0->x64_instrs, 2, X64_RCX);
    X64_emit_instr_mul_r(&proc0->x64_instrs, 4, X64_RCX);
    X64_emit_instr_mul_r(&proc0->x64_instrs, 8, X64_RCX);
    X64_emit_instr_mul_r(&proc0->x64_instrs, 8, X64_R10);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov eax, 32\n"
                            " mov ecx, 2\n"
                            " mov r10d, 2\n"
                            " mul cl\n"
                            " mul cx\n"
                            " mul ecx\n"
                            " mul rcx\n"
                            " mul r10\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_mul_m(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};

    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RAX, 32);
    X64_emit_instr_mov_mi(&proc0->x64_instrs, 4, rsp_addr, 2);
    X64_emit_instr_mov_rr(&proc0->x64_instrs, 8, X64_R10, X64_RSP);
    X64_emit_instr_mov_mi(&proc0->x64_instrs, 4, r10_addr, 2);

    X64_emit_instr_mul_m(&proc0->x64_instrs, 1, rsp_addr);
    X64_emit_instr_mul_m(&proc0->x64_instrs, 2, rsp_addr);
    X64_emit_instr_mul_m(&proc0->x64_instrs, 4, rsp_addr);
    X64_emit_instr_mul_m(&proc0->x64_instrs, 8, rsp_addr);
    X64_emit_instr_mul_m(&proc0->x64_instrs, 8, r10_addr);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov eax, 32\n"
                            " mov dword [rsp - 0x8], 2\n"
                            " mov r10, rsp\n"
                            " mov dword [r10 - 0x8], 2\n"
                            " mul byte [rsp - 0x8]\n"
                            " mul word [rsp - 0x8]\n"
                            " mul dword [rsp - 0x8]\n"
                            " mul qword [rsp - 0x8]\n"
                            " mul qword [r10 - 0x8]\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_imul_r(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RAX, 32);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RCX, 2);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_R10, 2);
    X64_emit_instr_imul_r(&proc0->x64_instrs, 1, X64_RCX);
    X64_emit_instr_imul_r(&proc0->x64_instrs, 2, X64_RCX);
    X64_emit_instr_imul_r(&proc0->x64_instrs, 4, X64_RCX);
    X64_emit_instr_imul_r(&proc0->x64_instrs, 8, X64_RCX);
    X64_emit_instr_imul_r(&proc0->x64_instrs, 8, X64_R10);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov eax, 32\n"
                            " mov ecx, 2\n"
                            " mov r10d, 2\n"
                            " imul cl\n"
                            " imul cx\n"
                            " imul ecx\n"
                            " imul rcx\n"
                            " imul r10\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_imul_m(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};

    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RAX, 32);
    X64_emit_instr_mov_mi(&proc0->x64_instrs, 4, rsp_addr, 2);
    X64_emit_instr_mov_rr(&proc0->x64_instrs, 8, X64_R10, X64_RSP);
    X64_emit_instr_mov_mi(&proc0->x64_instrs, 4, r10_addr, 2);

    X64_emit_instr_imul_m(&proc0->x64_instrs, 1, rsp_addr);
    X64_emit_instr_imul_m(&proc0->x64_instrs, 2, rsp_addr);
    X64_emit_instr_imul_m(&proc0->x64_instrs, 4, rsp_addr);
    X64_emit_instr_imul_m(&proc0->x64_instrs, 8, rsp_addr);
    X64_emit_instr_imul_m(&proc0->x64_instrs, 8, r10_addr);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov eax, 32\n"
                            " mov dword [rsp - 0x8], 2\n"
                            " mov r10, rsp\n"
                            " mov dword [r10 - 0x8], 2\n"
                            " imul byte [rsp - 0x8]\n"
                            " imul word [rsp - 0x8]\n"
                            " imul dword [rsp - 0x8]\n"
                            " imul qword [rsp - 0x8]\n"
                            " imul qword [r10 - 0x8]\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_imul_rr(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_imul_rr(&proc0->x64_instrs, 2, X64_RDX, X64_R10);
    X64_emit_instr_imul_rr(&proc0->x64_instrs, 4, X64_R10, X64_RDX);
    X64_emit_instr_imul_rr(&proc0->x64_instrs, 8, X64_RDX, X64_R10);
    X64_emit_instr_imul_rr(&proc0->x64_instrs, 8, X64_RDX, X64_RCX);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " imul dx, r10w\n"
                            " imul r10d, edx\n"
                            " imul rdx, r10\n"
                            " imul rdx, rcx\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_imul_rm(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rbp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RBP, .local.disp = -8};
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};
    X64_emit_instr_imul_rm(&proc0->x64_instrs, 2, X64_R11, rbp_addr);
    X64_emit_instr_imul_rm(&proc0->x64_instrs, 2, X64_RDX, rsp_addr);
    X64_emit_instr_imul_rm(&proc0->x64_instrs, 4, X64_R10, r10_addr);
    X64_emit_instr_imul_rm(&proc0->x64_instrs, 8, X64_RDX, rbp_addr);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " imul r11w, word [rbp - 8]\n"
                            " imul dx, word [rsp - 8]\n"
                            " imul r10d, dword [r10 - 8]\n"
                            " imul rdx, qword [rbp - 8]\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_imul_ri(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RAX, 2);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_R10, 2);
    X64_emit_instr_imul_ri(&proc0->x64_instrs, 2, X64_RAX, 2);
    X64_emit_instr_imul_ri(&proc0->x64_instrs, 4, X64_RAX, 2);
    X64_emit_instr_imul_ri(&proc0->x64_instrs, 8, X64_RAX, 2);
    X64_emit_instr_imul_ri(&proc0->x64_instrs, 8, X64_RAX, 256);
    X64_emit_instr_imul_ri(&proc0->x64_instrs, 8, X64_R10, 2);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov eax, 2\n"
                            " mov r10d, 2\n"
                            " imul ax, ax, 0x2\n"
                            " imul eax, eax, 0x2\n"
                            " imul rax, rax, 0x2\n"
                            " imul rax, rax, 256\n"
                            " imul r10, r10, 0x2\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_movsx_rr(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 1, X64_RCX, -2);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 1, X64_R10, -2);

    X64_emit_instr_movsx_rr(&proc0->x64_instrs, 2, X64_RAX, 1, X64_RCX);
    X64_emit_instr_movsx_rr(&proc0->x64_instrs, 4, X64_RAX, 1, X64_RCX);
    X64_emit_instr_movsx_rr(&proc0->x64_instrs, 8, X64_RAX, 1, X64_RCX);
    X64_emit_instr_movsx_rr(&proc0->x64_instrs, 4, X64_RAX, 2, X64_RCX);
    X64_emit_instr_movsx_rr(&proc0->x64_instrs, 8, X64_RAX, 2, X64_RCX);
    X64_emit_instr_movsxd_rr(&proc0->x64_instrs, 8, X64_RAX, 4, X64_RCX);

    X64_emit_instr_movsx_rr(&proc0->x64_instrs, 2, X64_R11, 1, X64_R10);
    X64_emit_instr_movsx_rr(&proc0->x64_instrs, 4, X64_R11, 1, X64_R10);
    X64_emit_instr_movsx_rr(&proc0->x64_instrs, 8, X64_R11, 1, X64_R10);
    X64_emit_instr_movsx_rr(&proc0->x64_instrs, 4, X64_R11, 2, X64_R10);
    X64_emit_instr_movsx_rr(&proc0->x64_instrs, 8, X64_R11, 2, X64_R10);
    X64_emit_instr_movsxd_rr(&proc0->x64_instrs, 8, X64_R11, 4, X64_R10);

    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov cl, -2\n"
                            " mov r10b, -2\n"
                            " movsx ax, cl\n"
                            " movsx eax, cl\n"
                            " movsx rax, cl\n"
                            " movsx eax, cx\n"
                            " movsx rax, cx\n"
                            " movsxd rax, ecx\n"
                            " movsx r11w, r10b\n"
                            " movsx r11d, r10b\n"
                            " movsx r11, r10b\n"
                            " movsx r11d, r10w\n"
                            " movsx r11, r10w\n"
                            " movsxd r11, r10d\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_movsx_rm(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};
    X64_emit_instr_mov_mi(&proc0->x64_instrs, 4, rsp_addr, -2);
    X64_emit_instr_mov_rr(&proc0->x64_instrs, 8, X64_R10, X64_RSP);

    X64_emit_instr_movsx_rm(&proc0->x64_instrs, 2, X64_RAX, 1, rsp_addr);
    X64_emit_instr_movsx_rm(&proc0->x64_instrs, 4, X64_RAX, 1, rsp_addr);
    X64_emit_instr_movsx_rm(&proc0->x64_instrs, 8, X64_RAX, 1, rsp_addr);
    X64_emit_instr_movsx_rm(&proc0->x64_instrs, 4, X64_RAX, 2, rsp_addr);
    X64_emit_instr_movsx_rm(&proc0->x64_instrs, 8, X64_RAX, 2, rsp_addr);
    X64_emit_instr_movsxd_rm(&proc0->x64_instrs, 8, X64_RAX, 4, rsp_addr);

    X64_emit_instr_movsx_rm(&proc0->x64_instrs, 2, X64_R11, 1, r10_addr);
    X64_emit_instr_movsx_rm(&proc0->x64_instrs, 4, X64_R11, 1, r10_addr);
    X64_emit_instr_movsx_rm(&proc0->x64_instrs, 8, X64_R11, 1, r10_addr);
    X64_emit_instr_movsx_rm(&proc0->x64_instrs, 4, X64_R11, 2, r10_addr);
    X64_emit_instr_movsx_rm(&proc0->x64_instrs, 8, X64_R11, 2, r10_addr);
    X64_emit_instr_movsxd_rm(&proc0->x64_instrs, 8, X64_R11, 4, r10_addr);

    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov dword [rsp - 0x8], -2\n"
                            " mov r10, rsp\n"
                            " movsx ax, byte [rsp - 0x8]\n"
                            " movsx eax, byte [rsp - 0x8]\n"
                            " movsx rax, byte [rsp - 0x8]\n"
                            " movsx eax, word [rsp - 0x8]\n"
                            " movsx rax, word [rsp - 0x8]\n"
                            " movsxd rax, dword [rsp - 0x8]\n"
                            " movsx r11w, byte [r10 - 0x8]\n"
                            " movsx r11d, byte [r10 - 0x8]\n"
                            " movsx r11, byte [r10 - 0x8]\n"
                            " movsx r11d, word [r10 - 0x8]\n"
                            " movsx r11, word [r10 - 0x8]\n"
                            " movsxd r11, dword [r10 - 0x8]\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_and_rr(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_and_rr(&proc0->x64_instrs, 1, X64_RDX, X64_RSI);
    X64_emit_instr_and_rr(&proc0->x64_instrs, 2, X64_RDX, X64_R10);
    X64_emit_instr_and_rr(&proc0->x64_instrs, 4, X64_R10, X64_RDX);
    X64_emit_instr_and_rr(&proc0->x64_instrs, 8, X64_RDX, X64_R10);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " and dl, sil\n"
                            " and dx, r10w\n"
                            " and r10d, edx\n"
                            " and rdx, r10\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_and_rm(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rbp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RBP, .local.disp = -8};
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};
    X64_emit_instr_and_rm(&proc0->x64_instrs, 1, X64_RDX, rbp_addr);
    X64_emit_instr_and_rm(&proc0->x64_instrs, 2, X64_RDX, rsp_addr);
    X64_emit_instr_and_rm(&proc0->x64_instrs, 4, X64_R10, r10_addr);
    X64_emit_instr_and_rm(&proc0->x64_instrs, 8, X64_RDX, rbp_addr);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " and dl, byte [rbp - 8]\n"
                            " and dx, word [rsp - 8]\n"
                            " and r10d, dword [r10 - 8]\n"
                            " and rdx, qword [rbp - 8]\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_and_mr(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rbp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RBP, .local.disp = -8};
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};
    X64_emit_instr_and_mr(&proc0->x64_instrs, 1, rbp_addr, X64_RDX);
    X64_emit_instr_and_mr(&proc0->x64_instrs, 2, rsp_addr, X64_RDX);
    X64_emit_instr_and_mr(&proc0->x64_instrs, 4, r10_addr, X64_R10);
    X64_emit_instr_and_mr(&proc0->x64_instrs, 8, rbp_addr, X64_RDX);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " and byte [rbp - 8], dl\n"
                            " and word [rsp - 8], dx\n"
                            " and dword [r10 - 8], r10d\n"
                            " and qword [rbp - 8], rdx\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_and_ri(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RDX, 3);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_R10, 128);

    // imm can fit in a byte
    X64_emit_instr_and_ri(&proc0->x64_instrs, 1, X64_RSI, 2);
    X64_emit_instr_and_ri(&proc0->x64_instrs, 2, X64_RDX, 2);
    X64_emit_instr_and_ri(&proc0->x64_instrs, 4, X64_RDX, 127);
    X64_emit_instr_and_ri(&proc0->x64_instrs, 8, X64_RDX, -128);

    // imm cannot fit in a byte
    X64_emit_instr_and_ri(&proc0->x64_instrs, 2, X64_RDX, 256);
    X64_emit_instr_and_ri(&proc0->x64_instrs, 4, X64_RDX, 255);
    X64_emit_instr_and_ri(&proc0->x64_instrs, 8, X64_R10, 129);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov edx, 3\n"
                            " mov r10d, 128\n"
                            " and sil, 0x2\n"
                            " and dx, 0x2\n"
                            " and edx, 127\n"
                            " and rdx, -128\n"
                            " and dx, 256\n"
                            " and edx, 255\n"
                            " and r10, 129\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_and_mi(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rbp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RBP, .local.disp = -8};
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};

    // imm can fit in a byte
    X64_emit_instr_and_mi(&proc0->x64_instrs, 1, rbp_addr, 2);
    X64_emit_instr_and_mi(&proc0->x64_instrs, 2, rbp_addr, 2);
    X64_emit_instr_and_mi(&proc0->x64_instrs, 4, rsp_addr, 127);
    X64_emit_instr_and_mi(&proc0->x64_instrs, 8, r10_addr, -128);

    // imm cannot fit in a byte
    X64_emit_instr_and_mi(&proc0->x64_instrs, 2, r10_addr, 256);
    X64_emit_instr_and_mi(&proc0->x64_instrs, 4, rbp_addr, 255);
    X64_emit_instr_and_mi(&proc0->x64_instrs, 8, rsp_addr, 129);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " and byte [rbp - 8], 0x2\n"
                            " and word [rbp - 8], 0x2\n"
                            " and dword [rsp - 8], 127\n"
                            " and qword [r10 - 8], -128\n"
                            " and word [r10 - 8], 256\n"
                            " and dword [rbp - 8], 255\n"
                            " and qword [rsp - 8], 129\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

// OR

static bool test_or_rr(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_or_rr(&proc0->x64_instrs, 1, X64_RDX, X64_RAX);
    X64_emit_instr_or_rr(&proc0->x64_instrs, 2, X64_RDX, X64_R10);
    X64_emit_instr_or_rr(&proc0->x64_instrs, 4, X64_R10, X64_RDX);
    X64_emit_instr_or_rr(&proc0->x64_instrs, 8, X64_RDX, X64_R10);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " or dl, al\n"
                            " or dx, r10w\n"
                            " or r10d, edx\n"
                            " or rdx, r10\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_or_rm(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rbp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RBP, .local.disp = -8};
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};
    X64_emit_instr_or_rm(&proc0->x64_instrs, 1, X64_RDX, rbp_addr);
    X64_emit_instr_or_rm(&proc0->x64_instrs, 2, X64_RDX, rsp_addr);
    X64_emit_instr_or_rm(&proc0->x64_instrs, 4, X64_R10, r10_addr);
    X64_emit_instr_or_rm(&proc0->x64_instrs, 8, X64_RDX, rbp_addr);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " or dl, byte [rbp - 8]\n"
                            " or dx, word [rsp - 8]\n"
                            " or r10d, dword [r10 - 8]\n"
                            " or rdx, qword [rbp - 8]\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_or_mr(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rbp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RBP, .local.disp = -8};
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};
    X64_emit_instr_or_mr(&proc0->x64_instrs, 1, rbp_addr, X64_RDX);
    X64_emit_instr_or_mr(&proc0->x64_instrs, 2, rsp_addr, X64_RDX);
    X64_emit_instr_or_mr(&proc0->x64_instrs, 4, r10_addr, X64_R10);
    X64_emit_instr_or_mr(&proc0->x64_instrs, 8, rbp_addr, X64_RDX);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " or byte [rbp - 8], dl\n"
                            " or word [rsp - 8], dx\n"
                            " or dword [r10 - 8], r10d\n"
                            " or qword [rbp - 8], rdx\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_or_ri(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RDX, 3);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_R10, 128);

    // imm can fit in a byte
    X64_emit_instr_or_ri(&proc0->x64_instrs, 1, X64_RDX, 2);
    X64_emit_instr_or_ri(&proc0->x64_instrs, 2, X64_RDX, 2);
    X64_emit_instr_or_ri(&proc0->x64_instrs, 4, X64_RDX, 127);
    X64_emit_instr_or_ri(&proc0->x64_instrs, 8, X64_RDX, -128);

    // imm cannot fit in a byte
    X64_emit_instr_or_ri(&proc0->x64_instrs, 2, X64_RDX, 256);
    X64_emit_instr_or_ri(&proc0->x64_instrs, 4, X64_RDX, 255);
    X64_emit_instr_or_ri(&proc0->x64_instrs, 8, X64_R10, 129);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov edx, 3\n"
                            " mov r10d, 128\n"
                            " or dl, 0x2\n"
                            " or dx, 0x2\n"
                            " or edx, 127\n"
                            " or rdx, -128\n"
                            " or dx, 256\n"
                            " or edx, 255\n"
                            " or r10, 129\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_or_mi(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rbp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RBP, .local.disp = -8};
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};

    // imm can fit in a byte
    X64_emit_instr_or_mi(&proc0->x64_instrs, 1, rbp_addr, 2);
    X64_emit_instr_or_mi(&proc0->x64_instrs, 2, rbp_addr, 2);
    X64_emit_instr_or_mi(&proc0->x64_instrs, 4, rsp_addr, 127);
    X64_emit_instr_or_mi(&proc0->x64_instrs, 8, r10_addr, -128);

    // imm cannot fit in a byte
    X64_emit_instr_or_mi(&proc0->x64_instrs, 2, r10_addr, 256);
    X64_emit_instr_or_mi(&proc0->x64_instrs, 4, rbp_addr, 255);
    X64_emit_instr_or_mi(&proc0->x64_instrs, 8, rsp_addr, 129);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " or byte [rbp - 8], 0x2\n"
                            " or word [rbp - 8], 0x2\n"
                            " or dword [rsp - 8], 127\n"
                            " or qword [r10 - 8], -128\n"
                            " or word [r10 - 8], 256\n"
                            " or dword [rbp - 8], 255\n"
                            " or qword [rsp - 8], 129\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

// XOR
static bool test_xor_rr(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_xor_rr(&proc0->x64_instrs, 1, X64_RDX, X64_RAX);
    X64_emit_instr_xor_rr(&proc0->x64_instrs, 2, X64_RDX, X64_R10);
    X64_emit_instr_xor_rr(&proc0->x64_instrs, 4, X64_R10, X64_RDX);
    X64_emit_instr_xor_rr(&proc0->x64_instrs, 8, X64_RDX, X64_R10);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " xor dl, al\n"
                            " xor dx, r10w\n"
                            " xor r10d, edx\n"
                            " xor rdx, r10\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_xor_rm(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rbp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RBP, .local.disp = -8};
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};
    X64_emit_instr_xor_rm(&proc0->x64_instrs, 1, X64_RDX, rbp_addr);
    X64_emit_instr_xor_rm(&proc0->x64_instrs, 2, X64_RDX, rsp_addr);
    X64_emit_instr_xor_rm(&proc0->x64_instrs, 4, X64_R10, r10_addr);
    X64_emit_instr_xor_rm(&proc0->x64_instrs, 8, X64_RDX, rbp_addr);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " xor dl, byte [rbp - 8]\n"
                            " xor dx, word [rsp - 8]\n"
                            " xor r10d, dword [r10 - 8]\n"
                            " xor rdx, qword [rbp - 8]\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_xor_mr(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rbp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RBP, .local.disp = -8};
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};
    X64_emit_instr_xor_mr(&proc0->x64_instrs, 1, rbp_addr, X64_RDX);
    X64_emit_instr_xor_mr(&proc0->x64_instrs, 2, rsp_addr, X64_RDX);
    X64_emit_instr_xor_mr(&proc0->x64_instrs, 4, r10_addr, X64_R10);
    X64_emit_instr_xor_mr(&proc0->x64_instrs, 8, rbp_addr, X64_RDX);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " xor byte [rbp - 8], dl\n"
                            " xor word [rsp - 8], dx\n"
                            " xor dword [r10 - 8], r10d\n"
                            " xor qword [rbp - 8], rdx\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_xor_ri(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RDX, 3);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_R10, 128);

    // imm can fit in a byte
    X64_emit_instr_xor_ri(&proc0->x64_instrs, 1, X64_RDX, 2);
    X64_emit_instr_xor_ri(&proc0->x64_instrs, 2, X64_RDX, 2);
    X64_emit_instr_xor_ri(&proc0->x64_instrs, 4, X64_RDX, 127);
    X64_emit_instr_xor_ri(&proc0->x64_instrs, 8, X64_RDX, -128);

    // imm cannot fit in a byte
    X64_emit_instr_xor_ri(&proc0->x64_instrs, 2, X64_RDX, 256);
    X64_emit_instr_xor_ri(&proc0->x64_instrs, 4, X64_RDX, 255);
    X64_emit_instr_xor_ri(&proc0->x64_instrs, 8, X64_R10, 129);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov edx, 3\n"
                            " mov r10d, 128\n"
                            " xor dl, 0x2\n"
                            " xor dx, 0x2\n"
                            " xor edx, 127\n"
                            " xor rdx, -128\n"
                            " xor dx, 256\n"
                            " xor edx, 255\n"
                            " xor r10, 129\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_xor_mi(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rbp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RBP, .local.disp = -8};
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};

    // imm can fit in a byte
    X64_emit_instr_xor_mi(&proc0->x64_instrs, 1, rbp_addr, 2);
    X64_emit_instr_xor_mi(&proc0->x64_instrs, 2, rbp_addr, 2);
    X64_emit_instr_xor_mi(&proc0->x64_instrs, 4, rsp_addr, 127);
    X64_emit_instr_xor_mi(&proc0->x64_instrs, 8, r10_addr, -128);

    // imm cannot fit in a byte
    X64_emit_instr_xor_mi(&proc0->x64_instrs, 2, r10_addr, 256);
    X64_emit_instr_xor_mi(&proc0->x64_instrs, 4, rbp_addr, 255);
    X64_emit_instr_xor_mi(&proc0->x64_instrs, 8, rsp_addr, 129);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " xor byte [rbp - 8], 0x2\n"
                            " xor word [rbp - 8], 0x2\n"
                            " xor dword [rsp - 8], 127\n"
                            " xor qword [r10 - 8], -128\n"
                            " xor word [r10 - 8], 256\n"
                            " xor dword [rbp - 8], 255\n"
                            " xor qword [rsp - 8], 129\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_shl_r(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RCX, 4);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RDX, 1);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_R10, 2);

    X64_emit_instr_shl_rr(&proc0->x64_instrs, 1, X64_RDX);
    X64_emit_instr_shl_rr(&proc0->x64_instrs, 2, X64_RDX);
    X64_emit_instr_shl_rr(&proc0->x64_instrs, 4, X64_RDX);
    X64_emit_instr_shl_rr(&proc0->x64_instrs, 8, X64_RDX);
    X64_emit_instr_shl_rr(&proc0->x64_instrs, 8, X64_R10);

    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov ecx, 4\n"
                            " mov edx, 1\n"
                            " mov r10d, 2\n"
                            " shl dl, cl\n"
                            " shl dx, cl\n"
                            " shl edx, cl\n"
                            " shl rdx, cl\n"
                            " shl r10, cl\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_shl_m(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rbp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RBP, .local.disp = -8};
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};
    X64_emit_instr_mov_rr(&proc0->x64_instrs, 8, X64_RBP, X64_RSP);
    X64_emit_instr_mov_rr(&proc0->x64_instrs, 8, X64_R10, X64_RSP);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RCX, 1);

    X64_emit_instr_shl_mr(&proc0->x64_instrs, 1, rbp_addr);
    X64_emit_instr_shl_mr(&proc0->x64_instrs, 2, rsp_addr);
    X64_emit_instr_shl_mr(&proc0->x64_instrs, 4, rsp_addr);
    X64_emit_instr_shl_mr(&proc0->x64_instrs, 8, rbp_addr);
    X64_emit_instr_shl_mr(&proc0->x64_instrs, 8, r10_addr);

    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov rbp, rsp\n"
                            " mov r10, rsp\n"
                            " mov ecx, 1\n"
                            " shl byte [rbp - 8], cl\n"
                            " shl word [rsp - 8], cl\n"
                            " shl dword [rsp - 8], cl\n"
                            " shl qword [rbp - 8], cl\n"
                            " shl qword [r10 - 8], cl\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_shl_ri(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RDX, 1);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_R10, 2);

    // imm == 1
    X64_emit_instr_shl_ri(&proc0->x64_instrs, 1, X64_RDX, 1);
    X64_emit_instr_shl_ri(&proc0->x64_instrs, 2, X64_RDX, 1);
    X64_emit_instr_shl_ri(&proc0->x64_instrs, 4, X64_RDX, 1);
    X64_emit_instr_shl_ri(&proc0->x64_instrs, 8, X64_RDX, 1);
    X64_emit_instr_shl_ri(&proc0->x64_instrs, 8, X64_R10, 1);

    // imm != 1
    X64_emit_instr_shl_ri(&proc0->x64_instrs, 1, X64_RDX, 2);
    X64_emit_instr_shl_ri(&proc0->x64_instrs, 2, X64_RDX, 6);
    X64_emit_instr_shl_ri(&proc0->x64_instrs, 4, X64_RDX, 8);
    X64_emit_instr_shl_ri(&proc0->x64_instrs, 8, X64_RDX, 32);
    X64_emit_instr_shl_ri(&proc0->x64_instrs, 8, X64_R10, 32);

    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov edx, 1\n"
                            " mov r10d, 2\n"
                            " shl dl, 1\n"
                            " shl dx, 1\n"
                            " shl edx, 1\n"
                            " shl rdx, 1\n"
                            " shl r10, 1\n"
                            " shl dl, 2\n"
                            " shl dx, 6\n"
                            " shl edx, 8\n"
                            " shl rdx, 32\n"
                            " shl r10, 32\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_shl_mi(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rbp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RBP, .local.disp = -8};
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};
    X64_emit_instr_mov_rr(&proc0->x64_instrs, 8, X64_RBP, X64_RSP);
    X64_emit_instr_mov_rr(&proc0->x64_instrs, 8, X64_R10, X64_RSP);

    // imm == 1
    X64_emit_instr_shl_mi(&proc0->x64_instrs, 1, rbp_addr, 1);
    X64_emit_instr_shl_mi(&proc0->x64_instrs, 2, rsp_addr, 1);
    X64_emit_instr_shl_mi(&proc0->x64_instrs, 4, rsp_addr, 1);
    X64_emit_instr_shl_mi(&proc0->x64_instrs, 8, rbp_addr, 1);
    X64_emit_instr_shl_mi(&proc0->x64_instrs, 8, r10_addr, 1);

    // imm != 1
    X64_emit_instr_shl_mi(&proc0->x64_instrs, 1, rbp_addr, 2);
    X64_emit_instr_shl_mi(&proc0->x64_instrs, 2, rsp_addr, 6);
    X64_emit_instr_shl_mi(&proc0->x64_instrs, 4, rsp_addr, 8);
    X64_emit_instr_shl_mi(&proc0->x64_instrs, 8, rbp_addr, 32);
    X64_emit_instr_shl_mi(&proc0->x64_instrs, 8, r10_addr, 32);

    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov rbp, rsp\n"
                            " mov r10, rsp\n"
                            " shl byte [rbp - 8], 1\n"
                            " shl word [rsp - 8], 1\n"
                            " shl dword [rsp - 8], 1\n"
                            " shl qword [rbp - 8], 1\n"
                            " shl qword [r10 - 8], 1\n"
                            " shl byte [rbp - 8], 2\n"
                            " shl word [rsp - 8], 6\n"
                            " shl dword [rsp - 8], 8\n"
                            " shl qword [rbp - 8], 32\n"
                            " shl qword [r10 - 8], 32\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_sar_r(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RCX, 2);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RDX, 1024);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_R10, 1024);

    X64_emit_instr_sar_rr(&proc0->x64_instrs, 1, X64_RDX);
    X64_emit_instr_sar_rr(&proc0->x64_instrs, 2, X64_RDX);
    X64_emit_instr_sar_rr(&proc0->x64_instrs, 4, X64_RDX);
    X64_emit_instr_sar_rr(&proc0->x64_instrs, 8, X64_RDX);
    X64_emit_instr_sar_rr(&proc0->x64_instrs, 8, X64_R10);

    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov ecx, 2\n"
                            " mov edx, 1024\n"
                            " mov r10d, 1024\n"
                            " sar dl, cl\n"
                            " sar dx, cl\n"
                            " sar edx, cl\n"
                            " sar rdx, cl\n"
                            " sar r10, cl\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_sar_m(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rbp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RBP, .local.disp = -8};
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};
    X64_emit_instr_mov_rr(&proc0->x64_instrs, 8, X64_RBP, X64_RSP);
    X64_emit_instr_mov_rr(&proc0->x64_instrs, 8, X64_R10, X64_RSP);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RCX, 1);

    X64_emit_instr_sar_mr(&proc0->x64_instrs, 1, rbp_addr);
    X64_emit_instr_sar_mr(&proc0->x64_instrs, 2, rsp_addr);
    X64_emit_instr_sar_mr(&proc0->x64_instrs, 4, rsp_addr);
    X64_emit_instr_sar_mr(&proc0->x64_instrs, 8, rbp_addr);
    X64_emit_instr_sar_mr(&proc0->x64_instrs, 8, r10_addr);

    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov rbp, rsp\n"
                            " mov r10, rsp\n"
                            " mov ecx, 1\n"
                            " sar byte [rbp - 8], cl\n"
                            " sar word [rsp - 8], cl\n"
                            " sar dword [rsp - 8], cl\n"
                            " sar qword [rbp - 8], cl\n"
                            " sar qword [r10 - 8], cl\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_sar_ri(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RDX, 0xFFFF);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_R10, 0xFFFF);

    // imm == 1
    X64_emit_instr_sar_ri(&proc0->x64_instrs, 1, X64_RDX, 1);
    X64_emit_instr_sar_ri(&proc0->x64_instrs, 2, X64_RDX, 1);
    X64_emit_instr_sar_ri(&proc0->x64_instrs, 4, X64_RDX, 1);
    X64_emit_instr_sar_ri(&proc0->x64_instrs, 8, X64_RDX, 1);
    X64_emit_instr_sar_ri(&proc0->x64_instrs, 8, X64_R10, 1);

    // imm != 1
    X64_emit_instr_sar_ri(&proc0->x64_instrs, 1, X64_RDX, 2);
    X64_emit_instr_sar_ri(&proc0->x64_instrs, 2, X64_RDX, 6);
    X64_emit_instr_sar_ri(&proc0->x64_instrs, 4, X64_RDX, 8);
    X64_emit_instr_sar_ri(&proc0->x64_instrs, 8, X64_RDX, 32);
    X64_emit_instr_sar_ri(&proc0->x64_instrs, 8, X64_R10, 32);

    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov edx, 0xFFFF\n"
                            " mov r10d, 0xFFFF\n"
                            " sar dl, 1\n"
                            " sar dx, 1\n"
                            " sar edx, 1\n"
                            " sar rdx, 1\n"
                            " sar r10, 1\n"
                            " sar dl, 2\n"
                            " sar dx, 6\n"
                            " sar edx, 8\n"
                            " sar rdx, 32\n"
                            " sar r10, 32\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_sar_mi(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rbp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RBP, .local.disp = -8};
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};
    X64_emit_instr_mov_rr(&proc0->x64_instrs, 8, X64_RBP, X64_RSP);
    X64_emit_instr_mov_rr(&proc0->x64_instrs, 8, X64_R10, X64_RSP);

    // imm == 1
    X64_emit_instr_sar_mi(&proc0->x64_instrs, 1, rbp_addr, 1);
    X64_emit_instr_sar_mi(&proc0->x64_instrs, 2, rsp_addr, 1);
    X64_emit_instr_sar_mi(&proc0->x64_instrs, 4, rsp_addr, 1);
    X64_emit_instr_sar_mi(&proc0->x64_instrs, 8, rbp_addr, 1);
    X64_emit_instr_sar_mi(&proc0->x64_instrs, 8, r10_addr, 1);

    // imm != 1
    X64_emit_instr_sar_mi(&proc0->x64_instrs, 1, rbp_addr, 2);
    X64_emit_instr_sar_mi(&proc0->x64_instrs, 2, rsp_addr, 6);
    X64_emit_instr_sar_mi(&proc0->x64_instrs, 4, rsp_addr, 8);
    X64_emit_instr_sar_mi(&proc0->x64_instrs, 8, rbp_addr, 32);
    X64_emit_instr_sar_mi(&proc0->x64_instrs, 8, r10_addr, 32);

    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov rbp, rsp\n"
                            " mov r10, rsp\n"
                            " sar byte [rbp - 8], 1\n"
                            " sar word [rsp - 8], 1\n"
                            " sar dword [rsp - 8], 1\n"
                            " sar qword [rbp - 8], 1\n"
                            " sar qword [r10 - 8], 1\n"
                            " sar byte [rbp - 8], 2\n"
                            " sar word [rsp - 8], 6\n"
                            " sar dword [rsp - 8], 8\n"
                            " sar qword [rbp - 8], 32\n"
                            " sar qword [r10 - 8], 32\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_neg_r(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RDX, 1024);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_R10, 1024);

    X64_emit_instr_neg_r(&proc0->x64_instrs, 1, X64_RDX);
    X64_emit_instr_neg_r(&proc0->x64_instrs, 2, X64_RDX);
    X64_emit_instr_neg_r(&proc0->x64_instrs, 4, X64_RDX);
    X64_emit_instr_neg_r(&proc0->x64_instrs, 8, X64_RDX);
    X64_emit_instr_neg_r(&proc0->x64_instrs, 8, X64_R10);

    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov edx, 1024\n"
                            " mov r10d, 1024\n"
                            " neg dl\n"
                            " neg dx\n"
                            " neg edx\n"
                            " neg rdx\n"
                            " neg r10\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_neg_m(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rbp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RBP, .local.disp = -8};
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};
    X64_emit_instr_mov_rr(&proc0->x64_instrs, 8, X64_RBP, X64_RSP);
    X64_emit_instr_mov_rr(&proc0->x64_instrs, 8, X64_R10, X64_RSP);

    X64_emit_instr_neg_m(&proc0->x64_instrs, 1, rbp_addr);
    X64_emit_instr_neg_m(&proc0->x64_instrs, 2, rsp_addr);
    X64_emit_instr_neg_m(&proc0->x64_instrs, 4, rsp_addr);
    X64_emit_instr_neg_m(&proc0->x64_instrs, 8, rbp_addr);
    X64_emit_instr_neg_m(&proc0->x64_instrs, 8, r10_addr);

    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov rbp, rsp\n"
                            " mov r10, rsp\n"
                            " neg byte [rbp - 8]\n"
                            " neg word [rsp - 8]\n"
                            " neg dword [rsp - 8]\n"
                            " neg qword [rbp - 8]\n"
                            " neg qword [r10 - 8]\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_not_r(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_RDX, 1024);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_R10, 1024);

    X64_emit_instr_not_r(&proc0->x64_instrs, 1, X64_RDX);
    X64_emit_instr_not_r(&proc0->x64_instrs, 2, X64_RDX);
    X64_emit_instr_not_r(&proc0->x64_instrs, 4, X64_RDX);
    X64_emit_instr_not_r(&proc0->x64_instrs, 8, X64_RDX);
    X64_emit_instr_not_r(&proc0->x64_instrs, 8, X64_R10);

    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov edx, 1024\n"
                            " mov r10d, 1024\n"
                            " not dl\n"
                            " not dx\n"
                            " not edx\n"
                            " not rdx\n"
                            " not r10\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_not_m(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rbp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RBP, .local.disp = -8};
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};
    X64_emit_instr_mov_rr(&proc0->x64_instrs, 8, X64_RBP, X64_RSP);
    X64_emit_instr_mov_rr(&proc0->x64_instrs, 8, X64_R10, X64_RSP);

    X64_emit_instr_not_m(&proc0->x64_instrs, 1, rbp_addr);
    X64_emit_instr_not_m(&proc0->x64_instrs, 2, rsp_addr);
    X64_emit_instr_not_m(&proc0->x64_instrs, 4, rsp_addr);
    X64_emit_instr_not_m(&proc0->x64_instrs, 8, rbp_addr);
    X64_emit_instr_not_m(&proc0->x64_instrs, 8, r10_addr);

    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov rbp, rsp\n"
                            " mov r10, rsp\n"
                            " not byte [rbp - 8]\n"
                            " not word [rsp - 8]\n"
                            " not dword [rsp - 8]\n"
                            " not qword [rbp - 8]\n"
                            " not qword [r10 - 8]\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_mov_flt_rr(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_mov_flt_rr(&proc0->x64_instrs, FLOAT_F32, X64_XMM1, X64_XMM8);
    X64_emit_instr_mov_flt_rr(&proc0->x64_instrs, FLOAT_F32, X64_XMM11, X64_XMM1);
    X64_emit_instr_mov_flt_rr(&proc0->x64_instrs, FLOAT_F64, X64_XMM1, X64_XMM9);
    X64_emit_instr_mov_flt_rr(&proc0->x64_instrs, FLOAT_F64, X64_XMM11, X64_XMM1);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " movss xmm1, xmm8\n"
                            " movss xmm11, xmm1\n"
                            " movsd xmm1, xmm9\n"
                            " movsd xmm11, xmm1\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_mov_flt_rm(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rbp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RBP, .local.disp = -8};
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};
    X64_emit_instr_mov_flt_rm(&proc0->x64_instrs, FLOAT_F32, X64_XMM1, rbp_addr);
    X64_emit_instr_mov_flt_rm(&proc0->x64_instrs, FLOAT_F32, X64_XMM11, rsp_addr);
    X64_emit_instr_mov_flt_rm(&proc0->x64_instrs, FLOAT_F64, X64_XMM1, r10_addr);
    X64_emit_instr_mov_flt_rm(&proc0->x64_instrs, FLOAT_F64, X64_XMM8, rbp_addr);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " movss xmm1, dword [rbp - 8]\n"
                            " movss xmm11, dword [rsp - 8]\n"
                            " movsd xmm1, qword [r10 - 8]\n"
                            " movsd xmm8, qword [rbp - 8]\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_mov_flt_mr(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rbp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RBP, .local.disp = -8};
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};
    X64_emit_instr_mov_flt_mr(&proc0->x64_instrs, FLOAT_F32, rbp_addr, X64_XMM1);
    X64_emit_instr_mov_flt_mr(&proc0->x64_instrs, FLOAT_F32, rsp_addr, X64_XMM11);
    X64_emit_instr_mov_flt_mr(&proc0->x64_instrs, FLOAT_F64, r10_addr, X64_XMM1);
    X64_emit_instr_mov_flt_mr(&proc0->x64_instrs, FLOAT_F64, rbp_addr, X64_XMM8);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " movss dword [rbp - 8], xmm1\n"
                            " movss dword [rsp - 8], xmm11\n"
                            " movsd qword [r10 - 8], xmm1\n"
                            " movsd qword [rbp - 8], xmm8\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_cvtss2sd_rr(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_cvtss2sd_rr(&proc0->x64_instrs, X64_XMM1, X64_XMM8);
    X64_emit_instr_cvtss2sd_rr(&proc0->x64_instrs, X64_XMM11, X64_XMM1);
    X64_emit_instr_cvtss2sd_rr(&proc0->x64_instrs, X64_XMM1, X64_XMM2);
    X64_emit_instr_cvtss2sd_rr(&proc0->x64_instrs, X64_XMM11, X64_XMM9);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " cvtss2sd xmm1, xmm8\n"
                            " cvtss2sd xmm11, xmm1\n"
                            " cvtss2sd xmm1, xmm2\n"
                            " cvtss2sd xmm11, xmm9\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_cvtss2sd_rm(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rbp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RBP, .local.disp = -8};
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};
    X64_emit_instr_cvtss2sd_rm(&proc0->x64_instrs, X64_XMM1, rbp_addr);
    X64_emit_instr_cvtss2sd_rm(&proc0->x64_instrs, X64_XMM11, rsp_addr);
    X64_emit_instr_cvtss2sd_rm(&proc0->x64_instrs, X64_XMM1, r10_addr);
    X64_emit_instr_cvtss2sd_rm(&proc0->x64_instrs, X64_XMM8, rbp_addr);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " cvtss2sd xmm1, dword [rbp - 8]\n"
                            " cvtss2sd xmm11, dword [rsp - 8]\n"
                            " cvtss2sd xmm1, dword [r10 - 8]\n"
                            " cvtss2sd xmm8, dword [rbp - 8]\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_cvtsd2ss_rr(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_cvtsd2ss_rr(&proc0->x64_instrs, X64_XMM1, X64_XMM8);
    X64_emit_instr_cvtsd2ss_rr(&proc0->x64_instrs, X64_XMM11, X64_XMM1);
    X64_emit_instr_cvtsd2ss_rr(&proc0->x64_instrs, X64_XMM1, X64_XMM2);
    X64_emit_instr_cvtsd2ss_rr(&proc0->x64_instrs, X64_XMM11, X64_XMM9);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " cvtsd2ss xmm1, xmm8\n"
                            " cvtsd2ss xmm11, xmm1\n"
                            " cvtsd2ss xmm1, xmm2\n"
                            " cvtsd2ss xmm11, xmm9\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_cvtsd2ss_rm(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rbp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RBP, .local.disp = -8};
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};
    X64_emit_instr_cvtsd2ss_rm(&proc0->x64_instrs, X64_XMM1, rbp_addr);
    X64_emit_instr_cvtsd2ss_rm(&proc0->x64_instrs, X64_XMM11, rsp_addr);
    X64_emit_instr_cvtsd2ss_rm(&proc0->x64_instrs, X64_XMM1, r10_addr);
    X64_emit_instr_cvtsd2ss_rm(&proc0->x64_instrs, X64_XMM8, rbp_addr);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " cvtsd2ss xmm1, qword [rbp - 8]\n"
                            " cvtsd2ss xmm11, qword [rsp - 8]\n"
                            " cvtsd2ss xmm1, qword [r10 - 8]\n"
                            " cvtsd2ss xmm8, qword [rbp - 8]\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_cvttss2si_rr(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_cvttss2si_rr(&proc0->x64_instrs, X64_RAX, false, X64_XMM0);
    X64_emit_instr_cvttss2si_rr(&proc0->x64_instrs, X64_RAX, true, X64_XMM0);
    X64_emit_instr_cvttss2si_rr(&proc0->x64_instrs, X64_R10, false, X64_XMM10);
    X64_emit_instr_cvttss2si_rr(&proc0->x64_instrs, X64_R10, true, X64_XMM10);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " cvttss2si eax, xmm0\n"
                            " cvttss2si rax, xmm0\n"
                            " cvttss2si r10d, xmm10\n"
                            " cvttss2si r10, xmm10\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_cvttss2si_rm(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rbp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RBP, .local.disp = -8};
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};
    X64_emit_instr_cvttss2si_rm(&proc0->x64_instrs, X64_RAX, false, rsp_addr);
    X64_emit_instr_cvttss2si_rm(&proc0->x64_instrs, X64_RAX, true, rsp_addr);
    X64_emit_instr_cvttss2si_rm(&proc0->x64_instrs, X64_RBX, false, rbp_addr);
    X64_emit_instr_cvttss2si_rm(&proc0->x64_instrs, X64_RBX, true, rbp_addr);
    X64_emit_instr_cvttss2si_rm(&proc0->x64_instrs, X64_R10, false, r10_addr);
    X64_emit_instr_cvttss2si_rm(&proc0->x64_instrs, X64_R10, true, r10_addr);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " cvttss2si eax, dword [rsp - 8]\n"
                            " cvttss2si rax, dword [rsp - 8]\n"
                            " cvttss2si ebx, dword [rbp - 8]\n"
                            " cvttss2si rbx, dword [rbp - 8]\n"
                            " cvttss2si r10d, dword [r10 - 8]\n"
                            " cvttss2si r10, dword [r10 - 8]\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_cvttsd2si_rm(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rbp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RBP, .local.disp = -8};
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};
    X64_emit_instr_cvttsd2si_rm(&proc0->x64_instrs, X64_RAX, false, rsp_addr);
    X64_emit_instr_cvttsd2si_rm(&proc0->x64_instrs, X64_RAX, true, rsp_addr);
    X64_emit_instr_cvttsd2si_rm(&proc0->x64_instrs, X64_RBX, false, rbp_addr);
    X64_emit_instr_cvttsd2si_rm(&proc0->x64_instrs, X64_RBX, true, rbp_addr);
    X64_emit_instr_cvttsd2si_rm(&proc0->x64_instrs, X64_R10, false, r10_addr);
    X64_emit_instr_cvttsd2si_rm(&proc0->x64_instrs, X64_R10, true, r10_addr);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " cvttsd2si eax, qword [rsp - 8]\n"
                            " cvttsd2si rax, qword [rsp - 8]\n"
                            " cvttsd2si ebx, qword [rbp - 8]\n"
                            " cvttsd2si rbx, qword [rbp - 8]\n"
                            " cvttsd2si r10d, qword [r10 - 8]\n"
                            " cvttsd2si r10, qword [r10 - 8]\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_cvttsd2si_rr(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_cvttsd2si_rr(&proc0->x64_instrs, X64_RAX, false, X64_XMM0);
    X64_emit_instr_cvttsd2si_rr(&proc0->x64_instrs, X64_RAX, true, X64_XMM0);
    X64_emit_instr_cvttsd2si_rr(&proc0->x64_instrs, X64_R10, false, X64_XMM10);
    X64_emit_instr_cvttsd2si_rr(&proc0->x64_instrs, X64_R10, true, X64_XMM10);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " cvttsd2si eax, xmm0\n"
                            " cvttsd2si rax, xmm0\n"
                            " cvttsd2si r10d, xmm10\n"
                            " cvttsd2si r10, xmm10\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_mov_rr(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_mov_rrh(&proc0->x64_instrs, X64_RCX, X64_RDX);
    X64_emit_instr_mov_rr(&proc0->x64_instrs, 1, X64_RDX, X64_RSI); // rsi aliases dh
    X64_emit_instr_mov_rr(&proc0->x64_instrs, 1, X64_RDX, X64_RCX);
    X64_emit_instr_mov_rr(&proc0->x64_instrs, 2, X64_RDX, X64_R10);
    X64_emit_instr_mov_rr(&proc0->x64_instrs, 4, X64_R10, X64_RDX);
    X64_emit_instr_mov_rr(&proc0->x64_instrs, 8, X64_RDX, X64_R10);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov cl, dh\n"
                            " mov dl, sil\n"
                            " mov dl, cl\n"
                            " mov dx, r10w\n"
                            " mov r10d, edx\n"
                            " mov rdx, r10\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_mov_mr(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rbp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RBP, .local.disp = -8};
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};
    X64_emit_instr_mov_mrh(&proc0->x64_instrs, rbp_addr, X64_RDX);
    X64_emit_instr_mov_mr(&proc0->x64_instrs, 1, rbp_addr, X64_RSI); // rsi aliases dh
    X64_emit_instr_mov_mr(&proc0->x64_instrs, 1, rbp_addr, X64_RDX);
    X64_emit_instr_mov_mr(&proc0->x64_instrs, 2, rsp_addr, X64_RDX);
    X64_emit_instr_mov_mr(&proc0->x64_instrs, 4, r10_addr, X64_R10);
    X64_emit_instr_mov_mr(&proc0->x64_instrs, 8, rbp_addr, X64_RDX);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov byte [rbp - 8], dh\n"
                            " mov byte [rbp - 8], sil\n"
                            " mov byte [rbp - 8], dl\n"
                            " mov word [rsp - 8], dx\n"
                            " mov dword [r10 - 8], r10d\n"
                            " mov qword [rbp - 8], rdx\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_mov_rm(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rbp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RBP, .local.disp = -8};
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};
    X64_emit_instr_mov_rm(&proc0->x64_instrs, 1, X64_RSI, rbp_addr); // rsi aliases dh
    X64_emit_instr_mov_rm(&proc0->x64_instrs, 1, X64_RDX, rbp_addr);
    X64_emit_instr_mov_rm(&proc0->x64_instrs, 2, X64_RDX, rsp_addr);
    X64_emit_instr_mov_rm(&proc0->x64_instrs, 4, X64_R10, r10_addr);
    X64_emit_instr_mov_rm(&proc0->x64_instrs, 8, X64_RDX, rbp_addr);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov sil, byte [rbp - 8]\n"
                            " mov dl, byte [rbp - 8]\n"
                            " mov dx, word [rsp - 8]\n"
                            " mov r10d, dword [r10 - 8]\n"
                            " mov rdx, qword [rbp - 8]\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_mov_mi(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_SIBD_Addr rbp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RBP, .local.disp = -8};
    X64_SIBD_Addr rsp_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.disp = -8};
    X64_SIBD_Addr r10_addr = {.kind = X64_SIBD_ADDR_LOCAL, .local.base_reg = X64_R10, .local.disp = -8};

    // imm can fit in a byte
    X64_emit_instr_mov_mi(&proc0->x64_instrs, 1, rbp_addr, 2);
    X64_emit_instr_mov_mi(&proc0->x64_instrs, 2, rbp_addr, 2);
    X64_emit_instr_mov_mi(&proc0->x64_instrs, 4, rsp_addr, 127);
    X64_emit_instr_mov_mi(&proc0->x64_instrs, 8, r10_addr, -128);

    // imm cannot fit in a byte
    X64_emit_instr_mov_mi(&proc0->x64_instrs, 2, r10_addr, 256);
    X64_emit_instr_mov_mi(&proc0->x64_instrs, 4, rbp_addr, 255);
    X64_emit_instr_mov_mi(&proc0->x64_instrs, 8, rsp_addr, 129);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov byte [rbp - 8], 0x2\n"
                            " mov word [rbp - 8], 0x2\n"
                            " mov dword [rsp - 8], 127\n"
                            " mov qword [r10 - 8], -128\n"
                            " mov word [r10 - 8], 256\n"
                            " mov dword [rbp - 8], 255\n"
                            " mov qword [rsp - 8], 129\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static bool test_mov_ri(Allocator* mem_arena, bool verbose)
{
    Elf_Test_Prog elf_prog = {0};
    init_test_program(mem_arena, &elf_prog);

    Elf_Test_Proc* proc0 = push_test_proc(mem_arena, &elf_prog);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 1, X64_RSI, 1); // rsi aliases dh
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 1, X64_RDX, -1);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 2, X64_RDX, 512);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 2, X64_RDX, -126);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 4, X64_R10, 70000);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 8, X64_RDX, 0x7FFFFFFFFFFFFFFF);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 8, X64_RDX, -1879048192);
    X64_emit_instr_mov_ri(&proc0->x64_instrs, 8, X64_RDX, 0x7FFFFFFF);
    X64_emit_instr_ret(&proc0->x64_instrs);
    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    const char* nasm_code = "proc0:\n"
                            " mov sil, 1\n"
                            " mov dl, -1\n"
                            " mov dx, 512\n"
                            " mov dx, -126\n"
                            " mov r10d, 70000\n"
                            " mov rdx, 0x7FFFFFFFFFFFFFFF\n"
                            " mov rdx, -1879048192\n"
                            " mov rdx, 0x7FFFFFFF\n"
                            " ret\n";
    if (!get_nasm_machine_code(&nasm_buffer, nasm_code, mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static Elf_Gen_Test elf_gen_tests[] = {
    {"test_add_ri_inf_loop", test_add_ri_inf_loop},
    {"test_push_pop_r64", test_push_pop_r64},
    {"test_ret", test_ret},
    {"test_call", test_call},
    {"test_call_r", test_call_r},
    {"test_call_m", test_call_m},
    {"test_sext_ax_to_dx", test_sext_ax_to_dx},
    {"test_div_r", test_div_r},
    {"test_div_m", test_div_m},
    {"test_idiv_r", test_idiv_r},
    {"test_idiv_m", test_idiv_m},
    {"test_mul_r", test_mul_r},
    {"test_mul_m", test_mul_m},
    {"test_imul_r", test_imul_r},
    {"test_imul_m", test_imul_m},
    {"test_imul_rr", test_imul_rr},
    {"test_imul_rm", test_imul_rm},
    {"test_imul_ri", test_imul_ri},
    {"test_movsx_rr", test_movsx_rr},
    {"test_movsx_rm", test_movsx_rm},
    {"test_mov_rr", test_mov_rr},
    {"test_mov_mr", test_mov_mr},
    {"test_mov_rm", test_mov_rm},
    {"test_mov_mi", test_mov_mi},
    {"test_mov_ri", test_mov_ri},
    {"test_and_rr", test_and_rr},
    {"test_and_rm", test_and_rm},
    {"test_and_mr", test_and_mr},
    {"test_and_ri", test_and_ri},
    {"test_and_mi", test_and_mi},
    {"test_or_rr", test_or_rr},
    {"test_or_rm", test_or_rm},
    {"test_or_mr", test_or_mr},
    {"test_or_ri", test_or_ri},
    {"test_or_mi", test_or_mi},
    {"test_xor_rr", test_xor_rr},
    {"test_xor_rm", test_xor_rm},
    {"test_xor_mr", test_xor_mr},
    {"test_xor_ri", test_xor_ri},
    {"test_xor_mi", test_xor_mi},
    {"test_shl_r", test_shl_r},
    {"test_shl_m", test_shl_m},
    {"test_shl_ri", test_shl_ri},
    {"test_shl_mi", test_shl_mi},
    {"test_sar_r", test_sar_r},
    {"test_sar_m", test_sar_m},
    {"test_sar_ri", test_sar_ri},
    {"test_sar_mi", test_sar_mi},
    {"test_neg_r", test_neg_r},
    {"test_neg_m", test_neg_m},
    {"test_not_r", test_not_r},
    {"test_not_m", test_not_m},
    {"test_mov_flt_rr", test_mov_flt_rr},
    {"test_mov_flt_rm", test_mov_flt_rm},
    {"test_mov_flt_mr", test_mov_flt_mr},
    {"test_cvtss2sd_rr", test_cvtss2sd_rr},
    {"test_cvtss2sd_rm", test_cvtss2sd_rm},
    {"test_cvtsd2ss_rr", test_cvtsd2ss_rr},
    {"test_cvtsd2ss_rm", test_cvtsd2ss_rm},
    {"test_cvttsd2si_rr", test_cvttsd2si_rr},
    {"test_cvttss2si_rm", test_cvttss2si_rm},
    {"test_cvttss2si_rr", test_cvttss2si_rr},
    {"test_cvttsd2si_rm", test_cvttsd2si_rm},
};

static void print_usage(FILE* fd, const char* program_name)
{
    ftprint_file(fd, true, "Usage: %s [OPTIONS]\n", program_name);
    ftprint_file(fd, true, "OPTIONS:\n");
    ftprint_file(fd, true, "    -h                              Print this help message and exit\n");
    ftprint_file(fd, true, "    -v                              Enable verbose mode\n");
}

int main(int argc, char** argv)
{
    Argv_Helper argv_helper = {.argc = argc, .argv = argv};
    const char* program_name = get_next_arg(&argv_helper);
    bool verbose = false;

    while (has_next_arg(&argv_helper)) {
        const char* arg = get_next_arg(&argv_helper);

        if (cstr_cmp(arg, "-h") == 0) {
            print_usage(stdout, program_name);
            exit(0);
        }
        else if (cstr_cmp(arg, "-v") == 0) {
            verbose = true;
        }
        else {
            ftprint_err("[ERROR]: unknown option `%s`\n\n", arg);
            print_usage(stderr, program_name);
            exit(1);
        }
    }

    Allocator alloc = allocator_create(16 * 1024);

    size_t num_tests_ok = 0;
    const size_t num_tests = ARRAY_LEN(elf_gen_tests);

    for (size_t i = 0; i < num_tests; i++) {
        ftprint_out("[ RUNNING ] %s\n", elf_gen_tests[i].test_name);
        bool passed = elf_gen_tests[i].test_fn(&alloc, verbose);
        ftprint_out("[ %s ]\n", (passed ? "OK" : "FAILED"));

        num_tests_ok += (size_t)passed;
    }

    const bool all_tests_ok = num_tests == num_tests_ok;

    ftprint_out("%lu/%lu tests passed\n", num_tests_ok, num_tests);
    if (!all_tests_ok) {
        ftprint_out("%lu/%lu tests failed\n", num_tests - num_tests_ok, num_tests);
    }

    allocator_destroy(&alloc);
    return !all_tests_ok;
}
