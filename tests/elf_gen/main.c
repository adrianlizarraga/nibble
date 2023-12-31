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
    X64_emit_instr_add_ri(&proc0->x64_instrs, 4, X64_RAX, 10);
    X64_emit_instr_jmp(&proc0->x64_instrs, 0);

    X64_elf_gen_instrs(mem_arena, &proc0->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    if (!get_nasm_machine_code(&nasm_buffer, "proc0:\nl0: add eax, 0xA\njmp l0", mem_arena)) {
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
        ftprint_out("[ %s ] %s\n\n", (passed ? "OK" : "FAILED"), elf_gen_tests[i].test_name);

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
