#include <stdio.h>
#include "basics.h"
#include "allocator.h"
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
    X64_elf_gen_instrs(mem_arena, &proc1->x64_instrs, &elf_prog.buffer, &elf_prog.relocs, &elf_prog.proc_off_patches);

    X64_patch_proc_uses(elf_prog.buffer, elf_prog.proc_off_patches, elf_prog.proc_offsets);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    if (!get_nasm_machine_code(&nasm_buffer, "proc0:\nmov rax, 10\nret\n\nproc1:\ncall proc0\n", mem_arena)) {
        return false;
    }

    return expect_bufs_equal(elf_prog.buffer, nasm_buffer, verbose);
}

static Elf_Gen_Test elf_gen_tests[] = {
    {"test_add_ri_inf_loop", test_add_ri_inf_loop},
    {"test_push_pop_r64", test_push_pop_r64},
    {"test_ret", test_ret},
    {"test_call", test_call},
};

int main(int argc, char** argv)
{
    NIBBLE_UNUSED_VAR(argc);
    NIBBLE_UNUSED_VAR(argv);

    bool verbose = false; // TODO: Get from command-line args

    Allocator alloc = allocator_create(16*1024);

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
