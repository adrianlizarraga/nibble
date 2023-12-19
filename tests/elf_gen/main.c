#include <stdio.h>
#include "basics.h"
#include "allocator.h"
#include "x64_gen/x64_instrs.h"
#include "x64_gen/machine_code.h"

#include "test_utils.h"

static bool test_add_ri_inf_loop(Allocator* mem_arena, bool verbose)
{
    X64_Instrs x64_instrs = {.bblocks = array_create(mem_arena, X64_BBlock, 4)};
    array_push(x64_instrs.bblocks, (X64_BBlock){0}); // Push first basic block

    X64_emit_instr_add_ri(&x64_instrs, 4, X64_RAX, 10);
    X64_emit_instr_jmp(&x64_instrs, 0);

    Array(u8) buffer = array_create(mem_arena, u8, 64);
    Array(X64_TextReloc) relocs = array_create(mem_arena, X64_TextReloc, 2);
    Array(X64_TextReloc) proc_off_patches = array_create(mem_arena, X64_TextReloc, 2);

    X64_elf_gen_instrs(mem_arena, &x64_instrs, &buffer, &relocs, &proc_off_patches);

    Array(u8) nasm_buffer = array_create(mem_arena, u8, 64);
    if (!get_nasm_machine_code(&nasm_buffer, "l0: add eax, 0xA\njmp l0", mem_arena)) {
        return false;
    }

    return expect_bufs_equal(buffer, nasm_buffer, verbose);
}

static Elf_Gen_Test elf_gen_tests[] = {
    {"test_add_ri_inf_loop", test_add_ri_inf_loop},
};

int main(int argc, char** argv)
{
    NIBBLE_UNUSED_VAR(argc);
    NIBBLE_UNUSED_VAR(argv);

    bool verbose = true; // TODO: Get from command-line args

    Allocator alloc = allocator_create(512);

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
