#include <stdio.h>
#include "basics.h"
#include "allocator.h"
#include "os_utils.h"
#include "x64_gen/x64_instrs.h"
#include "x64_gen/machine_code.h"

int main(int argc, char** argv) {
    Allocator alloc = allocator_create(512);
    X64_Instrs x64_instrs = {.bblocks = array_create(&alloc, X64_BBlock, 4)};
    array_push(x64_instrs.bblocks, (X64_BBlock){0}); // Push first basic block

    X64_emit_instr_add_ri(&x64_instrs, 4, X64_RAX, 10);
    X64_emit_instr_jmp(&x64_instrs, 0);
    ftprint_out("Num bblocks = %lu, Num instrs = %d\n", array_len(x64_instrs.bblocks), x64_instrs.num_instrs);

    Array(u8) buffer = array_create(&alloc, u8, 64);
    Array(X64_TextReloc) relocs = array_create(&alloc, X64_TextReloc, 2);
    Array(X64_TextReloc) proc_off_patches = array_create(&alloc, X64_TextReloc, 2);

    X64_elf_gen_instrs(&alloc, &x64_instrs, &buffer, &relocs, &proc_off_patches);

    // Print bytes.
    ftprint_out("Instruction bytes:\n");
    for (size_t i = 0; i < array_len(buffer); i++) {
        ftprint_out("0x%X ", buffer[i]);
    }
    ftprint_out("\n");

    FILE* test_asm_fd = fopen("test.s", "w");
    if (!test_asm_fd) {
        ftprint_out("[ERROR]: failed to open test.s for writing\n");
        allocator_destroy(&alloc);
        return 1;
    }

    ftprint_file(test_asm_fd, false, "l0: add eax, 0xA\njmp l0");
    fclose(test_asm_fd);

    const char* nasm_cmd_argv[] = {"nasm", "-f", "elf64", "test.s", "-o", "test.o", NULL};
    ExecCmd nasm_cmd = {.argv = nasm_cmd_argv, .argc = ARRAY_LEN(nasm_cmd_argv) - 1};
    if (run_cmd(&alloc, &nasm_cmd, false) != 0) {
        ftprint_out("[ERROR]: Failed to run NASM command\n");
        allocator_destroy(&alloc);
        return 1;
    }

    // TODO: Get bytes from test.o and compare with our generated buffer

    allocator_destroy(&alloc);
    return 0;
}
