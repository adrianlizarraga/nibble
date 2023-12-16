#include <stdio.h>
#include "basics.h"
#include "allocator.h"
#include "x64_gen/x64_instrs.h"
#include "x64_gen/machine_code.h"

int main(int argc, char** argv) {
    Allocator alloc = allocator_create(512);
    X64_Instrs x64_instrs = {.bblocks = array_create(&alloc, X64_BBlock, 4)};
    array_push(x64_instrs.bblocks, (X64_BBlock){0}); // Push first basic block

    X64_emit_instr_add_ri(&x64_instrs, 4, X64_RAX, 10);
    X64_emit_instr_jmp(&x64_instrs, 0);
    printf("Num bblocks = %lu, Num instrs = %d\n", array_len(x64_instrs.bblocks), x64_instrs.num_instrs);

    Array(u8) buffer = array_create(&alloc, u8, 64);
    Array(X64_TextReloc) relocs = array_create(&alloc, X64_TextReloc, 2);
    Array(X64_TextReloc) proc_off_patches = array_create(&alloc, X64_TextReloc, 2);

    X64_elf_gen_instrs(&alloc, &x64_instrs, &buffer, &relocs, &proc_off_patches);

    // Print bytes.
    printf("Instruction bytes:\n");
    for (size_t i = 0; i < array_len(buffer); i++) {
        printf("0x%X ", buffer[i]);
    }
    printf("\n");
    allocator_destroy(&alloc);
    return 0;
}
