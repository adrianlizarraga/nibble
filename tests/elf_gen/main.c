#include <stdio.h>
#include "x64_gen/xir_to_x64.h"
#include "allocator.h"

int main(int argc, char** argv) {
    Allocator alloc = allocator_create(512);
    X64_Instrs x64_instrs = {.bblocks = array_create(&alloc, X64_BBlock, 4)};
    array_push(x64_instrs.bblocks, (X64_BBlock){0}); // Push first basic block

    X64_emit_instr_add_ri(&x64_instrs, 4, X64_RAX, X64_RCX);

    printf("Num bblocks = %lu, Num instrs = %d\n", array_len(x64_instrs.bblocks), x64_instrs.num_instrs);
    allocator_destroy(&alloc);
    return 0;
}
