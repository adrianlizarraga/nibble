#ifndef NIBBLE_X64_MACHINE_CODE_H
#define NIBBLE_X64_MACHINE_CODE_H
#include "basics.h"
#include "allocator.h"
#include "array.h"
#include "x64_gen/x64_instrs.h"

typedef struct X64_TextReloc {
    s64 usage_off; // Location into which to write the symbol's relative offset.
    s64 bytes_to_next_ip; // Offset to the next instruction (typically 4)
    ConstAddr ref_addr; // Entity who's relative offset we need to patch.
} X64_TextReloc;

void X64_elf_gen_instrs(Allocator* tmp_mem, X64_Instrs* instrs, Array(u8) * buffer, Array(X64_TextReloc) * relocs,
                        Array(X64_TextReloc) * proc_off_patches);
#endif
