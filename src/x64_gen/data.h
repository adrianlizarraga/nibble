#ifndef NIBBLE_X64_GEN_DATA_H
#define NIBBLE_X64_GEN_DATA_H

#include "ast/module.h"
#include "compiler.h"
#include "array.h"
#include "hash_map.h"

typedef struct X64_DataReloc {
    ConstAddr ref_addr; // Referenced address of symbol or string that needs to be determined by linker.
    u32 usage_off; // Offset into data section that needs to be patched by linker.
} X64_DataReloc;

typedef struct X64_DataSection {
    Array(char) buf;
    u32 align;
    u64* var_offs; // Symbol::index -> u64 (offset in buf)
    Array(X64_DataReloc) relocs;
} X64_DataSection;

typedef struct X64_RODataSection {
    Array(char) buf;
    u32 align;
    u64* float_offs; // FloatLit::index -> u64 (offset in buf)
    u64* str_offs;   // StrLit::index -> u64 (offset in buf)
} X64_RODataSection;

void X64_init_rodata_section(X64_RODataSection* rodata, Allocator* gen_mem, Allocator* tmp_mem, GlobalData* floats,
                             GlobalData* strs);
void X64_init_data_section(X64_DataSection* data_sec, Allocator* gen_mem, Allocator* tmp_mem, GlobalData* vars);

#endif // defined(NIBBLE_X64_GEN_DATA_H)
