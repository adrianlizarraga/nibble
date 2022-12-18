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
    HMap var_offs; // Symbol* -> size_t (offset in buf)
    Array(X64_DataReloc) relocs;
} X64_DataSection;

typedef struct X64_RODataSection {
    Array(char) buf;
    u32 align;
    HMap float_offs; // FloatLit* -> size_t (offset in buf)
    HMap str_offs; // StrLit* -> size_t (offset in buf)
} X64_RODataSection;

void X64_add_reloc_use(Array(X64_DataReloc)* relocs, u64 usage_off, const ConstAddr* ref_addr);
void X64_init_rodata_section(X64_RODataSection* rodata, Allocator* gen_mem, Allocator* tmp_mem, GlobalData* floats,
                             GlobalData* strs);
void X64_init_data_section(X64_DataSection* data_sec, Allocator* gen_mem, Allocator* tmp_mem, GlobalData* vars);

#endif // defined(NIBBLE_X64_GEN_DATA_H)
