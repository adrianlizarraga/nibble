#ifndef NIBBLE_X64_TEXT_H
#define NIBBLE_X64_TEXT_H
#include "basics.h"
#include "ast/module.h"
#include "x64_gen/machine_code.h"

typedef struct X64_TextSection {
    const u8* buf;
    size_t size;
    u32 align;
    u64* proc_offs; // Symbol::index -> u64 (offset in buf)
    Array(X64_TextReloc) relocs;
} X64_TextSection;

void X64_init_text_section(X64_TextSection* text_sec, Allocator* gen_mem, Allocator* tmp_mem, BucketList* procs,
                           const Symbol* main_proc);
#endif
