#ifndef NIBBLE_X64_TEXT_H
#define NIBBLE_X64_TEXT_H
#include "nibble.h"
#include "ast/module.h"

typedef struct X64_TextReloc {
    s64 usage_off; // Location into which to write the symbol's relative offset.
    s64 bytes_to_next_ip; // Offset to the next instruction (typically 4)
    ConstAddr ref_addr; // Entity who's relative offset we need to patch.
} X64_TextReloc;

typedef struct X64_TextSection {
    const u8* buf;
    size_t size;
    u32 align;
    u64* proc_offs; // Symbol::index -> u64 (offset in buf)
    Array(X64_TextReloc) relocs;
} X64_TextSection;

void X64_init_text_section(X64_TextSection* text_sec, Allocator* gen_mem, Allocator* tmp_mem, BucketList* procs, const Symbol* main_proc);

#endif
