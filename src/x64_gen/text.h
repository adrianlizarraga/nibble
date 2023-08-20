#ifndef NIBBLE_X64_TEXT_H
#define NIBBLE_X64_TEXT_H
#include "nibble.h"
#include "ast/module.h"

typedef struct X64_SymRelOffPatch {
    s64 buffer_loc; // Location into which to write the symbol's relative offset.
    s64 bytes_to_next_ip; // Offset to the next instruction (typically 4)
    const Symbol* sym; // Symbol who's relative offset we need to patch.
} X64_SymRelOffPatch;

typedef struct X64_TextSection {
    const u8* buf;
    size_t size;
    u32 align;
    HMap proc_offs; // Symbol* -> size_t (offset in buf)
    Array(X64_SymRelOffPatch) relocs;
} X64_TextSection;

void X64_init_text_section(X64_TextSection* text_sec, Allocator* gen_mem, Allocator* tmp_mem, BucketList* procs, const Symbol* main_proc);

#endif
