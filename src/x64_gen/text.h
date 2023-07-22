#ifndef NIBBLE_X64_TEXT_H
#define NIBBLE_X64_TEXT_H
#include "nibble.h"
#include "ast/module.h"

typedef struct X64_TextSection {
    const u8* buf;
    size_t size;
    u32 align;
    HMap proc_offs; // Symbol* -> size_t (offset in buf)
} X64_TextSection;

void X64_init_text_section(X64_TextSection* text_sec, Allocator* gen_mem, Allocator* tmp_mem, BucketList* procs, const Symbol* main_proc);

#endif
