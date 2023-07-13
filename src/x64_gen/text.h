#ifndef NIBBLE_X64_TEXT_H
#define NIBBLE_X64_TEXT_H
#include "nibble.h"

typedef struct X64_TextSection {
    const u8* buf;
    size_t size;
    u32 align;
} X64_TextSection;

void X64_init_text_section(X64_TextSection* text_sec, Allocator* gen_mem, Allocator* tmp_mem, const BucketList* procs);

#endif
