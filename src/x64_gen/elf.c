#include "x64_gen/elf.h"
#include "array.h"


void Elf_strtab_init(Elf_StrTable* table, Allocator* arena, u32 cap)
{
    table->buf = array_create(arena, u8, cap);

    array_push(table->buf, '\0');
}

u32 Elf_strtab_add(Elf_StrTable* table, const char* str)
{
    u32 loc = (u32)array_len(table->buf);

    for (const char* p = str; *p; p += 1) {
        array_push(table->buf, *p);
    }

    array_push(table->buf, '\0');

    return loc;
}

u32 Elf_strtab_size(const Elf_StrTable* table)
{
    return (u32)array_len(table->buf);
}

