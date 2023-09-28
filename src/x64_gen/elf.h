#ifndef NIBBLE_X64_ELF_H
#define NIBBLE_X64_ELF_H
#include "nibble.h"
#include "array.h"

#define ELF_MAGIC0 127
#define ELF_MAGIC1 'E'
#define ELF_MAGIC2 'L'
#define ELF_MAGIC3 'F'
#define ELF_CLASS64 2
#define ELF_DATA_2_LSB 1 // 2's complement, little-endian
#define ELF_OS_ABI_SYSV 0
#define ELF_REL_FILE 1 // Relocatable file
#define ELF_EXEC_FILE 2 // Executable file
#define ELF_MACHINE_X86_64 62

// Values for sh_type
#define ELF_SHT_NULL 0 // Unused section header table entry
#define ELF_SHT_PROGBITS 1 // Program data
#define ELF_SHT_SYMTAB 2 // Symbol table
#define ELF_SHT_STRTAB 3 // String table
#define ELF_SHT_RELA 4 // Relocations with addends

// Values for sh_flags
#define ELF_SHF_WRITE (1 << 0) // Writeable.
#define ELF_SHF_ALLOC (1 << 1) // Occupies execution memory.
#define ELF_SHF_EXECINSTR (1 << 2) // Executable.

// Insert info to symbol's st_info field.
#define ELF_ST_INFO(bind, type) (((bind) << 4) + ((type)&0xF))

// ST_BIND subfield of st_info
#define ELF_STB_LOCAL 0
#define ELF_STB_GLOBAL 1

// ST_TYPE subfield of st_info
#define ELF_STT_NOTYPE 0
#define ELF_STT_OBJECT 1
#define ELF_STT_FUNC 2
#define ELF_STT_SECTION 3
#define ELF_STT_FILE 4

// Symbol visibility
#define ELF_STV_DEFAULT 0

// AMD X86_64 relocation types
#define ELF_R_X86_64_NONE 0 // None
#define ELF_R_X86_64_64 1 // Direct: S + A
#define ELF_R_X86_64_PC32 2 // PC relative: S + A - P
#define ELF_R_X86_64_PLT32 4 // 32-bit PLT address: L + A - P

typedef struct Elf64_Hdr {
    u8 e_ident[16]; // Magic number + stuff
    u16 e_type; // Object file type.
    u16 e_machine; // Architecture.
    u32 e_version; // Object file version.
    u64 e_entry; // Virtual address of entry point.
    u64 e_phoff; // File offset for the program header table.
    u64 e_shoff; // File offset for the section header table.
    u32 e_flags; // Processor-specific flags.
    u16 e_ehsize; // Size of this elf header.
    u16 e_phentsize; // Size of each program header table entry.
    u16 e_phnum; // Number of program header table entries.
    u16 e_shentsize; // Size of each section header table entry.
    u16 e_shnum; // Number of section header table entries.
    u16 e_shstrndx; // Index (in section header table) of the entry for the section header strings.
} Elf64_Hdr;

typedef struct Elf64_Shdr {
    u32 sh_name;
    u32 sh_type;
    u64 sh_flags;
    u64 sh_addr;
    u64 sh_offset;
    u64 sh_size;
    u32 sh_link;
    u32 sh_info;
    u64 sh_addralign;
    u64 sh_entsize;
} Elf64_Shdr;

typedef struct Elf64_Sym {
    u32 st_name;
    u8 st_info;
    u8 st_other;
    u16 st_shndx;
    u64 st_value;
    u64 st_size;
} Elf64_Sym;

typedef struct Elf64_Rela {
    u64 r_offset;
    u64 r_info;
    s64 r_addend;
} Elf64_Rela;

//
// ELF String table
//

typedef struct Elf_StrTable {
    Array(u8) buf;
} Elf_StrTable;

void Elf_strtab_init(Elf_StrTable* table, Allocator* arena, u32 cap);
u32 Elf_strtab_add(Elf_StrTable* table, const char* str);
u32 Elf_strtab_size(const Elf_StrTable* table);

//
// ELF Symbol table
//

typedef struct Elf_SymTable {
    Elf64_Sym* syms;
    u32 num_syms;
    u32 global_start;
} Elf_SymTable;

//
// ELF Relocations table
//

typedef struct Elf_RelaTable {
    Elf64_Rela* relocs;
    u32 num_relocs;
} Elf_RelaTable;

#endif
