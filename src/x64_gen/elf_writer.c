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

#define ELF_NEED_PATCH 0

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

//
// ELF String table
//

typedef struct Elf_StrTable {
    Array(u8) bytes;
} Elf_StrTable;

static void Elf_strtab_init(Elf_StrTable* table, Allocator* arena, u32 cap)
{
    table->bytes = array_create(arena, u8, cap);

    array_push(table->bytes, '\0');
}

static u32 Elf_strtab_add(Elf_StrTable* table, const char* str)
{
    u32 loc = (u32)array_len(table->bytes);

    for (const char* p = str; *p; p += 1) {
        array_push(table->bytes, *p);
    }

    array_push(table->bytes, '\0');

    return loc;
}

static u32 Elf_strtab_size(const Elf_StrTable* table)
{
    return (u32)array_len(table->bytes);
}

//
// Data segment
//

typedef struct X64_DataSegment {
    Array(u8) bytes;
} X64_DataSegment;

/*
$ xxd -g 1 -s $((0x180)) -l $((0x41)) out.o
00000180: 48 31 ed 8b 3c 24 48 8d 74 24 08 48 8d 54 fc 10  H1..<$H.t$.H.T..
00000190: 31 c0 e8 09 00 00 00 89 c7 b8 3c 00 00 00 0f 05  1.........<.....
000001a0: 55 48 89 e5 48 83 ec 10 c7 45 fc 0a 00 00 00 c7  UH..H....E......
000001b0: 45 f8 01 00 00 00 8b 45 fc 03 45 f8 48 89 ec 5d  E......E..E.H..]
000001c0: c3
*/
// Hard-coded for now.
static const u8 text_bin[] = {0x48, 0x31, 0xed, 0x8b, 0x3c, 0x24, 0x48, 0x8d, 0x74, 0x24, 0x08, 0x48, 0x8d, 0x54, 0xfc, 0x10, 0x31,
                              0xc0, 0xe8, 0x09, 0x00, 0x00, 0x00, 0x89, 0xc7, 0xb8, 0x3c, 0x00, 0x00, 0x00, 0x0f, 0x05, 0x55, 0x48,
                              0x89, 0xe5, 0x48, 0x83, 0xec, 0x10, 0xc7, 0x45, 0xfc, 0x0a, 0x00, 0x00, 0x00, 0xc7, 0x45, 0xf8, 0x01,
                              0x00, 0x00, 0x00, 0x8b, 0x45, 0xfc, 0x03, 0x45, 0xf8, 0x48, 0x89, 0xec, 0x5d, 0xc3};

static u32 write_bin(FILE* fd, const void* bin, u32 size, u32 tgt_offset, u32 curr_offset)
{
    u32 offset = curr_offset;

    while (offset < tgt_offset) {
        fputc('\0', fd);
        offset += 1;
    }

    assert(offset == tgt_offset);

    u32 num_written = (u32)fwrite(bin, 1, size, fd);

    if (num_written != size) {
        NIBBLE_FATAL_EXIT("Failed to write elf binary.");
        return offset;
    }

    return offset + size;
}

bool x64_gen_elf(Allocator* gen_mem, Allocator* tmp_mem, BucketList* vars, BucketList* procs, BucketList* str_lits,
                 BucketList* float_lits, BucketList* foreign_procs, const char* output_file)
{
    FILE* out_fd = fopen("elf.o", "wb");
    if (!out_fd) {
        ftprint_err("Failed to open output file `elf.o`\n");
        return false;
    }

    AllocatorState gen_mem_state = allocator_get_state(gen_mem);

    const bool has_data_sec = vars->num_elems > 0;
    const bool has_rodata_sec = (str_lits->num_elems > 0) || (float_lits->num_elems > 0);
    const bool has_rela_sec = false; // TODO: Determined by text section usage of "global" syms

    // NULL, .rodata?, .data?, .text, .shstrtab, .symtab, .strtab, .rela.text?
    const u32 num_shdrs = 5 + has_rodata_sec + has_data_sec + has_rela_sec;

    Elf64_Hdr elf_hdr = {
        .e_ident = {ELF_MAGIC0, ELF_MAGIC1, ELF_MAGIC2, ELF_MAGIC3, ELF_CLASS64, ELF_DATA_2_LSB, 1, ELF_OS_ABI_SYSV},
        .e_type = ELF_REL_FILE,
        .e_machine = ELF_MACHINE_X86_64,
        .e_version = 1,
        .e_entry = 0,
        .e_phoff = 0,
        .e_shoff = sizeof(Elf64_Hdr), // Right after this header
        .e_flags = 0,
        .e_ehsize = sizeof(Elf64_Hdr),
        .e_phentsize = 0,
        .e_phnum = 0,
        .e_shentsize = sizeof(Elf64_Shdr),
        .e_shnum = num_shdrs,
        .e_shstrndx = ELF_NEED_PATCH // Patch to entry for shstrtab after creating section header table.
    };

    const u32 sections_off = elf_hdr.e_ehsize + elf_hdr.e_shnum * elf_hdr.e_shentsize;

    const u32 rodata_off = sections_off;
    const u32 rodata_size = 0; // TODO
    const char* rodata_bin = NULL; // TODO

    const u32 data_off = ALIGN_UP(rodata_off + rodata_size, 16);
    const u32 data_size = 0; // TODO
    const char* data_bin = NULL; // TODO

    const u32 text_off = ALIGN_UP(data_off + data_size, 16);
    const u32 text_size = sizeof(text_bin);

    Elf_StrTable shstrtab = {0};
    Elf_strtab_init(&shstrtab, gen_mem, 38);

    Elf64_Shdr* elf_shdrs = alloc_array(gen_mem, Elf64_Shdr, num_shdrs, true);

    u32 she_idx = 1; // Skip null entry
    u32 rodata_shndx = 0;
    u32 data_shndx = 0;
    u32 text_shndx = 0;
    u32 shstrtab_shndx = 0;
    u32 symtab_shndx = 0;
    u32 strtab_shndx = 0;

    // .rodata section header table entry
    if (has_rodata_sec) {
        rodata_shndx = she_idx++;
        elf_shdrs[rodata_shndx] = (Elf64_Shdr){.sh_name = Elf_strtab_add(&shstrtab, ".rodata"),
                                               .sh_type = ELF_SHT_PROGBITS,
                                               .sh_flags = ELF_SHF_ALLOC,
                                               .sh_addr = 0,
                                               .sh_offset = rodata_off,
                                               .sh_size = rodata_size,
                                               .sh_link = 0,
                                               .sh_info = 0,
                                               .sh_addralign = 0x10,
                                               .sh_entsize = 0};
    }

    // .data section header table entry
    if (has_data_sec) {
        data_shndx = she_idx++;
        elf_shdrs[data_shndx] = (Elf64_Shdr){.sh_name = Elf_strtab_add(&shstrtab, ".data"),
                                             .sh_type = ELF_SHT_PROGBITS,
                                             .sh_flags = ELF_SHF_WRITE | ELF_SHF_ALLOC,
                                             .sh_addr = 0,
                                             .sh_offset = data_off,
                                             .sh_size = data_size,
                                             .sh_link = 0,
                                             .sh_info = 0,
                                             .sh_addralign = 0x10,
                                             .sh_entsize = 0};
    }

    // .text section header table entry
    text_shndx = she_idx++;
    elf_shdrs[text_shndx] = (Elf64_Shdr){.sh_name = Elf_strtab_add(&shstrtab, ".text"),
                                         .sh_type = ELF_SHT_PROGBITS,
                                         .sh_flags = ELF_SHF_ALLOC | ELF_SHF_EXECINSTR,
                                         .sh_addr = 0,
                                         .sh_offset = text_off,
                                         .sh_size = text_size,
                                         .sh_link = 0,
                                         .sh_info = 0,
                                         .sh_addralign = 0x10,
                                         .sh_entsize = 0};

    // .shstrtab section header table entry
    shstrtab_shndx = she_idx++;
    elf_shdrs[shstrtab_shndx] = (Elf64_Shdr){.sh_name = Elf_strtab_add(&shstrtab, ".shstrtab"),
                                             .sh_type = ELF_SHT_STRTAB,
                                             .sh_flags = 0,
                                             .sh_addr = 0,
                                             .sh_offset = ALIGN_UP(text_off + text_size, 16),
                                             .sh_size = ELF_NEED_PATCH, // Patched after we create all section header entries.
                                             .sh_link = 0,
                                             .sh_info = 0,
                                             .sh_addralign = 1,
                                             .sh_entsize = 0};

    // .symtab section header table entry
    symtab_shndx = she_idx++;
    elf_shdrs[symtab_shndx] =
        (Elf64_Shdr){.sh_name = Elf_strtab_add(&shstrtab, ".symtab"),
                     .sh_type = ELF_SHT_SYMTAB,
                     .sh_flags = 0,
                     .sh_addr = 0,
                     .sh_offset = ELF_NEED_PATCH, // Patched after we know location and size of .shstrtab section.
                     .sh_size = ELF_NEED_PATCH, // Patched after adding all symbols to the table.
                     .sh_link = ELF_NEED_PATCH, // Patched to index (in section header table) of associated string table entry.
                     .sh_info = ELF_NEED_PATCH, // Should point to index of the first global symbol.
                     .sh_addralign = 0x8,
                     .sh_entsize = sizeof(Elf64_Sym)};

    // .strtab
    strtab_shndx = she_idx++;
    elf_shdrs[strtab_shndx] = (Elf64_Shdr){.sh_name = Elf_strtab_add(&shstrtab, ".strtab"),
                                           .sh_type = ELF_SHT_STRTAB,
                                           .sh_flags = 0,
                                           .sh_addr = 0,
                                           .sh_offset = ELF_NEED_PATCH, // Patch after placing .symtab (goes last)
                                           .sh_size = ELF_NEED_PATCH, // Patch after adding all symbol names.
                                           .sh_link = 0,
                                           .sh_info = 0,
                                           .sh_addralign = 1,
                                           .sh_entsize = 0};

    if (has_rela_sec) {
        // TODO
        assert(0);
    }

    // Patch elf header to point to the entry for .shstrtab
    elf_hdr.e_shstrndx = shstrtab_shndx;

    // Patch size of shstrtab in the corresponding section header table entry.
    elf_shdrs[shstrtab_shndx].sh_size = Elf_strtab_size(&shstrtab);

    // Patch location of symtab (after .shstrtab)
    elf_shdrs[symtab_shndx].sh_offset = ALIGN_UP(elf_shdrs[shstrtab_shndx].sh_offset + elf_shdrs[shstrtab_shndx].sh_size, 16);

    // Patch link from symtab to strtab
    elf_shdrs[symtab_shndx].sh_link = strtab_shndx;

    // Create string table for symbols.
    Elf_StrTable strtab = {0};
    Elf_strtab_init(&strtab, gen_mem, 16);

    // Add symbols to symtab. Try just adding sections and global/external syms.
    const u32 num_sec_syms = 1 + has_data_sec + has_rodata_sec; // .text + .data? + .rodata?
    const u32 num_syms = 1 + num_sec_syms + 1 + foreign_procs->num_elems; // null <sections> _start <foreign>

    Elf64_Sym* elf_syms = alloc_array(gen_mem, Elf64_Sym, num_syms, true);

    u32 sym_idx = 1;

    // .rodata symbol
    if (has_rodata_sec) {
        elf_syms[sym_idx++] = (Elf64_Sym){.st_name = 0,
                                          .st_info = ELF_ST_INFO(ELF_STB_LOCAL, ELF_STT_SECTION),
                                          .st_other = ELF_STV_DEFAULT,
                                          .st_shndx = rodata_shndx,
                                          .st_value = 0,
                                          .st_size = 0};
    }

    // .data symbol
    if (has_data_sec) {
        elf_syms[sym_idx++] = (Elf64_Sym){.st_name = 0,
                                          .st_info = ELF_ST_INFO(ELF_STB_LOCAL, ELF_STT_SECTION),
                                          .st_other = ELF_STV_DEFAULT,
                                          .st_shndx = data_shndx,
                                          .st_value = 0,
                                          .st_size = 0};
    }

    // .text symbol
    elf_syms[sym_idx++] = (Elf64_Sym){.st_name = 0,
                                      .st_info = ELF_ST_INFO(ELF_STB_LOCAL, ELF_STT_SECTION),
                                      .st_other = ELF_STV_DEFAULT,
                                      .st_shndx = text_shndx,
                                      .st_value = 0,
                                      .st_size = 0};

    const u32 start_sym_idx = sym_idx++; // Will be the first global sym.
    elf_syms[start_sym_idx] = (Elf64_Sym){.st_name = Elf_strtab_add(&strtab, "_start"),
                                          .st_info = ELF_ST_INFO(ELF_STB_GLOBAL, ELF_STT_NOTYPE),
                                          .st_other = ELF_STV_DEFAULT,
                                          .st_shndx = text_shndx,
                                          .st_value = 0,
                                          .st_size = 0};

    // Patch .symtab size in section header table.
    Elf64_Shdr* symtab_she = &elf_shdrs[symtab_shndx];
    symtab_she->sh_size = num_syms * sizeof(Elf64_Sym);
    symtab_she->sh_info = start_sym_idx; // Point to first global sym in table.

    // Patch .strtab offset and size in section header table.
    Elf64_Shdr* strtab_she = &elf_shdrs[strtab_shndx];
    strtab_she->sh_offset = ALIGN_UP(symtab_she->sh_offset + symtab_she->sh_size, 16);
    strtab_she->sh_size = Elf_strtab_size(&strtab);

    // Write elf file.
    u32 curr_file_off = 0;

    curr_file_off = write_bin(out_fd, &elf_hdr, sizeof(Elf64_Hdr), 0, curr_file_off);
    curr_file_off = write_bin(out_fd, elf_shdrs, num_shdrs * sizeof(Elf64_Shdr), elf_hdr.e_shoff, curr_file_off);

    if (has_rodata_sec) {
        curr_file_off =
            write_bin(out_fd, rodata_bin, elf_shdrs[rodata_shndx].sh_size, elf_shdrs[rodata_shndx].sh_offset, curr_file_off);
    }

    if (has_data_sec) {
        curr_file_off = write_bin(out_fd, data_bin, elf_shdrs[data_shndx].sh_size, elf_shdrs[data_shndx].sh_offset, curr_file_off);
    }

    curr_file_off = write_bin(out_fd, text_bin, elf_shdrs[text_shndx].sh_size, elf_shdrs[text_shndx].sh_offset, curr_file_off);
    curr_file_off =
        write_bin(out_fd, shstrtab.bytes, elf_shdrs[shstrtab_shndx].sh_size, elf_shdrs[shstrtab_shndx].sh_offset, curr_file_off);
    curr_file_off = write_bin(out_fd, elf_syms, elf_shdrs[symtab_shndx].sh_size, elf_shdrs[symtab_shndx].sh_offset, curr_file_off);
    curr_file_off = write_bin(out_fd, strtab.bytes, elf_shdrs[strtab_shndx].sh_size, elf_shdrs[strtab_shndx].sh_offset, curr_file_off);

    fclose(out_fd);
    allocator_restore_state(gen_mem_state);
    return true;
}
