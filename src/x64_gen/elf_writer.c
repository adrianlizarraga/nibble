#include "nibble.h"
#include "x64_gen/elf.h"
#include "x64_gen/data.h"
#include "x64_gen/text.h"


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

typedef enum X64_SectionKind {
    X64_ELF_SECTION_RO_DATA,
    X64_ELF_SECTION_DATA,
    X64_ELF_SECTION_TEXT,
    X64_ELF_SECTION_SYMTAB,
    X64_ELF_SECTION_STRTAB,
    X64_ELF_SECTION_RELA_DATA,
    X64_ELF_SECTION_RELA_TEXT,
    X64_ELF_SECTION_SHSTRTAB,
    X64_ELF_SECTION_COUNT
} X64_SectionKind;

typedef struct X64_Section {
    u32 offset;
    X64_SectionKind kind; // TODO: not needed? We always pass in the kind explicitly with elf_prog
    union {
        const X64_RODataSection* rodata;
        const X64_DataSection* data;
        const X64_TextSection* text;
        Elf_SymTable symtab;
        Elf_StrTable strtab;
        Elf_StrTable shstrtab;
        Elf_RelaTable rela_data;
        Elf_RelaTable rela_text;
    };
} X64_Section;

typedef struct X64_ElfProg {
    u32 num_sections;
    X64_Section sections[X64_ELF_SECTION_COUNT];
    u32 sh_indices[X64_ELF_SECTION_COUNT];
    u32 sh_name_locs[X64_ELF_SECTION_COUNT];
} X64_ElfProg;

static inline const void* x64_prog_sec_data(const X64_ElfProg* prog, X64_SectionKind section_kind)
{
    const X64_Section* section = &prog->sections[section_kind];

    switch (section_kind) {
    case X64_ELF_SECTION_RO_DATA: {
        return section->rodata->buf;
    }
    case X64_ELF_SECTION_DATA: {
        return section->data->buf;
    }
    case X64_ELF_SECTION_TEXT: {
        return section->text->buf;
    }
    case X64_ELF_SECTION_SYMTAB: {
        return section->symtab.syms;
    }
    case X64_ELF_SECTION_STRTAB: {
        return section->strtab.buf;
    }
    case X64_ELF_SECTION_RELA_DATA: {
        return section->rela_data.relocs;
    }
    case X64_ELF_SECTION_RELA_TEXT: {
        return section->rela_text.relocs;
    }
    case X64_ELF_SECTION_SHSTRTAB: {
        return section->shstrtab.buf;
    }
    default:
        NIBBLE_FATAL_EXIT("Unhandled X64_SectionKind %d", section_kind);
    }

    return NULL;
}

static inline u32 x64_prog_sec_size(const X64_ElfProg* prog, X64_SectionKind section_kind)
{
    const X64_Section* section = &prog->sections[section_kind];

    switch (section_kind) {
    case X64_ELF_SECTION_RO_DATA: {
        return array_len(section->rodata->buf);
    }
    case X64_ELF_SECTION_DATA: {
        return array_len(section->data->buf);
    }
    case X64_ELF_SECTION_TEXT: {
        return section->text->size;
    }
    case X64_ELF_SECTION_SYMTAB: {
        return section->symtab.num_syms * sizeof(Elf64_Sym);
    }
    case X64_ELF_SECTION_STRTAB: {
        return Elf_strtab_size(&section->strtab);
    }
    case X64_ELF_SECTION_RELA_DATA: {
        return section->rela_data.num_relocs * sizeof(Elf64_Rela);
    }
    case X64_ELF_SECTION_RELA_TEXT: {
        return section->rela_text.num_relocs * sizeof(Elf64_Rela);
    }
    case X64_ELF_SECTION_SHSTRTAB: {
        return Elf_strtab_size(&section->shstrtab);
    }
    default:
        NIBBLE_FATAL_EXIT("Unhandled X64_SectionKind %d", section_kind);
    }

    return 0;
}

static inline u32 x64_prog_sec_align(const X64_ElfProg* prog, X64_SectionKind section_kind)
{
    const X64_Section* section = &prog->sections[section_kind];

    switch (section_kind) {
    case X64_ELF_SECTION_RO_DATA: {
        return section->rodata->align;
    }
    case X64_ELF_SECTION_DATA: {
        return section->data->align;
    }
    case X64_ELF_SECTION_TEXT: {
        return section->text->align;
    }
    case X64_ELF_SECTION_SYMTAB: {
        return 0x8;
    }
    case X64_ELF_SECTION_STRTAB: {
        return 1;
    }
    case X64_ELF_SECTION_RELA_DATA: {
        return 0x8;
    }
    case X64_ELF_SECTION_RELA_TEXT: {
        return 0x8;
    }
    case X64_ELF_SECTION_SHSTRTAB: {
        return 1;
    }
    default:
        NIBBLE_FATAL_EXIT("Unhandled X64_SectionKind %d", section_kind);
    }

    return 0;
}

static inline bool x64_prog_has_sec(const X64_ElfProg* prog, X64_SectionKind section_kind)
{
    return prog->sh_indices[section_kind] > 0;
}

static inline u32 x64_prog_sec_offset(const X64_ElfProg* prog, X64_SectionKind section_kind)
{
    return prog->sections[section_kind].offset;
}

static inline void x64_init_elf_prog(X64_ElfProg* elf_prog, Allocator* gen_mem, const X64_RODataSection* rodata_sec,
                                     const X64_DataSection* data_sec, const X64_TextSection* text_sec)
{
    // Init shstrtab
    Elf_StrTable* shstrtab = &elf_prog->sections[X64_ELF_SECTION_SHSTRTAB].shstrtab;
    Elf_strtab_init(shstrtab, gen_mem, 38);

    const bool has_rodata_sec = array_len(rodata_sec->buf) > 0;
    const bool has_data_sec = array_len(data_sec->buf) > 0;
    const bool has_rela_data_sec = array_len(data_sec->relocs) > 0;
    const bool has_rela_text_sec = array_len(text_sec->relocs) > 0;

    // .rodata?, .data?, .text, .shstrtab, .symtab, .strtab, .rela.text?, .rela.data?
    elf_prog->num_sections = 4 + has_rodata_sec + has_data_sec +  has_rela_data_sec + has_rela_text_sec;

    u32 sh_idx = 1;

#define _ADD_SECTION(k, i, n)                                    \
    do {                                                         \
        elf_prog->sh_indices[k] = i;                             \
        elf_prog->sh_name_locs[k] = Elf_strtab_add(shstrtab, n); \
        elf_prog->sections[k].kind = k;                          \
    } while (0)

    if (has_rodata_sec) {
        _ADD_SECTION(X64_ELF_SECTION_RO_DATA, sh_idx++, ".rodata");
        elf_prog->sections[X64_ELF_SECTION_RO_DATA].rodata = rodata_sec;
    }

    if (has_data_sec) {
        _ADD_SECTION(X64_ELF_SECTION_DATA, sh_idx++, ".data");
        elf_prog->sections[X64_ELF_SECTION_DATA].data = data_sec;
    }

    _ADD_SECTION(X64_ELF_SECTION_TEXT, sh_idx++, ".text");
    elf_prog->sections[X64_ELF_SECTION_TEXT].text = text_sec;

    _ADD_SECTION(X64_ELF_SECTION_SYMTAB, sh_idx++, ".symtab");
    _ADD_SECTION(X64_ELF_SECTION_STRTAB, sh_idx++, ".strtab");

    if (has_rela_data_sec) {
        _ADD_SECTION(X64_ELF_SECTION_RELA_DATA, sh_idx++, ".rela.data");
    }

    if (has_rela_text_sec) {
        _ADD_SECTION(X64_ELF_SECTION_RELA_TEXT, sh_idx++, ".rela.text");
    }

    _ADD_SECTION(X64_ELF_SECTION_SHSTRTAB, sh_idx++, ".shstrtab");

#undef _ADD_SECTION
}

static inline u32 x64_prog_shndx(const X64_ElfProg* prog, X64_SectionKind kind)
{
    return prog->sh_indices[kind];
}

static inline u32 x64_prog_sh_name_loc(const X64_ElfProg* prog, X64_SectionKind kind)
{
    return prog->sh_name_locs[kind];
}

static inline void x64_prog_finalize_offsets(X64_ElfProg* prog, u32 init_offset)
{
    u32 offset = init_offset;

    for (u32 i = 0; i < X64_ELF_SECTION_COUNT; ++i) {
        X64_Section* section = &prog->sections[i];
        u32 index = prog->sh_indices[i];

        if (index > 0) {
            section->offset = ALIGN_UP(offset, x64_prog_sec_align(prog, (X64_SectionKind)i));
            offset = section->offset + x64_prog_sec_size(prog, (X64_SectionKind)i);
        }
    }
}

static inline void x64_prog_sec_fill_shdr(const X64_ElfProg* prog, X64_SectionKind section_kind, Elf64_Shdr* shdr)
{
    const u32 offset = x64_prog_sec_offset(prog, section_kind);
    const u32 size = x64_prog_sec_size(prog, section_kind);
    const u32 align = x64_prog_sec_align(prog, section_kind);
    const u32 name_loc = x64_prog_sh_name_loc(prog, section_kind);

    const X64_Section* section = &prog->sections[section_kind];

    switch (section_kind) {
    case X64_ELF_SECTION_RO_DATA: {
        *shdr = (Elf64_Shdr){.sh_name = name_loc,
                             .sh_type = ELF_SHT_PROGBITS,
                             .sh_flags = ELF_SHF_ALLOC,
                             .sh_addr = 0,
                             .sh_offset = offset,
                             .sh_size = size,
                             .sh_link = 0,
                             .sh_info = 0,
                             .sh_addralign = align,
                             .sh_entsize = 0};
        break;
    }
    case X64_ELF_SECTION_DATA: {
        *shdr = (Elf64_Shdr){.sh_name = name_loc,
                             .sh_type = ELF_SHT_PROGBITS,
                             .sh_flags = ELF_SHF_WRITE | ELF_SHF_ALLOC,
                             .sh_addr = 0,
                             .sh_offset = offset,
                             .sh_size = size,
                             .sh_link = 0,
                             .sh_info = 0,
                             .sh_addralign = align,
                             .sh_entsize = 0};
        break;
    }
    case X64_ELF_SECTION_TEXT: {
        *shdr = (Elf64_Shdr){.sh_name = name_loc,
                             .sh_type = ELF_SHT_PROGBITS,
                             .sh_flags = ELF_SHF_ALLOC | ELF_SHF_EXECINSTR,
                             .sh_addr = 0,
                             .sh_offset = offset,
                             .sh_size = size,
                             .sh_link = 0,
                             .sh_info = 0,
                             .sh_addralign = align,
                             .sh_entsize = 0};
        break;
    }
    case X64_ELF_SECTION_SYMTAB: {
        const Elf_SymTable* symtab = &section->symtab;

        *shdr = (Elf64_Shdr){.sh_name = name_loc,
                             .sh_type = ELF_SHT_SYMTAB,
                             .sh_flags = 0,
                             .sh_addr = 0,
                             .sh_offset = offset,
                             .sh_size = size,
                             .sh_link = x64_prog_shndx(prog, X64_ELF_SECTION_STRTAB), // Index of associated string table entry.
                             .sh_info = symtab->global_start, // Should point to index of the first global symbol.
                             .sh_addralign = align,
                             .sh_entsize = sizeof(Elf64_Sym)};
        break;
    }
    case X64_ELF_SECTION_STRTAB: {
        *shdr = (Elf64_Shdr){.sh_name = name_loc,
                             .sh_type = ELF_SHT_STRTAB,
                             .sh_flags = 0,
                             .sh_addr = 0,
                             .sh_offset = offset,
                             .sh_size = size,
                             .sh_link = 0,
                             .sh_info = 0,
                             .sh_addralign = align,
                             .sh_entsize = 0};
        break;
    }
    case X64_ELF_SECTION_RELA_DATA: {
        *shdr = (Elf64_Shdr){
            .sh_name = name_loc,
            .sh_type = ELF_SHT_RELA,
            .sh_flags = 0,
            .sh_addr = 0,
            .sh_offset = offset,
            .sh_size = size,
            .sh_link = x64_prog_shndx(prog, X64_ELF_SECTION_SYMTAB), // Index of symbol table entry that contains referenced symbol.
            .sh_info = x64_prog_shndx(prog, X64_ELF_SECTION_DATA), // Index of section that needs relocation patch (.data here)
            .sh_addralign = align,
            .sh_entsize = sizeof(Elf64_Rela)};
        break;
    }
    case X64_ELF_SECTION_RELA_TEXT: {
        *shdr = (Elf64_Shdr){
            .sh_name = name_loc,
            .sh_type = ELF_SHT_RELA,
            .sh_flags = 0,
            .sh_addr = 0,
            .sh_offset = offset,
            .sh_size = size,
            .sh_link = x64_prog_shndx(prog, X64_ELF_SECTION_SYMTAB), // Index of symbol table entry that contains referenced symbol.
            .sh_info = x64_prog_shndx(prog, X64_ELF_SECTION_TEXT), // Index of section that needs relocation patch (.text here)
            .sh_addralign = align,
            .sh_entsize = sizeof(Elf64_Rela)};
        break;
    }
    case X64_ELF_SECTION_SHSTRTAB: {
        *shdr = (Elf64_Shdr){.sh_name = name_loc,
                             .sh_type = ELF_SHT_STRTAB,
                             .sh_flags = 0,
                             .sh_addr = 0,
                             .sh_offset = offset,
                             .sh_size = size,
                             .sh_link = 0,
                             .sh_info = 0,
                             .sh_addralign = align,
                             .sh_entsize = 0};
        break;
    }
    default:
        NIBBLE_FATAL_EXIT("Unhandled X64_SectionKind %d", section_kind);
    }
}

static void x64_prog_fill_shdrs(const X64_ElfProg* prog, Elf64_Shdr* elf_shdrs)
{
    for (u32 i = 0; i < X64_ELF_SECTION_COUNT; ++i) {
        u32 index = prog->sh_indices[i];

        if (index > 0) {
            x64_prog_sec_fill_shdr(prog, (X64_SectionKind)i, &elf_shdrs[index]);
        }
    }
}

static void x64_prog_write_sections(const X64_ElfProg* prog, FILE* out_fd, u32 init_file_offset)
{
    u32 curr_file_off = init_file_offset;

    for (u32 i = 0; i < X64_ELF_SECTION_COUNT; ++i) {
        X64_SectionKind kind = i;
        u32 index = prog->sh_indices[kind];

        if (index > 0) {
            const void* data = x64_prog_sec_data(prog, kind);
            const u32 size = x64_prog_sec_size(prog, kind);
            const u32 offset = x64_prog_sec_offset(prog, kind);

            curr_file_off = write_bin(out_fd, data, size, offset, curr_file_off);
        }
    }
}

static bool x64_write_elf(Allocator* gen_mem, Allocator* tmp_mem, const X64_RODataSection* rodata_sec, const X64_DataSection* data_sec,
                          const X64_TextSection* text_sec, const BucketList* foreign_procs, const char* output_file)
{
    // TODO: Actually use vars.
    NIBBLE_UNUSED_VAR(tmp_mem);
    NIBBLE_UNUSED_VAR(output_file);

    FILE* out_fd = fopen("elf.o", "wb");
    if (!out_fd) {
        ftprint_err("Failed to open output file `elf.o`\n");
        return false;
    }

    X64_ElfProg elf_prog = {0};
    x64_init_elf_prog(&elf_prog, gen_mem, rodata_sec, data_sec, text_sec);

    const bool has_rodata_sec = x64_prog_has_sec(&elf_prog, X64_ELF_SECTION_RO_DATA);
    const bool has_data_sec = x64_prog_has_sec(&elf_prog, X64_ELF_SECTION_DATA);
    const bool has_rela_data_sec = x64_prog_has_sec(&elf_prog, X64_ELF_SECTION_RELA_DATA);
    const bool has_rela_text_sec = x64_prog_has_sec(&elf_prog, X64_ELF_SECTION_RELA_TEXT);

    // .strtab
    X64_Section* strtab = &elf_prog.sections[X64_ELF_SECTION_STRTAB];
    Elf_strtab_init(&strtab->strtab, gen_mem, 16);

    // .symtab
    // Add symbols to symtab. Try just adding sections and global/external syms.
    const u32 num_sec_syms = 1 + has_data_sec + has_rodata_sec; // .text + .data? + .rodata?
    const u32 num_syms = 1 + num_sec_syms + 1 + foreign_procs->num_elems; // null <sections> _start <foreign>

    X64_Section* symtab = &elf_prog.sections[X64_ELF_SECTION_SYMTAB];
    symtab->symtab.num_syms = num_syms;
    symtab->symtab.syms = alloc_array(gen_mem, Elf64_Sym, symtab->symtab.num_syms, true);

    u32 sym_idx = 1;
    u32 rodata_sym_idx = 0;
    u32 data_sym_idx = 0;

    // .rodata symbol
    if (has_rodata_sec) {
        rodata_sym_idx = sym_idx;
        symtab->symtab.syms[sym_idx++] = (Elf64_Sym){.st_name = 0,
                                                     .st_info = ELF_ST_INFO(ELF_STB_LOCAL, ELF_STT_SECTION),
                                                     .st_other = ELF_STV_DEFAULT,
                                                     .st_shndx = x64_prog_shndx(&elf_prog, X64_ELF_SECTION_RO_DATA),
                                                     .st_value = 0,
                                                     .st_size = 0};
    }

    // .data symbol
    if (has_data_sec) {
        data_sym_idx = sym_idx;
        symtab->symtab.syms[sym_idx++] = (Elf64_Sym){.st_name = 0,
                                                     .st_info = ELF_ST_INFO(ELF_STB_LOCAL, ELF_STT_SECTION),
                                                     .st_other = ELF_STV_DEFAULT,
                                                     .st_shndx = x64_prog_shndx(&elf_prog, X64_ELF_SECTION_DATA),
                                                     .st_value = 0,
                                                     .st_size = 0};
    }

    // .text symbol
    u32 text_sym_idx = sym_idx;
    symtab->symtab.syms[sym_idx++] = (Elf64_Sym){.st_name = 0,
                                                 .st_info = ELF_ST_INFO(ELF_STB_LOCAL, ELF_STT_SECTION),
                                                 .st_other = ELF_STV_DEFAULT,
                                                 .st_shndx = x64_prog_shndx(&elf_prog, X64_ELF_SECTION_TEXT),
                                                 .st_value = 0,
                                                 .st_size = 0};

    symtab->symtab.global_start = sym_idx; // First global symbol.
    symtab->symtab.syms[sym_idx++] = (Elf64_Sym){.st_name = Elf_strtab_add(&strtab->strtab, "_start"),
                                                 .st_info = ELF_ST_INFO(ELF_STB_GLOBAL, ELF_STT_NOTYPE),
                                                 .st_other = ELF_STV_DEFAULT,
                                                 .st_shndx = x64_prog_shndx(&elf_prog, X64_ELF_SECTION_TEXT),
                                                 .st_value = 0,
                                                 .st_size = 0};

    // Add foreign procs as symbols.
    const u32 num_foreign_procs = foreign_procs->num_elems;
    HMap foreign_proc_sym_idxs = hmap(clp2(num_foreign_procs), gen_mem);

    for (u32 i = 0; i < num_foreign_procs; i += 1) {
        Symbol* proc_sym = (Symbol*)(*bucket_list_get_elem_packed(foreign_procs, i));

        hmap_put(&foreign_proc_sym_idxs, PTR_UINT(proc_sym), sym_idx); // Track the symbol index for each foreign proc
        symtab->symtab.syms[sym_idx++] = (Elf64_Sym){.st_name = Elf_strtab_add(&strtab->strtab,
                                                                               proc_sym->as_proc.foreign_name->str),
                                                     .st_info = ELF_ST_INFO(ELF_STB_GLOBAL, ELF_STT_NOTYPE),
                                                     .st_other = ELF_STV_DEFAULT,
                                                     .st_shndx = 0,
                                                     .st_value = 0,
                                                     .st_size = 0};
    }

    // .rela.data
    if (has_rela_data_sec) {
        Array(const X64_DataReloc) relocs_info = data_sec->relocs;
        const u32 num_relocs = array_len(relocs_info);

        X64_Section* rela_data = &elf_prog.sections[X64_ELF_SECTION_RELA_DATA];
        rela_data->rela_data.num_relocs = num_relocs;
        rela_data->rela_data.relocs = alloc_array(gen_mem, Elf64_Rela, num_relocs, true);

        for (size_t i = 0; i < num_relocs; ++i) {
            const X64_DataReloc* reloc_info = &relocs_info[i];
            Elf64_Rela* reloc = &rela_data->rela_data.relocs[i];

            reloc->r_offset = reloc_info->usage_off;

            if (reloc_info->ref_addr.kind == CONST_ADDR_STR_LIT) {
                const StrLit* str_lit = reloc_info->ref_addr.str_lit;
                u64* str_off_ptr = hmap_get(&rodata_sec->str_offs, PTR_UINT(str_lit));

                assert(str_off_ptr != NULL);

                reloc->r_info = ((u64)(rodata_sym_idx) << 32) + (u64)(ELF_R_X86_64_64);
                reloc->r_addend = *str_off_ptr + reloc_info->ref_addr.disp;
            }
            else if (reloc_info->ref_addr.kind == CONST_ADDR_SYM) {
                const Symbol* sym = reloc_info->ref_addr.sym;

                if (sym->kind == SYMBOL_VAR) {
                    u64* sym_off_ptr = hmap_get(&data_sec->var_offs, PTR_UINT(sym));
                    assert(sym_off_ptr != NULL);

                    reloc->r_info = ((u64)(data_sym_idx) << 32) + (u64)(ELF_R_X86_64_64);
                    reloc->r_addend = *sym_off_ptr + reloc_info->ref_addr.disp;
                }
                else {
                    assert(sym->kind == SYMBOL_PROC);
                    u64* sym_off_ptr = hmap_get(&text_sec->proc_offs, PTR_UINT(sym));
                    assert(sym_off_ptr != NULL);

                    reloc->r_info = ((u64)(text_sym_idx) << 32) + (u64)(ELF_R_X86_64_64);
                    reloc->r_addend = *sym_off_ptr;
                    assert(reloc_info->ref_addr.disp == 0);
                }
            }
            else {
                NIBBLE_FATAL_EXIT("Only support string lits and vars for rela.data relocations");
            }
        }
    }

    // .rela.text
    if (has_rela_text_sec) {
        Array(const X64_SymRelOffPatch) relocs_info = text_sec->relocs;
        const u32 num_relocs = array_len(relocs_info);

        X64_Section* rela_text = &elf_prog.sections[X64_ELF_SECTION_RELA_TEXT];
        rela_text->rela_text.num_relocs = num_relocs;
        rela_text->rela_text.relocs = alloc_array(gen_mem, Elf64_Rela, num_relocs, true);

        for (size_t i = 0; i < num_relocs; ++i) {
            const X64_SymRelOffPatch* reloc_info = &relocs_info[i];
            Elf64_Rela* reloc = &rela_text->rela_text.relocs[i];

            if (reloc_info->sym->kind == SYMBOL_VAR) { // Global var used in code.
                u64* sym_off_ptr = hmap_get(&data_sec->var_offs, PTR_UINT(reloc_info->sym));
                assert(sym_off_ptr != NULL);

                reloc->r_offset = reloc_info->buffer_loc;
                reloc->r_info = ((u64)(data_sym_idx) << 32) + (u64)(ELF_R_X86_64_PC32);
                reloc->r_addend = *sym_off_ptr - reloc_info->bytes_to_next_ip;
            }
            else if (reloc_info->sym->kind == SYMBOL_PROC) { // Foreign procedure used in code.
                u64* foreign_proc_sym_idx_ptr = hmap_get(&foreign_proc_sym_idxs, PTR_UINT(reloc_info->sym));
                assert(foreign_proc_sym_idx_ptr != NULL);

                reloc->r_offset = reloc_info->buffer_loc;
                reloc->r_info = (*foreign_proc_sym_idx_ptr << 32) + (u64)(ELF_R_X86_64_PLT32);
                reloc->r_addend = -(reloc_info->bytes_to_next_ip);
            }
        }
    }

    const u32 num_shdrs = 1 + elf_prog.num_sections; // Account for NULL section header

    Elf64_Hdr elf_hdr = {.e_ident = {ELF_MAGIC0, ELF_MAGIC1, ELF_MAGIC2, ELF_MAGIC3, ELF_CLASS64, ELF_DATA_2_LSB, 1, ELF_OS_ABI_SYSV},
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
                         .e_shstrndx = x64_prog_shndx(&elf_prog, X64_ELF_SECTION_SHSTRTAB)};

    const u32 sections_init_off = elf_hdr.e_ehsize + elf_hdr.e_shnum * elf_hdr.e_shentsize;
    x64_prog_finalize_offsets(&elf_prog, sections_init_off);

    Elf64_Shdr* elf_shdrs = alloc_array(gen_mem, Elf64_Shdr, num_shdrs, true);
    x64_prog_fill_shdrs(&elf_prog, elf_shdrs);

    // Write elf file.
    u32 curr_file_off = 0;

    curr_file_off = write_bin(out_fd, &elf_hdr, sizeof(Elf64_Hdr), 0, curr_file_off);
    curr_file_off = write_bin(out_fd, elf_shdrs, num_shdrs * sizeof(Elf64_Shdr), elf_hdr.e_shoff, curr_file_off);

    x64_prog_write_sections(&elf_prog, out_fd, curr_file_off);

    fclose(out_fd);
    return true;
}

bool x64_gen_elf(Allocator* gen_mem, Allocator* tmp_mem, GlobalData* vars, BucketList* procs, const Symbol* main_proc, GlobalData* str_lits,
                 GlobalData* float_lits, BucketList* foreign_procs, const char* output_file)
{
    AllocatorState gen_mem_state = allocator_get_state(gen_mem);
    AllocatorState tmp_mem_state = allocator_get_state(tmp_mem);

    const bool has_rodata_sec = (str_lits->list.num_elems > 0) || (float_lits->list.num_elems > 0);
    const bool has_data_sec = vars->list.num_elems > 0;

    // .rodata
    X64_RODataSection rodata_sec = {0};
    if (has_rodata_sec) {
        X64_init_rodata_section(&rodata_sec, gen_mem, tmp_mem, float_lits, str_lits);
    }

    // .data
    X64_DataSection data_sec = {0};
    if (has_data_sec) {
        X64_init_data_section(&data_sec, gen_mem, tmp_mem, vars);
    }

    // .text
    X64_TextSection text_sec = {0};
    X64_init_text_section(&text_sec, gen_mem, tmp_mem, procs, main_proc);

    bool success = x64_write_elf(gen_mem, tmp_mem, &rodata_sec, &data_sec, &text_sec, foreign_procs, output_file);

    allocator_restore_state(tmp_mem_state);
    allocator_restore_state(gen_mem_state);

    return success;
}

