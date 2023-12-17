#include <stdio.h>
#include "basics.h"
#include "allocator.h"
#include "os_utils.h"
#include "x64_gen/x64_instrs.h"
#include "x64_gen/machine_code.h"
#include "x64_gen/elf.h"

static bool get_elf_text_bytes(Array(u8) * dst, const char* elf_filepath, Allocator* alloc)
{
    StringView file_contents = {0};
    Slurp_File_Err err = slurp_file(&file_contents, alloc, elf_filepath);

    if (err != SLURP_FILE_OK) {
        print_slurp_file_err(elf_filepath, err);
        return false;
    }

    const Elf64_Hdr* elf_hdr = (const Elf64_Hdr*)file_contents.str;
    if (elf_hdr->e_ident[0] != ELF_MAGIC0 || elf_hdr->e_ident[1] != ELF_MAGIC1 || elf_hdr->e_ident[2] != ELF_MAGIC2 ||
        elf_hdr->e_ident[3] != ELF_MAGIC3) {
        ftprint_err("[ERROR]: Invalid elf file magic bytes for %s\n", elf_filepath);
        return false;
    }

    if (elf_hdr->e_ident[4] != ELF_CLASS64 || elf_hdr->e_ident[5] != ELF_DATA_2_LSB || elf_hdr->e_type != ELF_REL_FILE ||
        elf_hdr->e_machine != ELF_MACHINE_X86_64) {
        ftprint_err("[ERROR]: Unsupported elf file %s\n", elf_filepath);
        return false;
    }

    u16 sh_offset = elf_hdr->e_shoff; // File offset to section header table
    u16 shstr_index = elf_hdr->e_shstrndx; // Index of the section header entry that stores section entry names.
    u16 num_section_hdrs = elf_hdr->e_shnum;

    if (shstr_index >= num_section_hdrs) {
        ftprint_err("[ERROR]: Invalid elf file e_shstrndx for %s\n", elf_filepath);
        return false;
    }

    const Elf64_Shdr* section_hdrs = (const Elf64_Shdr*)(&file_contents.str[sh_offset]);
    const Elf64_Shdr* shstr_entry = &section_hdrs[shstr_index];
    const char* sh_name_table = &file_contents.str[shstr_entry->sh_offset];
    u16 text_shdr_index = 0;

    for (u64 i = 0; i < num_section_hdrs; i++) {
        const Elf64_Shdr* shdr = &section_hdrs[i];
        const char* shdr_name = &sh_name_table[shdr->sh_name];

        if (cstr_cmp(shdr_name, ".text") == 0) {
            text_shdr_index = i; // Found .text section header entry
            break;
        }
    }

    if (text_shdr_index == 0 || text_shdr_index >= num_section_hdrs) {
        ftprint_err("[ERROR]: Failed to find .text section header for %s\n", elf_filepath);
        return false;
    }

    const Elf64_Shdr* text_shdr = &section_hdrs[text_shdr_index];
    const u8* text_bytes = (const u8*)&file_contents.str[text_shdr->sh_offset];

    for (u64 i = 0; i < text_shdr->sh_size; i++) {
        array_push(*dst, text_bytes[i]);
    }

    return true;
}

static bool are_bufs_equal(Array(u8) * buf0, Array(u8) * buf1)
{
    if (array_len(*buf0) != array_len(*buf1)) {
        return false;
    }

    const size_t len = array_len(*buf0);

    for (size_t i = 0; i < len; i++) {
        if ((*buf0)[i] != (*buf1)[i]) {
            return false;
        }
    }

    return true;
}

int main(int argc, char** argv)
{
    NIBBLE_UNUSED_VAR(argc);
    NIBBLE_UNUSED_VAR(argv);

    Allocator alloc = allocator_create(512);
    X64_Instrs x64_instrs = {.bblocks = array_create(&alloc, X64_BBlock, 4)};
    array_push(x64_instrs.bblocks, (X64_BBlock){0}); // Push first basic block

    X64_emit_instr_add_ri(&x64_instrs, 4, X64_RAX, 10);
    X64_emit_instr_jmp(&x64_instrs, 0);

    Array(u8) buffer = array_create(&alloc, u8, 64);
    Array(X64_TextReloc) relocs = array_create(&alloc, X64_TextReloc, 2);
    Array(X64_TextReloc) proc_off_patches = array_create(&alloc, X64_TextReloc, 2);

    X64_elf_gen_instrs(&alloc, &x64_instrs, &buffer, &relocs, &proc_off_patches);

    // Print bytes.
    ftprint_out("Instruction bytes:\n");
    for (size_t i = 0; i < array_len(buffer); i++) {
        ftprint_out("0x%X ", buffer[i]);
    }
    ftprint_out("\n");

    FILE* test_asm_fd = fopen("test.s", "w");
    if (!test_asm_fd) {
        ftprint_err("[ERROR]: failed to open test.s for writing\n");
        allocator_destroy(&alloc);
        return 1;
    }

    ftprint_file(test_asm_fd, false, "l0: add eax, 0xA\njmp l0");
    fclose(test_asm_fd);

    const char* nasm_cmd_argv[] = {"nasm", "-f", "elf64", "test.s", "-o", "test.o", NULL};
    ExecCmd nasm_cmd = {.argv = nasm_cmd_argv, .argc = ARRAY_LEN(nasm_cmd_argv) - 1};
    if (run_cmd(&alloc, &nasm_cmd, true) != 0) {
        ftprint_err("[ERROR]: Failed to run NASM command\n");
        allocator_destroy(&alloc);
        return 1;
    }

    // Get bytes from test.o and compare with our generated buffer
    Array(u8) nasm_buffer = array_create(&alloc, u8, 64);
    if (!get_elf_text_bytes(&nasm_buffer, "test.o", &alloc)) {
        ftprint_err("[ERROR]: Failed to get .text bytes from elf file\n");
        allocator_destroy(&alloc);
        return 1;
    }

    // Print nasm bytes.
    ftprint_out("NASM Instruction bytes:\n");
    for (size_t i = 0; i < array_len(nasm_buffer); i++) {
        ftprint_out("0x%X ", nasm_buffer[i]);
    }
    ftprint_out("\n");

    bool pass = are_bufs_equal(&buffer, &nasm_buffer);
    if (pass) {
        ftprint_out("SUCCESS\n");
    }
    else {
        ftprint_out("FAILURE\n");
    }

    allocator_destroy(&alloc);
    return 0;
}
