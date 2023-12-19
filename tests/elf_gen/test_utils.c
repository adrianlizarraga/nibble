#include "test_utils.h"

#include <assert.h>

#include "os_utils.h"
#include "cstring.h"
#include "x64_gen/elf.h"

bool get_elf_text_bytes(Array(u8) * dst, const char* elf_filepath, Allocator* alloc)
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

bool get_nasm_machine_code(Array(u8) * dst, const char* asm_str, Allocator* mem_arena)
{
    const char* asm_file_name = "_test.s";
    const char* obj_file_name = "_test.o";

    FILE* test_asm_fd = fopen(asm_file_name, "w");
    if (!test_asm_fd) {
        ftprint_err("[ERROR]: failed to open %s for writing\n", asm_file_name);
        return false;
    }

    ftprint_file(test_asm_fd, false, asm_str);
    fclose(test_asm_fd);

    const char* nasm_cmd_argv[] = {"nasm", "-f", "elf64", asm_file_name, "-o", obj_file_name, NULL};
    ExecCmd nasm_cmd = {.argv = nasm_cmd_argv, .argc = ARRAY_LEN(nasm_cmd_argv) - 1};
    if (run_cmd(mem_arena, &nasm_cmd, true) != 0) {
        ftprint_err("[ERROR]: Failed to run NASM command\n");
        return false;
    }

    // Get bytes from test.o and compare with our generated buffer
    if (!get_elf_text_bytes(dst, obj_file_name, mem_arena)) {
        ftprint_err("[ERROR]: Failed to get .text bytes from elf file %s\n", obj_file_name);
        return false;
    }

    return true;
}

bool expect_bufs_equal(Array(u8) actual_buf, Array(u8) expected_buf, bool verbose)
{
    const bool are_equal = arrays_equal(actual_buf, expected_buf);

    if (!are_equal && verbose) {
        ftprint_err("Expected bytes: ");
        for (size_t i = 0; i < array_len(expected_buf); i++) {
            ftprint_err("0x%X ", expected_buf[i]);
        }
        ftprint_err("\n");

        ftprint_err("Actual bytes: ");
        for (size_t i = 0; i < array_len(actual_buf); i++) {
            ftprint_err("0x%X ", actual_buf[i]);
        }
        ftprint_err("\n");
    }

    return are_equal;
}
