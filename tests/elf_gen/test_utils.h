#ifndef NIBBLE_TESTS_ELF_GEN_TEST_UTILS_H
#define NIBBLE_TESTS_ELF_GEN_TEST_UTILS_H
#include "basics.h"
#include "allocator.h"
#include "array.h"

typedef bool(elf_gen_test_fn)(Allocator* mem_arena, bool verbose);

typedef struct {
    const char* test_name;
    elf_gen_test_fn* test_fn;
} Elf_Gen_Test;

bool get_elf_text_bytes(Array(u8) * dst, const char* elf_filepath, Allocator* alloc);
bool get_nasm_machine_code(Array(u8) * dst, const char* asm_str, Allocator* mem_arena);
bool expect_bufs_equal(Array(u8) actual_buf, Array(u8) expected_buf, bool verbose);

#endif
