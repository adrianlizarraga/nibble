#ifndef NIBBLE_C_STRING_H
#define NIBBLE_C_STRING_H
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>

#include "basics.h"
#include "allocator.h"

typedef enum Slurp_File_Err {
    SLURP_FILE_OK = 0,
    SLURP_FILE_ERR_OPEN,
    SLURP_FILE_ERR_READ,
} Slurp_File_Err;

Slurp_File_Err slurp_file(StringView* contents, Allocator* allocator, const char* filename);
void print_slurp_file_err(const char* filename, Slurp_File_Err err);

int cstr_cmp(const char* str1, const char* str2);
int cstr_ncmp(const char* str1, const char* str2, size_t num);
size_t cstr_len(const char* str);
size_t cstr_nlen(const char* str, size_t max_len);
void cstr_tolower(char* str);
char* cstr_ncpy(char* dst, const char* src, size_t count);
char* cstr_dup(Allocator* allocator, const char* cstr);

char* cstr_escape(Allocator* allocator, const char* str, size_t len, char extra_escape);

extern const unsigned char char_props[256];
extern const unsigned char char_to_biased_digit[256];
extern const char escaped_to_char[256];

#define is_whitespace(c) (char_props[(unsigned char)(c)] & 0x01)
#define is_dec_digit(c) (char_props[(unsigned char)(c)] & 0x02)
#define is_alphanum(c) (char_props[(unsigned char)(c)] & 0x04)
#define is_cntrl(c) (char_props[(unsigned char)(c)] & 0x08)

#define unescape_char(c) (escaped_to_char[(unsigned char)(c)])
#define biased_digit(c) (char_to_biased_digit[(unsigned char)(c)])

#define PRINT_MAX_NUM_DIGITS 32
#define PRINT_DEFAULT_FLOAT_PRECISION 6
#define PRINT_FILE_BUF_SIZE 128

typedef bool(PutCharFunc)(void* data, char character);

size_t ftprint(PutCharFunc* put_char, void* arg, const char* format, ...);
size_t ftprintv(PutCharFunc* put_char, void* arg, const char* format, va_list args);

size_t ftprint_file(FILE* fd, bool nullterm, const char* format, ...);
size_t ftprintv_file(FILE* fd, bool nullterm, const char* format, va_list vargs);
#define ftprint_out(format, ...) ftprint_file(stdout, false, (format), ##__VA_ARGS__)
#define ftprintv_out(format, vargs) ftprintv_file(stdout, false, (format), (vargs))
#define ftprint_err(format, ...) ftprint_file(stderr, false, (format), ##__VA_ARGS__)
#define ftprintv_err(format, vargs) ftprintv_file(stderr, false, (format), (vargs))

void u32_set_bit(u32* mask, u8 bit);
void u32_unset_bit(u32* mask, u8 bit);
bool u32_is_bit_set(u32 mask, u8 bit);

typedef struct BitArray {
    size_t len;
    size_t* bits;
} BitArray;

void bit_arr_init(BitArray* barr, Allocator* arena, size_t len);
void bit_arr_set(BitArray* barr, size_t index, bool val);
bool bit_arr_get(BitArray* barr, size_t index);
void bit_arr_or(BitArray* dst, BitArray* src);
void bit_arr_and(BitArray* dst, BitArray* src);
void bit_arr_and_inv(BitArray* dst, BitArray* src);
void bit_arr_inv(BitArray* barr);
#endif
