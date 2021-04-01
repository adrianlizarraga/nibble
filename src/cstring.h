#ifndef NIBBLE_C_STRING_H
#define NIBBLE_C_STRING_H
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

int    cstr_cmp(const char* str1, const char* str2);
int    cstr_ncmp(const char* str1, const char* str2, size_t num);
size_t cstr_len(const char* str);

extern const unsigned char char_props[256];
extern const unsigned char char_to_biased_digit[256];
extern const char escaped_to_char[256];

#define is_whitespace(c) (char_props[(unsigned char)(c)] & 0x01)
#define is_dec_digit(c) (char_props[(unsigned char)(c)] & 0x02)
#define is_alphanum(c) (char_props[(unsigned char)(c)] & 0x04)

#define unescape_char(c) (escaped_to_char[(unsigned char)(c)])
#define biased_digit(c) (char_to_biased_digit[(unsigned char)(c)])

typedef bool (PutCharFunc)(void* data, char character);

size_t ftprint(PutCharFunc* put_char, void* arg, const char* format, ...);
size_t ftprintv(PutCharFunc* put_char, void* arg, const char* format, va_list args);

size_t ftprint_file(FILE* fd, bool nullterm, const char* format, ...);
size_t ftprintv_file(FILE* fd, bool nullterm, const char* format, va_list vargs);
#define ftprint_out(format, ...) ftprint_file(stdout, true, (format), ## __VA_ARGS__)
#define ftprint_err(format, ...) ftprint_file(stderr, true, (format), ## __VA_ARGS__)
#endif