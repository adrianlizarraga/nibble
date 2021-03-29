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

size_t print(PutCharFunc* put_char, void* arg, const char* format, ...);
size_t print_vlist(PutCharFunc* put_char, void* arg, const char* format, va_list args);

size_t print_file(FILE* fd, const char* format, ...);
size_t print_file_vlist(FILE* fd, const char* format, va_list vargs);
#define print_out(format, ...) print_file(stdout, (format), ## __VA_ARGS__)
#define print_err(format, ...) print_file(stderr, (format), ## __VA_ARGS__)
#endif
