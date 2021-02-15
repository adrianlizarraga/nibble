#ifndef NIBBLE_STR_H
#define NIBBLE_STR_H
#include <stdbool.h>
#include <stddef.h>

typedef struct String {
    const char* str;
    size_t len;
} String;

bool is_whitespace(char c);
#endif
