#ifndef NIBBLE_H
#define NIBBLE_H
#include <stddef.h>
#include <stdint.h>

#define MAX_ERROR_LEN 256

typedef uint32_t ProgPos;

typedef struct ProgRange {
    ProgPos start;
    ProgPos end;
} ProgRange;

const char* intern_str_lit(const char* str, size_t len);
const char* intern_ident(const char* str, size_t len);
#endif
