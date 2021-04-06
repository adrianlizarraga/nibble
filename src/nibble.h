#ifndef NIBBLE_H
#define NIBBLE_H
#include <stddef.h>
#include <stdint.h>

#define MAX_ERROR_LEN 256

#define ARRAY_LEN(a) (sizeof(a) / sizeof((a)[0]))

typedef uint32_t ProgPos;

typedef struct ProgRange {
    ProgPos start;
    ProgPos end;
} ProgRange;

typedef struct StringView {
    const char* str;
    size_t len;
} StringView;
#define string_view_lit(cstr_lit) { .str = cstr_lit, .len = sizeof(cstr_lit) - 1 }

typedef enum Keyword {
    KW_VAR = 0,
    KW_CONST,
    KW_ENUM,
    KW_UNION,
    KW_STRUCT,
    KW_PROC,
    KW_TYPEDEF,
    KW_SIZEOF,
    KW_TYPEOF,
    KW_LABEL,
    KW_GOTO,
    KW_BREAK,
    KW_CONTINUE,
    KW_RETURN,
    KW_IF,
    KW_ELSE,
    KW_WHILE,
    KW_DO,
    KW_FOR,
    KW_SWITCH,
    KW_CASE,
    KW_DEFAULT,
    KW_CAST,
    KW_UNDERSCORE,

    KW_COUNT,
} Keyword;

extern const char* keywords[KW_COUNT];
const char* intern_str_lit(const char* str, size_t len);
const char* intern_ident(const char* str, size_t len);
#endif
