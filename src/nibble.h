#ifndef NIBBLE_H
#define NIBBLE_H
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#include "allocator.h"
#include "stream.h"

#define MAX_ERROR_LEN 256
#define ARRAY_LEN(a) (sizeof(a) / sizeof((a)[0]))
#define ALIGN_UP(p, a) (((p) + (a) - 1) & ~((a) - 1))

typedef enum OS {
    OS_LINUX,
    OS_WIN32,
    OS_OSX,
    NUM_OS,
} OS;

typedef enum Arch {
    ARCH_X64,
    ARCH_X86,
    NUM_ARCH,
} Arch;

extern const char* os_names[NUM_OS];
extern const char* arch_names[NUM_ARCH];

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
    KW_ELIF,
    KW_WHILE,
    KW_DO,
    KW_FOR,
    KW_SWITCH,
    KW_CASE,
    KW_UNDERSCORE,

    KW_COUNT,
} Keyword;

typedef struct CompiledModule {
    Allocator allocator;
    Allocator ast_arena;
    ByteStream errors;
} CompiledModule;

extern const char* keywords[KW_COUNT];

const char* intern_str_lit(const char* str, size_t len);
const char* intern_ident(const char* str, size_t len, bool* is_kw, Keyword* kw);

bool nibble_init(OS target_os, Arch target_arch);
void nibble_cleanup(void);

CompiledModule* compile_module(const char* filename, ProgPos pos);
void free_compiled_module(CompiledModule* module);
#endif
