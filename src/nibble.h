#ifndef NIBBLE_H
#define NIBBLE_H
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#include "allocator.h"
#include "hash_map.h"
#include "stream.h"

#define MAX_ERROR_LEN 256
#define ARRAY_LEN(a) (sizeof(a) / sizeof((a)[0]))
#define ALIGN_UP(p, a) (((p) + (a) - 1) & ~((a) - 1))
#define BITS(x) (sizeof(x) * 8)

typedef enum OS {
    OS_INVALID,
    OS_LINUX,
    OS_WIN32,
    OS_OSX,
    NUM_OS,
} OS;

typedef enum Arch {
    ARCH_INVALID,
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

typedef uint8_t  u8;
typedef int8_t   s8;
typedef uint16_t u16;
typedef int16_t  s16;
typedef uint32_t u32;
typedef int32_t  s32;
typedef uint64_t u64;
typedef int64_t  s64;
typedef float    f32;
typedef double   f64;

typedef enum FloatKind {
    FLOAT_F64,
    FLOAT_F32,
} FloatKind;

typedef struct Float {
    union {
        f64 _f64;
        f32 _f32;
    };
} Float;

typedef enum IntegerKind {
    INTEGER_U8,
    INTEGER_S8,
    INTEGER_U16,
    INTEGER_S16,
    INTEGER_U32,
    INTEGER_S32,
    INTEGER_U64,
    INTEGER_S64,
} IntegerKind;

typedef struct Integer {
    union {
        u8  _u8;
        s8  _s8;
        u16 _u16;
        s16 _s16;
        u32 _u32;
        s32 _s32;
        u64 _u64;
        s64 _s64;
    };
} Integer;

typedef struct Scalar {
    union {
        Float   as_float;
        Integer as_int;
        void* as_ptr;
    };
} Scalar;

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
    KW_UNDERSCORE,

    KW_COUNT,
} Keyword;

typedef struct TypeCache {
    HMap ptrs;
    HMap arrays;
    HMap procs;
} TypeCache;

extern const char* keywords[KW_COUNT];

const char* intern_str_lit(const char* str, size_t len);
const char* intern_ident(const char* str, size_t len, bool* is_kw, Keyword* kw);

bool nibble_init(OS target_os, Arch target_arch);
void nibble_compile(const char* input_file, const char* output_file);
void nibble_cleanup(void);

#define NIBBLE_FATAL_EXIT(f, ...) nibble_fatal_exit((f), ## __VA_ARGS__)
void nibble_fatal_exit(const char* format, ...);
#endif
