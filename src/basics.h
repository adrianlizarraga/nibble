#ifndef BASICS_H
#define BASICS_H
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#define MAX_ERROR_LEN 256
#define ARRAY_LEN(a) (sizeof(a) / sizeof((a)[0]))
#define ALIGN_UP(p, a) (((p) + (a)-1) & ~((a)-1))
#define BITS(x) (sizeof(x) * 8)
#define IS_POW2(x) (((x) & ((x) - 1)) == 0)
#define MAX(x, y) ((x) > (y) ? (x) : (y))
#define MIN(x, y) ((x) < (y) ? (x) : (y))

#define NIBBLE_UNUSED_VAR(x) (void)(x)

#if defined(__linux__)
#define NIBBLE_HOST_LINUX
#else
#error "This operating system is not yet supported!"
#endif

typedef uint8_t u8;
typedef int8_t s8;
typedef uint16_t u16;
typedef int16_t s16;
typedef uint32_t u32;
typedef int32_t s32;
typedef uint64_t u64;
typedef int64_t s64;
typedef float f32;
typedef double f64;

typedef struct StringView {
    const char* str;
    size_t len;
} StringView;

#define string_view_lit(cstr_lit)                    \
    {                                                \
        .str = cstr_lit, .len = sizeof(cstr_lit) - 1 \
    }
#define string_view(s) ((StringView){.str = (s), .len = cstr_len(s)})

#define NIBBLE_FATAL_EXIT(f, ...) nibble_fatal_exit(__FILE__, __LINE__, (f), ##__VA_ARGS__)
void nibble_fatal_exit(const char* file, u32 line, const char* format, ...);

#endif
