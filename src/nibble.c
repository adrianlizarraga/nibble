#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "nibble.h"
#include "cstring.h"

const char* os_names[NUM_OS] = {
    [OS_LINUX] = "linux",
};

const char* arch_names[NUM_ARCH] = {
    [ARCH_X64] = "x64",
};

const u8 float_kind_sizes[FLOAT_KIND_COUNT] = {[FLOAT_F64] = 8, [FLOAT_F32] = 4};
const char* float_kind_names[FLOAT_KIND_COUNT] = {[FLOAT_F64] = "f64", [FLOAT_F32] = "f32"};
const u8 int_kind_sizes[INTEGER_KIND_COUNT] = {
    [INTEGER_BOOL] = 1, [INTEGER_U8] = 1,  [INTEGER_S8] = 1,  [INTEGER_U16] = 2, [INTEGER_S16] = 2,
    [INTEGER_U32] = 4,  [INTEGER_S32] = 4, [INTEGER_U64] = 8, [INTEGER_S64] = 8,
};
const char* int_kind_names[INTEGER_KIND_COUNT] = {
    [INTEGER_BOOL] = "bool", [INTEGER_U8] = "u8",   [INTEGER_S8] = "s8",   [INTEGER_U16] = "u16", [INTEGER_S16] = "s16",
    [INTEGER_U32] = "u32",   [INTEGER_S32] = "s32", [INTEGER_U64] = "u64", [INTEGER_S64] = "s64",
};
const bool int_kind_signed[INTEGER_KIND_COUNT] = {
    [INTEGER_BOOL] = false, [INTEGER_U8] = false, [INTEGER_S8] = true,   [INTEGER_U16] = false, [INTEGER_S16] = true,
    [INTEGER_U32] = false,  [INTEGER_S32] = true, [INTEGER_U64] = false, [INTEGER_S64] = true,
};
const u64 int_kind_max[INTEGER_KIND_COUNT] = {
    [INTEGER_BOOL] = 0x1,
    [INTEGER_U8] = 0xFF,
    [INTEGER_S8] = 0x7F,
    [INTEGER_U16] = 0xFFFF,
    [INTEGER_S16] = 0x7FFF,
    [INTEGER_U32] = 0xFFFFFFFF,
    [INTEGER_S32] = 0x7FFFFFFF,
    [INTEGER_U64] = 0xFFFFFFFFFFFFFFFF,
    [INTEGER_S64] = 0x7FFFFFFFFFFFFFFF,
};
const u64 int_kind_min[INTEGER_KIND_COUNT] = {
    [INTEGER_BOOL] = 0x0,
    [INTEGER_U8] = 0x00,
    [INTEGER_S8] = 0x80,
    [INTEGER_U16] = 0x0000,
    [INTEGER_S16] = 0x8000,
    [INTEGER_U32] = 0x00000000,
    [INTEGER_S32] = 0x80000000,
    [INTEGER_U64] = 0x0000000000000000,
    [INTEGER_S64] = 0x8000000000000000,
};
const char* keyword_names[KW_COUNT];
const char* annotation_names[ANNOTATION_COUNT];

Identifier* intrinsic_idents[INTRINSIC_COUNT];
Identifier* builtin_struct_fields[BUILTIN_STRUCT_FIELD_COUNT];
Identifier* main_proc_ident;

ProgRange merge_ranges(ProgRange a, ProgRange b)
{
    ProgRange r = {.start = (a.start < b.start ? a.start : b.start), .end = (a.end > b.end ? a.end : b.end)};

    return r;
}

void report_error(ErrorStream* errors, ProgRange range, const char* format, ...)
{
    char buf[MAX_ERROR_LEN];
    va_list vargs;

    va_start(vargs, format);
    size_t size = vsnprintf(buf, MAX_ERROR_LEN, format, vargs) + 1;
    va_end(vargs);

    if (size >= MAX_ERROR_LEN) {
        buf[MAX_ERROR_LEN - 1] = '\0';
        size = MAX_ERROR_LEN;
    }

    error_stream_add(errors, range, buf, size);
}

void error_stream_init(ErrorStream* stream, Allocator* allocator)
{
    memset(stream, 0, sizeof(ErrorStream));
    stream->allocator = allocator;
}

void error_stream_free(ErrorStream* stream)
{
    Error* err = stream->first;

    while (err) {
        Error* next = err->next;
        mem_free(stream->allocator, err);
        err = next;
    }

    stream->first = stream->last = NULL;
}

void error_stream_add(ErrorStream* stream, ProgRange range, const char* msg, size_t size)
{
    if (stream) {
        size_t err_size = offsetof(Error, msg) + size;
        Error* err = mem_allocate(stream->allocator, err_size, DEFAULT_ALIGN, false);

        if (err) {
            memcpy(err->msg, msg, size);
            err->size = size;
            err->next = NULL;
            err->range = range;

            if (!stream->first)
                stream->last = stream->first = err;
            else
                stream->last = stream->last->next = err;

            stream->count += 1;
        }
    }
}

void add_global_data(GlobalData* data, void* item, size_t size)
{
    bucket_list_add_elem(&data->list, item);
    data->size += size;
}

FloatLit* intern_float_lit(HMap* float_lit_map, FloatKind kind, Float value)
{
    Allocator* allocator = float_lit_map->allocator;

    u64 num_bytes = float_kind_sizes[kind];
    u64 key = hash_bytes(&value, num_bytes, FNV_INIT);
    u64* pval = hmap_get(float_lit_map, key);
    FloatLit* intern = pval ? (void*)*pval : NULL;

    // Walk linked list in case of collision.
    for (FloatLit* it = intern; it; it = it->next) {
        if ((it->kind == kind) && float_eq(kind, it->value, value)) {
            return it;
        }
    }

    // If we got here, add this float literal to the intern table.
    FloatLit* new_intern = alloc_type(allocator, FloatLit, true);

    if (!new_intern) {
        NIBBLE_FATAL_EXIT("Out of memory.\n%s:%d\n", __FILE__, __LINE__);
        return NULL;
    }

    new_intern->next = intern; // Chain to colliding literal (if any)
    new_intern->kind = kind;
    new_intern->value = value;

    hmap_put(float_lit_map, key, (uintptr_t)new_intern);

    return new_intern;
}

StrLit* intern_str_lit(HMap* strmap, const char* str, size_t len)
{
    Allocator* allocator = strmap->allocator;

    uint64_t key = hash_bytes(str, len, FNV_INIT);
    uint64_t* pval = hmap_get(strmap, key);
    StrLit* intern = pval ? (void*)*pval : NULL;

    // Collisions will only occur if identical hash values are produced. Collisions due to
    // contention for hash map slots will not occur (open addressing).
    //
    // Walk the linked list in case of collision.
    for (StrLit* it = intern; it; it = it->next) {
        if ((it->len == len) && (cstr_ncmp(it->str, str, len) == 0))
            return it;
    }

    // If we got here, need to add this string to the intern table.
    StrLit* new_intern = mem_allocate(allocator, offsetof(StrLit, str) + len + 1, DEFAULT_ALIGN, true);

    if (!new_intern) {
        NIBBLE_FATAL_EXIT("Out of memory.\n%s:%d\n", __FILE__, __LINE__);
        return NULL;
    }

    new_intern->next = intern; // Record collision. If a collision did _NOT_ occur, this will be null.
    new_intern->len = len;

    memcpy(new_intern->str, str, len);
    new_intern->str[len] = '\0';

    hmap_put(strmap, key, (uintptr_t)new_intern);

    return new_intern;
}

Identifier* intern_ident(HMap* strmap, const char* str, size_t len)
{
    Allocator* allocator = strmap->allocator;
    uint64_t key = hash_bytes(str, len, FNV_INIT);
    uint64_t* pval = hmap_get(strmap, key);
    Identifier* intern = pval ? (void*)*pval : NULL;

    // Collisions will only occur if identical hash values are produced. Collisions due to
    // contention for hash map slots will not occur (open addressing).
    //
    // Walk the linked list in case of collision.
    for (Identifier* it = intern; it; it = it->next) {
        if ((it->len == len) && (cstr_ncmp(it->str, str, len) == 0)) {
            return it;
        }
    }

    // If we got here, need to add this string to the intern table.
    Identifier* new_intern = mem_allocate(allocator, offsetof(Identifier, str) + len + 1, DEFAULT_ALIGN, true);

    if (!new_intern) {
        // TODO: Handle in a better way.
        ftprint_err("[INTERNAL ERROR]: Out of memory.\n%s:%d\n", __FILE__, __LINE__);
        exit(1);
        return NULL;
    }

    new_intern->next = intern; // Record collision. If a collision did _NOT_ occur, this will be null.
    new_intern->len = len;

    memcpy(new_intern->str, str, len);
    new_intern->str[len] = '\0';

    hmap_put(strmap, key, (uintptr_t)new_intern);

    return new_intern;
}
