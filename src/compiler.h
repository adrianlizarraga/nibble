#ifndef NIBBLE_COMPILER_H
#define NIBBLE_COMPILER_H
#include "nibble.h"
#include "cstring.h"
#include "stream.h"
#include "hash_map.h"

typedef struct TypeCache {
    HMap ptrs;
    HMap arrays;
    HMap procs;
    HMap slices; // Struct types that represent array slices
    HMap structs; // Anonymous
    HMap unions; // Anonymous
} TypeCache;

typedef struct NibbleCtx {
    Allocator gen_mem;
    Allocator ast_mem;
    Allocator tmp_mem;

    bool silent;

    HMap ident_map;
    HMap str_lit_map;
    HMap float_lit_map;
    HMap mod_map;

    BucketList src_files;

    ErrorStream errors;

    TypeCache type_cache;

    OS target_os;
    Arch target_arch;

    struct Module* main_mod;
    struct Module* builtin_mod;
    size_t num_builtins;

    BucketList vars;
    BucketList procs;
    BucketList aggregate_types;
    BucketList str_lits;
    BucketList float_lits;
} NibbleCtx;

NibbleCtx* nibble_init(OS target_os, Arch target_arch, bool silent);
bool nibble_compile(NibbleCtx* nibble, const char* mainf_name, size_t mainf_len, const char* outf_name, size_t outf_len);
void nibble_cleanup(NibbleCtx* nibble);

void report_error(ErrorStream* error_stream, ProgRange range, const char* format, ...);

StrLit* intern_str_lit(HMap* map, const char* str, size_t len);
FloatLit* intern_float_lit(HMap* map, FloatKind kind, Float value);
Identifier* intern_ident(HMap* map, const char* str, size_t len);

#endif
