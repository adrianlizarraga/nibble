#ifndef NIBBLE_COMPILER_H
#define NIBBLE_COMPILER_H
#include "nibble.h"
#include "cstring.h"
#include "stream.h"
#include "hash_map.h"

typedef struct NibbleCtx NibbleCtx;

struct NibbleCtx {
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

    Path base_ospath;
    char* entry_filename;

    OS target_os;
    Arch target_arch;

    struct Module* builtin_mod;
    size_t num_builtins;

    BucketList vars;
    BucketList procs;
    BucketList aggregate_types;
    BucketList str_lits;
    BucketList float_lits;
};

NibbleCtx* nibble_init(OS target_os, Arch target_arch, bool silent);
bool nibble_compile(NibbleCtx* nibble, const char* mainf_name, size_t mainf_len, const char* outf_name, size_t outf_len);
void nibble_cleanup(NibbleCtx* nibble);

void report_error(ErrorStream* error_stream, ProgRange range, const char* format, ...);

typedef struct InternMap {
    HMap map;
    Allocator* alloc;
} InternMap;

StrLit* intern_str_lit(InternMap* map, const char* str, size_t len);
FloatLit* intern_float_lit(InternMap* map, FloatKind kind, Float value);
Identifier* intern_ident(InternMap* map, const char* str, size_t len);

#endif
