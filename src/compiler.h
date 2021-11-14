#ifndef NIBBLE_COMPILER_H
#define NIBBLE_COMPILER_H
#include "nibble.h"
#include "cstring.h"
#include "stream.h"
#include "hash_map.h"
#include "ast.h"

typedef struct NibbleCtx {
    Allocator gen_mem;
    Allocator ast_mem;
    Allocator tmp_mem;

    HMap ident_map;
    HMap str_lit_map;
    HMap mod_map;

    ByteStream errors;

    TypeCache type_cache;

    Path base_ospath;
    char* entry_filename;

    OS target_os;
    Arch target_arch;

    Module builtin_mod;
    size_t num_builtins;

    BucketList vars;
    BucketList procs;
} NibbleCtx;

bool nibble_init(OS target_os, Arch target_arch);
void nibble_compile(const char* input_file, const char* output_file);
void nibble_cleanup(void);

#endif
