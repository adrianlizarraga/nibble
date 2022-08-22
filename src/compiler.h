#ifndef NIBBLE_COMPILER_H
#define NIBBLE_COMPILER_H
#include "nibble.h"
#include "cstring.h"
#include "stream.h"
#include "hash_map.h"
#include "path_utils.h"

typedef struct TypeCache {
    HMap ptrs;
    HMap arrays;
    HMap procs;
    HMap slices; // Struct types that represent array slices
    HMap structs; // Anonymous
    HMap unions; // Anonymous
} TypeCache;

typedef struct ForeignLib {
    StrLit* name;
    u32 ref_count;
    // NOTE: Might add a list of the actual symbols accessed from this foreign lib.
} ForeignLib;

typedef struct NibbleCtx {
    // Arena allocators.
    Allocator gen_mem;
    Allocator ast_mem;
    Allocator tmp_mem;

    // True if compiler should not print to stdout.
    // Set with '-s' compiler flag.
    bool silent;

    Path working_dir; // The path from which the compiler is called.
    Path prog_entry_dir; // The directory containing the program entry file (i.e., main())

    // Import and include search paths.
    // Add paths with '-I <new_path>'
    const StringView* search_paths;
    u32 num_search_paths;

    HMap ident_map;
    HMap str_lit_map;
    HMap float_lit_map;
    HMap mod_map; // Mod path => Module

    BucketList src_files;

    ErrorStream errors;

    TypeCache type_cache;

    OS target_os;
    Arch target_arch;

    struct Module* main_mod;
    struct Module* builtin_mod;
    size_t num_builtins;

    HMap foreign_lib_map; // lib name => ForeignLib struct
    BucketList foreign_libs; // Array of ForeignLib elems
    BucketList foreign_procs; // Array of Symbol elems

    // List of symbols reachable from main
    BucketList vars;
    BucketList procs;
    BucketList aggregate_types;

    // List of literals reachable from main
    BucketList str_lits;
    BucketList float_lits;
} NibbleCtx;

NibbleCtx* nibble_init(OS target_os, Arch target_arch, bool silent, const StringView* search_paths, u32 num_search_paths);
bool nibble_compile(NibbleCtx* nibble, StringView main_file, StringView out_file);
void nibble_cleanup(NibbleCtx* nibble);

ForeignLib* nibble_add_foreign_lib(NibbleCtx* nib_ctx, StrLit* foreign_lib_name);

void report_error(ErrorStream* error_stream, ProgRange range, const char* format, ...);

StrLit* intern_str_lit(HMap* map, const char* str, size_t len);
FloatLit* intern_float_lit(HMap* map, FloatKind kind, Float value);
Identifier* intern_ident(HMap* map, const char* str, size_t len);

#endif
