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

typedef enum ForeignLibKind {
    FOREIGN_LIB_INVALID,
    FOREIGN_LIB_OBJ,
    FOREIGN_LIB_STATIC,
    FOREIGN_LIB_SHARED
} ForeignLibKind;

typedef struct ForeignLib {
    ForeignLibKind kind;
    const StrLit* name;
    u32 ref_count;
} ForeignLib;

typedef struct GlobalData {
    BucketList list;
    size_t size;
} GlobalData;

typedef struct NibbleCtx {
    // Arena allocators.
    Allocator* gen_mem;
    Allocator ast_mem;
    Allocator tmp_mem;

    // True if compiler should not print to stdout.
    // Set with '-s' compiler flag.
    bool silent;

    const Path* working_dir; // The path from which the compiler is called.
    const Path* prog_entry_dir; // The directory containing the program entry file (i.e., main())

    // Search paths for imported/included modules.
    // Add paths with '-I <new_path>'
    const StringView* module_paths;
    u32 num_module_paths;

    const StringView* lib_paths;
    u32 num_lib_paths;

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
    GlobalData vars;
    BucketList procs;
    BucketList aggregate_types;

    // List of literals reachable from main
    GlobalData str_lits;
    GlobalData float_lits;
} NibbleCtx;

NibbleCtx* nibble_init(Allocator* mem_arena, OS target_os, Arch target_arch, bool silent,
                       const Path* working_dir, const Path* prog_entry_dir,
                       const StringView* module_paths, u32 num_module_paths,
                       const StringView* lib_paths, u32 num_lib_paths);
bool nibble_compile(NibbleCtx* nibble, const Path* main_path, const Path* out_path);
void nibble_cleanup(NibbleCtx* nibble);

ForeignLib* nibble_add_foreign_lib(NibbleCtx* nib_ctx, const StrLit* foreign_lib_name);

void add_global_data(GlobalData* data, void* item, size_t size);

void report_error(ErrorStream* error_stream, ProgRange range, const char* format, ...);

StrLit* intern_str_lit(HMap* map, const char* str, size_t len);
FloatLit* intern_float_lit(HMap* map, FloatKind kind, Float value);
Identifier* intern_ident(HMap* map, const char* str, size_t len);

#endif
