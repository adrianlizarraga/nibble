#ifndef NIBBLE_COMPILER_H
#define NIBBLE_COMPILER_H
#include "nibble.h"
#include "cstring.h"
#include "stream.h"
#include "hash_map.h"
#include "ast.h"

typedef struct Error Error;
typedef struct ErrorStream ErrorStream;
typedef struct NibbleCtx NibbleCtx;

struct Error {
    Error* next;
    ProgRange range;
    size_t size;
    char msg[];
};

struct ErrorStream {
    Error* first;
    Error* last;
    size_t count;
    Allocator* allocator;
};

void error_stream_init(ErrorStream* stream, Allocator* allocator);
void error_stream_free(ErrorStream* stream);
void error_stream_add(ErrorStream* stream, ProgRange range, const char* buf, size_t size);

struct NibbleCtx {
    Allocator gen_mem;
    Allocator ast_mem;
    Allocator tmp_mem;

    HMap ident_map;
    HMap str_lit_map;
    HMap mod_map;

    ErrorStream errors;

    TypeCache type_cache;

    Path base_ospath;
    char* entry_filename;

    OS target_os;
    Arch target_arch;

    Module* builtin_mod;
    size_t num_builtins;

    BucketList vars;
    BucketList procs;
};

bool nibble_init(OS target_os, Arch target_arch);
bool nibble_compile(const char* mainf_name, size_t mainf_len, const char* outf_name, size_t outf_len);
void nibble_cleanup(void);

#endif
