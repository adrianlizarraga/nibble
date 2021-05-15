#ifndef NIBBLE_RESOLVER_H
#define NIBBLE_RESOLVER_H
#include "nibble.h"
#include "allocator.h"
#include "stream.h"
#include "llist.h"
#include "ast.h"

#define MAX_LOCAL_SYMS 512

typedef struct Program Program;

struct Program {
    Allocator gen_mem;
    Allocator tmp_mem;
    Allocator ast_mem;
    ByteStream errors;

    const char* path;
    const char* code;

    List decls;
    HMap global_syms;

    // TODO: Create an expandable stack similar to the arena allocator
    Symbol local_syms[MAX_LOCAL_SYMS];
    Symbol* local_syms_at;

    HMap type_ptr_cache;
    HMap type_proc_cache;
};

Program* compile_program(const char* path);
void free_program(Program* program);
#endif
