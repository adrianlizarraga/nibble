//#define NDEBUG 1
#define PRINT_MEM_USAGE 1
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "allocator.c"
#include "cstring.c"
#include "print.c"
#include "array.c"
#include "hash_map.c"
#include "stream.c"
#include "nibble.c"
#include "lexer.c"
#include "ast.c"
#include "parser.c"

typedef struct CompiledModule {
    Allocator allocator;
    Allocator ast_arena;
    ByteStream errors;
} CompiledModule;

char* slurp_file(Allocator* allocator, const char* filename)
{
    FILE* fd = fopen(filename, "r");
    if (!fd)
        return NULL;

    if (fseek(fd, 0, SEEK_END) < 0)
        return NULL;

    long int size = ftell(fd);
    if (size < 0)
        return NULL;

    char* buf = mem_allocate(allocator, size + 1, DEFAULT_ALIGN, false);
    if (!buf)
        return NULL;

    if (fseek(fd, 0, SEEK_SET) < 0)
        return NULL;

    size_t n = fread(buf, 1, (size_t)size, fd);
    if (ferror(fd))
        return NULL;

    fclose(fd);

    buf[n] = '\0';

    return buf;
}

static CompiledModule* compile_module(const char* filename, ProgPos pos)
{
    Allocator bootstrap = allocator_create(256);
    CompiledModule* module = new_type(&bootstrap, CompiledModule, true);
    module->allocator = bootstrap;
    module->errors = byte_stream_create(&module->allocator);
    module->ast_arena = allocator_create(1024);

    const char* str = slurp_file(&module->allocator, filename);
    if (!str)
        return NULL;

    /////////////////////////////////////
    //  Print AST
    /////////////////////////////////////

    Parser parser = parser_create(&module->ast_arena, str, pos, &module->errors);

    next_token(&parser);

    while (!is_token_kind(&parser, TKN_EOF))
    {
        Decl* decl = parse_decl(&parser);
        if (!decl)
            break;

        ftprint_out("%s\n", ftprint_decl(&module->allocator, decl));
    }

    ftprint_out("\n");

    if (module->errors.num_chunks > 0)
    {
        ftprint_out("\nParsing errors: %lu\n", module->errors.num_chunks);
        ByteStreamChunk* chunk = module->errors.first;

        while (chunk)
        {
            ftprint_out("%s\n", chunk->buf);
            chunk = chunk->next;
        }
    }

    parser_destroy(&parser);

    return module;
}

static void free_compiled_module(CompiledModule* module)
{
    Allocator bootstrap = module->allocator;

#ifndef NDEBUG
    print_allocator_stats(&module->ast_arena, "AST mem stats");
    print_allocator_stats(&module->allocator, "Module mem stats");
#endif

    allocator_destroy(&module->ast_arena);
    allocator_destroy(&bootstrap);
}


void print_usage(FILE* fd, const char* program_name)
{
    ftprint_file(fd, true, "Usage: %s [OPTIONS] <input.nib>\n", program_name);
    ftprint_file(fd, true, "OPTIONS:\n");
    ftprint_file(fd, true, "    -h        Print this help message\n");
}

char* consume_arg(int* argc, char*** argv)
{
    assert(*argc);
    char* arg = *argv[0];

    *argc -= 1;
    *argv += 1;

    return arg;
}

int main(int argc, char* argv[])
{
    const char* program_name = consume_arg(&argc, &argv);
    const char* input_file = NULL;

    while (argc > 0)
    {
        const char* arg = consume_arg(&argc, &argv);

        if (cstr_cmp(arg, "-h") == 0)
        {
            print_usage(stdout, program_name);
            exit(0);
        }
        else
        {
            if (input_file)
            {
                ftprint_err("ERROR: unknown option `%s`\n\n", arg);
                print_usage(stderr, program_name);
                exit(1);
            }

            input_file = arg;
        }
    }

    if (!input_file)
    {
        ftprint_err("ERROR: No input file provided.\n\n");
        print_usage(stderr, program_name);
        exit(1);
    }

    if (!nibble_init())
    {
        ftprint_err("ERROR: Failed to initialize compiler.\n");
        exit(1);
    }

    CompiledModule* module = compile_module(input_file, 0);
    if (!module) {
        ftprint_err("ERROR: Failed to compile file.\n");
        exit(1);
    }

    // sizeof(TypeSpec) = 48, sizeof(Expr) = 48, sizeof(Stmt) = 32, sizeof(Decl) = 80
    // sizeof(TypeSpec) = 48, sizeof(Expr) = 48, sizeof(Stmt) = 32, sizeof(Decl) = 24
    // sizeof(TypeSpec) = 12, sizeof(Expr) = 48, sizeof(Stmt) = 32, sizeof(Decl) = 24
    // sizeof(TypeSpec) = 12, sizeof(Expr) = 12, sizeof(Stmt) = 32, sizeof(Decl) = 24
    // ftprint_out("sizeof(TypeSpec) = %lu, sizeof(Expr) = %lu, sizeof(Stmt) = %lu, sizeof(Decl) = %lu\n\n",
    // sizeof(TypeSpec), sizeof(Expr), sizeof(Stmt), sizeof(Decl));

    free_compiled_module(module);
    nibble_cleanup();
}
