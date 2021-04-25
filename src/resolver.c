#include "resolver.h"
#include "parser.h"

static char* slurp_file(Allocator* allocator, const char* filename)
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

Module* compile_module(const char* filename, ProgPos pos)
{
    Allocator bootstrap = allocator_create(256);
    Module* module = new_type(&bootstrap, Module, true);
    module->allocator = bootstrap;
    module->errors = byte_stream_create(&module->allocator);
    module->ast_arena = allocator_create(1024);

    const char* str = slurp_file(&module->allocator, filename);
    if (!str)
        return NULL;

    /////////////////////////////////////
    //  Print AST
    /////////////////////////////////////

    Parser parser = {0};
    parser_init(&parser, &module->ast_arena, str, pos, &module->errors);
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

void free_module(Module* module)
{
    Allocator bootstrap = module->allocator;

#ifndef NDEBUG
    print_allocator_stats(&module->ast_arena, "AST mem stats");
    print_allocator_stats(&module->allocator, "Module mem stats");
#endif

    allocator_destroy(&module->ast_arena);
    allocator_destroy(&bootstrap);
}
