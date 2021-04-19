//#define NDEBUG 1
#define PRINT_MEM_USAGE 1
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// This is a "unity build".
// Having a single compilation unit makes building trivial.
// I'll add a proper build system later.
#include "allocator.c"
#include "cstring.c"
#include "print.c"
#include "array.c"
#include "hash_map.c"
#include "stream.c"
#include "lexer.c"
#include "ast.c"
#include "parser.c"
#include "types.c"
#include "nibble.c"

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

    // TODO: Retrieve from command-line or environment.
    OS target_os = OS_LINUX;
    Arch target_arch = ARCH_X64;

    if (!nibble_init(target_os, target_arch))
    {
        ftprint_err("ERROR: Failed to initialize compiler.\n");
        exit(1);
    }

    CompiledModule* module = compile_module(input_file, 0);
    if (!module) {
        ftprint_err("ERROR: Failed to compile file.\n");
        exit(1);
    }

    free_compiled_module(module);
    nibble_cleanup();
}
