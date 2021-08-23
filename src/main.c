//#define NDEBUG 1
#define PRINT_MEM_USAGE 1
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//#define NDEBUG
#define NIBBLE_PRINT_DECLS

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
#include "resolver.c"
#include "bytecode.c"
#include "print_ir.c"
#include "gen_assembly.c"
#include "x64_gen.c"
#include "nibble.c"

void print_usage(FILE* fd, const char* program_name)
{
    ftprint_file(fd, true, "Usage: %s [OPTIONS] <input.nib>\n", program_name);
    ftprint_file(fd, true, "OPTIONS:\n");
    ftprint_file(fd, true, "    -h                              Print this help message\n");
    ftprint_file(fd, true, "    -os   [linux | win32 | osx]     Target OS\n");
    ftprint_file(fd, true, "    -arch [x64 | x86]               Target architecture\n");
    ftprint_file(fd, true, "    -o    <output_file>             Output binary file name. Defaults to `out.s`\n");
}

char* consume_arg(int* argc, char*** argv)
{
    assert(*argc);
    char* arg = *argv[0];

    *argc -= 1;
    *argv += 1;

    return arg;
}

char* get_flag_value(int* argc, char*** argv, const char* program_name, const char* flag)
{
    if (*argc == 0)
    {
        ftprint_err("ERROR: no value provided for `%s` option\n", flag);
        print_usage(stderr, program_name);
        exit(1);
    }

    return consume_arg(argc, argv);
}

OS get_target_os(int* argc, char*** argv, const char* program_name)
{
    OS target_os = OS_INVALID;
    char* os_val = get_flag_value(argc, argv, program_name, "-os");

    cstr_tolower(os_val);

    for (int i = 1; i < NUM_OS; i += 1)
    {
        if (cstr_cmp(os_names[i], os_val) == 0)
        {
            target_os = (OS)i;
            break;
        }
    }

    if (target_os == OS_INVALID)
    {
        ftprint_err("ERROR: Invalid OS `%s`\n", os_val);
        print_usage(stderr, program_name);
        exit(1);
    }

    return target_os;
}

Arch get_target_arch(int* argc, char*** argv, const char* program_name)
{
    Arch target_arch = ARCH_INVALID;
    char* arch_val = get_flag_value(argc, argv, program_name, "-arch");

    cstr_tolower(arch_val);

    for (int i = 1; i < NUM_ARCH; i += 1)
    {
        if (cstr_cmp(arch_names[i], arch_val) == 0)
        {
            target_arch = (Arch)i;
            break;
        }
    }

    if (target_arch == ARCH_INVALID)
    {
        ftprint_err("ERROR: Invalid architecture `%s`\n", arch_val);
        print_usage(stderr, program_name);
        exit(1);
    }

    return target_arch;
}

int main(int argc, char* argv[])
{
    const char* program_name = consume_arg(&argc, &argv);
    const char* input_file = NULL;
    const char* output_file = "out.s";

    // TODO: Detect default os/arch from env variables or GCC macros
    OS target_os = OS_LINUX;
    Arch target_arch = ARCH_X64;

    while (argc > 0)
    {
        const char* arg = consume_arg(&argc, &argv);

        if (cstr_cmp(arg, "-h") == 0)
        {
            print_usage(stdout, program_name);
            exit(0);
        }
        else if (cstr_cmp(arg, "-os") == 0)
        {
            target_os = get_target_os(&argc, &argv, program_name);
        }
        else if (cstr_cmp(arg, "-arch") == 0)
        {
            target_arch = get_target_arch(&argc, &argv, program_name);
        }
        else if (cstr_cmp(arg, "-o") == 0)
        {
            output_file = get_flag_value(&argc, &argv, program_name, "-o");
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

    if (!nibble_init(target_os, target_arch))
    {
        ftprint_err("ERROR: Failed to initialize compiler.\n");
        exit(1);
    }

    nibble_compile(input_file, output_file);
    nibble_cleanup();

    return 0;
}
