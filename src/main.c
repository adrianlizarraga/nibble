/* TODO:

   - Consider making string literals evaluate to slices (or pointers to static arrays) rather than arrays
   - Only generate assembly for procs reachable from main()
     - Currently, we prune procs not reachable from the main _MODULE_, but unused procs in main module remain.
     - RELATED: Compile all module symbols, but only generate used symbols.
   - Switch statements
   - Floating point primitives
   - Defer statement
*/

/* Wishlist:
    - Anonymous procedures. Can put code in array indexed by an enum kind!
    - Enum sub types. EX: A proc only expects a subset of possible enum values.
    - Programmer should be able to map constants (enums) to other constants (enums) at compile-time, without
      having to create a global/static/readonly mapping array. Need some way of communicating this intent to compiler.
    - Allow constructing/initializing via a procedure using only a single statement.
      Ex: Instead of `var a : MyType; InitMyType(^a);` allow `var a : MyType #init InitMyType();` or `InitMyType(var a : MyType);`
*/

/*
 * BUGS
    - Printing all IRS for floats.nib crashes.
 */

//#define NDEBUG 1
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//#define NDEBUG
//#define NIBBLE_PRINT_IRS
//#define NIBBLE_PRINT_MEM_USAGE

// This is a "unity build".
// Having a single compilation unit makes building trivial.
// I'll add a proper build system later.
#include "allocator.c"
#include "cstring.c"
#include "path_utils.c"
#include "os_utils.c"
#include "print.c"
#include "array.c"
#include "hash_map.c"
#include "stream.c"
#include "lexer/module.c"
#include "ast/module.c"
#include "parser/module.c"
#include "resolver/module.c"
#include "bytecode/module.c"
#include "x64_gen/module.c"
#include "nibble.c"

void print_usage(FILE* fd, const char* program_name)
{
    ftprint_file(fd, true, "Usage: %s [OPTIONS] <input.nib>\n", program_name);
    ftprint_file(fd, true, "OPTIONS:\n");
    ftprint_file(fd, true, "    -h                              Print this help message\n");
    ftprint_file(fd, true, "    -s                              Silent mode (no output to stdout)\n");
    ftprint_file(fd, true, "    -os   [linux | win32 | osx]     Target OS\n");
    ftprint_file(fd, true, "    -arch [x64 | x86]               Target architecture\n");
    ftprint_file(fd, true, "    -I    <import_search_path>      Add import search path\n");
    ftprint_file(fd, true, "    -o    <output_file>             Output binary file name. Defaults to `out`\n");
}

char* consume_arg(int* argc, char*** argv)
{
    assert(*argc);
    char* arg = (*argv)[0];

    *argc -= 1;
    *argv += 1;

    return arg;
}

char* get_flag_value(int* argc, char*** argv, const char* program_name, const char* flag)
{
    if (*argc == 0) {
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

    for (int i = 1; i < NUM_OS; i += 1) {
        if (cstr_cmp(os_names[i], os_val) == 0) {
            target_os = (OS)i;
            break;
        }
    }

    if (target_os == OS_INVALID) {
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

    for (int i = 1; i < NUM_ARCH; i += 1) {
        if (cstr_cmp(arch_names[i], arch_val) == 0) {
            target_arch = (Arch)i;
            break;
        }
    }

    if (target_arch == ARCH_INVALID) {
        ftprint_err("ERROR: Invalid architecture `%s`\n", arch_val);
        print_usage(stderr, program_name);
        exit(1);
    }

    return target_arch;
}

#define NUM_USER_SEARCH_PATHS 63
#define NUM_DEFAULT_SEARCH_PATHS 1

void add_search_path(StringView* paths, u32* p_num_paths, u32 cap, const char* new_path, const char* prog_name)
{
    if (*p_num_paths >= cap) {
        ftprint_err("ERROR: Too many import search paths (maximum is %u)\n", cap);
        print_usage(stderr, prog_name);
        exit(1);
    }

    paths[*p_num_paths] = (StringView){new_path, cstr_len(new_path)};
    *p_num_paths += 1;
}

int main(int argc, char* argv[])
{
    StringView search_paths[NUM_USER_SEARCH_PATHS + NUM_DEFAULT_SEARCH_PATHS];
    u32 num_search_paths;

    const char* program_name = consume_arg(&argc, &argv);
    const char* mainf_name = NULL;
    const char* outf_name = "out";

    bool silent = false;

    // Set default target os/arch.
#if defined(NIBBLE_HOST_LINUX)
    OS target_os = OS_LINUX;
    Arch target_arch = ARCH_X64;
#else
    OS target_os = OS_INVALID;
    Arch target_arch = ARCH_INVALID;
#endif

    while (argc > 0) {
        const char* arg = consume_arg(&argc, &argv);

        if (cstr_cmp(arg, "-h") == 0) {
            print_usage(stdout, program_name);
            exit(0);
        }
        else if (cstr_cmp(arg, "-s") == 0) {
            silent = true;
        }
        else if (cstr_cmp(arg, "-os") == 0) {
            target_os = get_target_os(&argc, &argv, program_name);
        }
        else if (cstr_cmp(arg, "-arch") == 0) {
            target_arch = get_target_arch(&argc, &argv, program_name);
        }
        else if (cstr_cmp(arg, "-o") == 0) {
            outf_name = get_flag_value(&argc, &argv, program_name, "-o");
        }
        else if (cstr_cmp(arg, "-I") == 0) {
            const char* search_path = get_flag_value(&argc, &argv, program_name, "-I");

            add_search_path(search_paths, &num_search_paths, NUM_USER_SEARCH_PATHS, search_path, program_name);
        }
        else {
            if (mainf_name) {
                ftprint_err("[ERROR]: unknown option `%s`\n\n", arg);
                print_usage(stderr, program_name);
                exit(1);
            }

            mainf_name = arg;
        }
    }

    // Add working directory as a search path.
    add_search_path(search_paths, &num_search_paths, ARRAY_LEN(search_paths), ".", program_name);

    size_t mainf_len = mainf_name ? cstr_len(mainf_name) : 0;
    size_t outf_len = outf_name ? cstr_len(outf_name) : 0;

    if (!mainf_len) {
        ftprint_err("[ERROR]: No input source file provided.\n\n");
        print_usage(stderr, program_name);
        exit(1);
    }

    if (!outf_len) {
        ftprint_err("[ERROR]: Invalid output file name provided.\n\n");
        print_usage(stderr, program_name);
        exit(1);
    }

    NibbleCtx* nib_ctx = nibble_init(target_os, target_arch, silent, search_paths, num_search_paths);

    if (!nib_ctx) {
        ftprint_err("[ERROR]: Failed to initialize compiler.\n");
        exit(1);
    }

    bool success = nibble_compile(nib_ctx, mainf_name, mainf_len, outf_name, outf_len);
    nibble_cleanup(nib_ctx);

    return !success;
}
