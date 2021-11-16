//#define NDEBUG 1
#define PRINT_MEM_USAGE 1
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// TODO: REFACTOR AND IMPLEMENT cmd_echoed() on windows
#include <sys/wait.h>
#include <unistd.h>

//#define NDEBUG
//#define NIBBLE_PRINT_DECLS

// This is a "unity build".
// Having a single compilation unit makes building trivial.
// I'll add a proper build system later.
#include "allocator.c"
#include "cstring.c"
#include "path_utils.c"
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
#include "code_gen.c"
#include "x64_gen/gen.c"
#include "nibble.c"

void print_usage(FILE* fd, const char* program_name)
{
    ftprint_file(fd, true, "Usage: %s [OPTIONS] <input.nib>\n", program_name);
    ftprint_file(fd, true, "OPTIONS:\n");
    ftprint_file(fd, true, "    -h                              Print this help message\n");
    ftprint_file(fd, true, "    -os   [linux | win32 | osx]     Target OS\n");
    ftprint_file(fd, true, "    -arch [x64 | x86]               Target architecture\n");
    ftprint_file(fd, true, "    -o    <output_file>             Output binary file name. Defaults to `out`\n");
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

char* get_env_val(const char* name, char* envp[])
{
    size_t len = cstr_len(name);

    if (len && envp) {
        for (char** p = envp; *p; p += 1) {
            if (!cstr_ncmp(name, *p, len) && (*p)[len] == '=') {
                return *p + len + 1;
            }
        }
    }

    return NULL;
}

int cmd_echoed(char* argv[], size_t argc)
{
    (void)argc;

    pid_t child_pid;
    int child_status;

    ftprint_out("[CMD]:");
    for (char** p = argv; *p; p += 1) {
        ftprint_out(" %s", *p);
    }
    ftprint_out("\n");

    child_pid = fork();

    if (child_pid < 0) {
        // Fork failed.
        return -1;
    }
    else if (child_pid == 0) {
        // Replace child process image with the command to run.
        execvp(argv[0], argv);

        // Error if execvp returns!
        return -1;
    }
    else {
        // Parent process just waits for the child process to exit.
        while (wait(&child_status) != child_pid)
            ;

        int ret = WIFEXITED(child_status) ? WEXITSTATUS(child_status) : -1;

        return ret;
    }
}

void make_tmp_name_ext(char dst[NIBBLE_MAX_PATH], const char* src, size_t src_len, const char* ext, size_t ext_len)
{
    size_t dst_len = src_len + ext_len;

    memcpy(dst, src, src_len);
    memcpy(dst + src_len, ext, ext_len);

    dst[dst_len] = '\0';
}

int main(int argc, char* argv[], char* envp[])
{
    (void) envp;
    /*
    const char* path_env = get_env_val("PATH", envp);
    ftprint_out("PATH IS: %s\n", path_env);

    const char* p = path_env;

    while (*p) {
        const char* e = p;
        while (*e && *e != ':') {
            e++;
        }

        int len = e - p;
        ftprint_out("%.*s\n", len, p);

        p = *e ? e + 1 : e;
    }
    */

    const char* program_name = consume_arg(&argc, &argv);
    char* input_file = NULL;
    char* output_file = "out";

    OS target_os = OS_LINUX;
    Arch target_arch = ARCH_X64;

    while (argc > 0) {
        char* arg = consume_arg(&argc, &argv);

        if (cstr_cmp(arg, "-h") == 0) {
            print_usage(stdout, program_name);
            exit(0);
        }
        else if (cstr_cmp(arg, "-os") == 0) {
            target_os = get_target_os(&argc, &argv, program_name);
        }
        else if (cstr_cmp(arg, "-arch") == 0) {
            target_arch = get_target_arch(&argc, &argv, program_name);
        }
        else if (cstr_cmp(arg, "-o") == 0) {
            output_file = get_flag_value(&argc, &argv, program_name, "-o");
        }
        else {
            if (input_file) {
                ftprint_err("[ERROR]: unknown option `%s`\n\n", arg);
                print_usage(stderr, program_name);
                exit(1);
            }

            input_file = arg;
        }
    }

    if (!input_file) {
        ftprint_err("[ERROR]: No input file provided.\n\n");
        print_usage(stderr, program_name);
        exit(1);
    }

    if (!nibble_init(target_os, target_arch)) {
        ftprint_err("[ERROR]: Failed to initialize compiler.\n");
        exit(1);
    }

    size_t out_len = cstr_len(output_file);
    const char nasm_ext[] = ".s";
#ifdef NIBBLE_HOST_LINUX
    const char obj_ext[] = ".o";
#else
    const char obj_ext[] = ".obj";
#endif

    const size_t max_len = NIBBLE_MAX_PATH - 1 - sizeof(obj_ext) - 1;

    if (out_len >= max_len) {
        ftprint_err("[ERROR]: Output file path is too large ( > %llu).\n", max_len);
        exit(1);
    }

    char nasm_input[NIBBLE_MAX_PATH];
    make_tmp_name_ext(nasm_input, output_file, out_len, nasm_ext, sizeof(nasm_ext) - 1);

    char nasm_output[NIBBLE_MAX_PATH];
    make_tmp_name_ext(nasm_output, output_file, out_len, obj_ext, sizeof(obj_ext) - 1);

    if (nibble_compile(input_file, nasm_input)) {
        char* nasm_cmd[] = {"nasm", "-f", "elf64", nasm_input, "-o", nasm_output, NULL};

        if (cmd_echoed(nasm_cmd, ARRAY_LEN(nasm_cmd) - 1) >= 0) {
            char* ld_cmd[] = {"ld", "-o", output_file, nasm_output, NULL};

            if (cmd_echoed(ld_cmd, ARRAY_LEN(ld_cmd) - 1) >= 0) {
                ftprint_out("[INFO]: Done\n");
            }
            else {
                ftprint_err("[ERROR]: linker command failed\n");
            }
        }
        else {
            ftprint_err("[ERROR]: assembler command failed\n");
        }
    }

    nibble_cleanup();

    return 0;
}
