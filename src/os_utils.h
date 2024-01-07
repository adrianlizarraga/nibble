#ifndef NIBBLE_OS_UTILS_H
#define NIBBLE_OS_UTILS_H
#include "basics.h"
#include "allocator.h"
#include "array.h"

typedef struct ExecCmd {
    const char** argv;
    size_t argc;
} ExecCmd;

int run_cmd(Allocator* allocator, const ExecCmd* cmd, bool silent);
Array(char) cmd_to_str(Allocator* arena, const ExecCmd* cmd);
bool is_stderr_atty(void);
#endif
