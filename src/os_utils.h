#ifndef NIBBLE_OS_UTILS_H
#define NIBBLE_OS_UTILS_H
#include "nibble.h"
#include "allocator.h"

int run_cmd(Allocator* allocator, char* argv[], int argc, bool silent);
bool is_stderr_atty();
#endif
