#pragma once
#include <assert.h>
#include <stdbool.h>

typedef struct Argv_Helper {
    int argc;
    char** argv;
    int index;
} Argv_Helper;

static inline bool has_next_arg(Argv_Helper* helper)
{
    return helper->index < helper->argc;
}

static inline const char* get_next_arg(Argv_Helper* helper)
{
    assert(helper->index < helper->argc);
    return helper->argv[helper->index++];
}

static inline const char* peek_next_arg(Argv_Helper* helper)
{
    assert(helper->index < helper->argc);
    return helper->argv[helper->index];
}
