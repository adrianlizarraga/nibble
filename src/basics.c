#include "basics.h"
#include "cstring.h"
#include <stdlib.h>

void nibble_fatal_exit(const char* file, u32 line, const char* format, ...)
{
    va_list vargs;

    ftprint_err("%s:%u:%u: [FATAL ERROR]: ", file, line, 0);

    va_start(vargs, format);
    ftprintv_err(format, vargs);
    va_end(vargs);

    ftprint_err("\n");

    exit(1);
}
