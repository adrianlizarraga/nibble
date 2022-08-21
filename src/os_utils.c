#include "os_utils.h"

#include <sys/wait.h>
#include <unistd.h>

int run_cmd(Allocator* allocator, const char* const argv[], int argc, bool silent)
{
    (void)allocator;
    (void)argc;

    pid_t child_pid;
    int child_status;

    if (!silent) {
        ftprint_out("[CMD]:");
        for (const char* const* p = argv; *p; p += 1) {
            ftprint_out(" %s", *p);
        }
        ftprint_out("\n");
    }

    child_pid = fork();

    if (child_pid < 0) {
        // Fork failed.
        return -1;
    }
    else if (child_pid == 0) {
        // Replace child process image with the command to run.
        execvp(argv[0], (char* const*)argv);

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

bool is_stderr_atty()
{
    return isatty(STDERR_FILENO);
}

