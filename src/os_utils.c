#include "os_utils.h"

#ifdef NIBBLE_HOST_LINUX
#include <sys/wait.h>
#include <unistd.h>

int run_cmd(char* argv[], int argc)
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
#elif defined(NIBBLE_HOST_WINDOWS)

int run_cmd(char* argv[], int argc)
{
    (void)argc;

    ftprint_out("[CMD]:");
    for (char** p = argv; *p; p += 1) {
        ftprint_out(" %s", *p);
    }
    ftprint_out(" ... NOT SUPPORTED\n");

    return -1;
}
#else
#endif
