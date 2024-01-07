#include "allocator.h"
#include "array.h"
#include "os_utils.h"
#include "cstring.h"

#include <sys/wait.h>
#include <unistd.h>

int run_cmd(Allocator* allocator, const ExecCmd* cmd, bool silent)
{
    (void)allocator;

    pid_t child_pid;
    int child_status;

    if (!silent) {
        ftprint_out("[CMD]:");
        for (const char** p = cmd->argv; *p; p += 1) {
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
        execvp(cmd->argv[0], (char* const*)cmd->argv);

        // Error if execvp returns!
        return -1;
    }
    else {
        // Parent process just waits for the child process to exit.
        while (wait(&child_status) != child_pid) {
        }

        int ret = WIFEXITED(child_status) ? WEXITSTATUS(child_status) : -1;

        return ret;
    }
}

Array(char) cmd_to_str(Allocator* arena, const ExecCmd* cmd)
{
    Array(char) str = array_create(arena, char, 32);

    for (size_t i = 0; i < cmd->argc; i++) {
        if (i > 0) {
            array_push(str, ' ');
        }

        ftprint_char_array(&str, false, "%s", cmd->argv[i]);
    }

    array_push(str, '\0');

    return str;
}

bool is_stderr_atty(void)
{
    return isatty(STDERR_FILENO);
}
