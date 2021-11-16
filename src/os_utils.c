#include "os_utils.h"

#ifdef NIBBLE_HOST_LINUX
#include <sys/wait.h>
#include <unistd.h>

int run_cmd(Allocator* allocator, char* argv[], int argc)
{
    (void)allocator;
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
#include "array.h"

int run_cmd(Allocator* allocator, char* argv[], int argc)
{
    (void)argc;

    char* cmd_line_cstr = array_create(allocator, char, 64);

    ftprint_out("[CMD]:");
    for (char** p = argv; *p; p += 1) {
        ftprint_out(" %s", *p);
        ftprint_char_array(&cmd_line_cstr, false, "%s ", *p);
    }

    ftprint_out("\n");
    array_push(cmd_line_cstr, '\0');

    STARTUPINFO si = {0};
    PROCESS_INFORMATION pi = {0};

    si.cb = sizeof(si);

    // Start the child process. 
    if( !CreateProcess( NULL,   // No module name (use command line)
        cmd_line_cstr,  // Command line
        NULL,           // Process handle not inheritable
        NULL,           // Thread handle not inheritable
        FALSE,          // Set handle inheritance to FALSE
        0,              // No creation flags
        NULL,           // Use parent's environment block
        NULL,           // Use parent's starting directory 
        &si,            // Pointer to STARTUPINFO structure
        &pi )           // Pointer to PROCESS_INFORMATION structure
    ) 
    {
        printf( "CreateProcess failed (%d).\n", GetLastError() );
        return -1;
    }

    // Wait until child process exits.
    WaitForSingleObject( pi.hProcess, INFINITE );

    // Close process and thread handles. 
    CloseHandle( pi.hProcess );
    CloseHandle( pi.hThread );

    return 0;
}
#else
#endif
