#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <signal.h>
#include <stdbool.h>

#define MY_ASSERT(COND) \
    do { \
        if (!(COND)) { \
            printf("%s:%d Assertion \"%s\" failed\n", __FILE__, __LINE__, #COND ); \
            kill(getpid(), SIGABRT); \
        } \
    } while(0)


int main(void) {
    printf("My PID %d\n", getpid());
    int pid = getpid();

    MY_ASSERT(pid == 1);

    return 0;
}
