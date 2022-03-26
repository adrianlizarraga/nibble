#include <stdio.h>

int find_prime_iterative (int initial) {
    int prime = 1;
    int curr = 1;
    int prime_count = 0;

    while (prime_count < initial) {
        for (int denom = 2; denom < curr; denom++) {
            if (curr % denom == 0) {
                curr = curr + 1;
                continue;
            }
        }
        prime = curr;
        curr = curr + 1;
        prime_count = prime_count + 1;
    }

    return prime;
}

int main() {
    int out = find_prime_iterative(20000);
    printf("result: %d\n", out);
    return 0;
}

/*
find_prime_iterative:
        push    rbp
        mov     rbp, rsp
        mov     DWORD PTR [rbp-20], edi
        mov     DWORD PTR [rbp-4], 1
        mov     DWORD PTR [rbp-8], 1
        mov     DWORD PTR [rbp-12], 0
        jmp     .L2
.L6:
        mov     DWORD PTR [rbp-16], 2
        jmp     .L3
.L5:
        mov     eax, DWORD PTR [rbp-8]
        cdq
        idiv    DWORD PTR [rbp-16]
        mov     eax, edx
        test    eax, eax
        jne     .L4
        add     DWORD PTR [rbp-8], 1
        nop
.L4:
        add     DWORD PTR [rbp-16], 1
.L3:
        mov     eax, DWORD PTR [rbp-16]
        cmp     eax, DWORD PTR [rbp-8]
        jl      .L5
        mov     eax, DWORD PTR [rbp-8]
        mov     DWORD PTR [rbp-4], eax
        add     DWORD PTR [rbp-8], 1
        add     DWORD PTR [rbp-12], 1
.L2:
        mov     eax, DWORD PTR [rbp-12]
        cmp     eax, DWORD PTR [rbp-20]
        jl      .L6
        mov     eax, DWORD PTR [rbp-4]
        pop     rbp
        ret
 */
