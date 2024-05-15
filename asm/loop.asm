global main
extern printf

SECTION .data
val             dq  42

SECTION .rodata
hello           db  "Hello %ld!",10,0
;
SECTION .text

main:
    push    rbp
    mov     r12, [val]

loop_start:
    lea     rdi, [hello]
    mov     rsi, r12
    xor     rax, rax
    call    printf
    sub     r12, 1
    jnz     loop_start

    mov     rax, 0
    pop     rbp
    ret
