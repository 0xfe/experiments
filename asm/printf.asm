global main
extern printf
default rel

SECTION .data
val             dq  42

SECTION .rodata
hello           db  "Hello %ld!",10,0
;
SECTION .text

main:
    push    rbx
    lea     rdi, [hello]
    mov     rsi, [val]
    xor     rax, rax
    call    printf
    mov     rax, 0
    pop     rbx
    ret
