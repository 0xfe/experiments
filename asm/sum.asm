; sum list of integers in arguments


global main
extern printf, atoi

section .data
    E_badargs db "Usage: sum N [N ...]", 10, 0
    P_print_sum db "Sum: %ld", 10, 0

section .text

main:
    push rbp
    mov rbp, rsp

    ; enough args?
    cmp rdi, 2
    jl arg_error

    mov r12, rdi ; argc
    mov r13, 0   ; total
    mov r14, rsi ; argv
    mov r15, 1   ; counter

sum_loop:
    ; convert arg to integer
    mov rdi, [r14+r15*8]
    call atoi               ; from lib.asm
    add r13, rax
    inc r15
    cmp r15, r12
    jl sum_loop

    lea rdi, [P_print_sum]  ; arg 0 - the format string
    mov rsi, r13            ; arg 1 - the number
    xor rax, rax            ; must be 0 if no FP args
    call printf

    xor rax, rax
    pop rbp
    ret

arg_error:
    mov rdi, E_badargs
    mov rax, 0
    call printf
    mov rax, 1
    mov rsp, rbp
    pop rbp
    ret