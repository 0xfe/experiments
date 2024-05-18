global main
extern printf

section .data
    E_badargs db "Usage: factorial N", 10, 0
    P_print_fac db "Factorial of %d is %ld", 10, 0

section .text

main:
    push rbp
    mov rbp, rsp

    ; enough args?
    cmp rdi, 2
    jne arg_error

    ; convert arg to integer
    mov rdi, [rsi+8]
    call atoi
    push rax ; save result
    mov rdi, rax
    push rsi ; dummy for stack alignment
    call factorial
    pop rsi ; dummy

    lea rdi, [P_print_fac]  ; arg 0 - the format string
    pop rsi                 ; arg 1 - the number
    mov rdx, rax            ; arg 2 - the factorial
    xor rax, rax            ; must be 0 if no FP args
    call printf

    xor rax, rax
    pop rbp
    ret

factorial:
    push rbp
    mov rbp, rsp

    mov rcx, rdi
    mov rax, rdi

factorial_loop:
    cmp rcx, 1
    je factorial_done
    dec rcx
    mul rcx
    jmp factorial_loop

factorial_done:
    leave
    ret

arg_error:
    mov rdi, E_badargs
    mov rax, 0
    call printf
    mov rax, 1
    mov rsp, rbp
    pop rbp
    ret

atoi:
    push rbp
    mov rbp, rsp

    mov r9, 10   ; power of 10 for mul
    mov rcx, 0
    xor rax, rax

atoi_loop:
    movzx rsi, byte [rdi+rcx]
    cmp rsi, 0
    je atoi_done

    cmp rsi, 48
    jl atoi_done

    cmp rsi, 57
    jg atoi_done

    sub rsi, 48
    mul r9
    add rax, rsi
    inc rcx
    jmp atoi_loop

atoi_done:
    leave
    ret