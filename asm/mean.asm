; print the mean of a list of floating point arguments

global main
extern printf

section .data
    E_badargs     db "Usage: mean N [N ...]", 10, 0
    P_print_mean  db "Mean: %lf", 10, 0
    FP_zero       dq 0.0
    FP_ten        dq 10.0

section .text

main:
    push rbp
    mov rbp, rsp

    ; enough args?
    cmp rdi, 2
    jl arg_error

    mov r12, rdi ; argc
    mov r14, rsi ; argv
    mov r15, 1   ; counter
    movsd xmm1, qword [FP_zero] ; total

sum_loop:
    ; convert arg to integer
    mov rdi, [r14+r15*8]
    call atof               ; from lib.asm
    addsd xmm1, xmm0
    inc r15
    cmp r15, r12
    jl sum_loop

    ; divide by count
    sub r12, 1
    cvtsi2sd xmm3, r12
    divsd xmm1, xmm3

    lea rdi, [P_print_mean]    ; arg 0 - the format string
    movsd xmm0, xmm1           ; arg 1 - the mean
    mov rax, 1                 ; there's 1 FP arg to printf
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

atof:
    push rbp
    mov rbp, rsp

    mov rcx, 0         ; string intex

    movsd xmm0, qword [FP_zero] ; accumulator
    movsd xmm3, qword [FP_ten]  ; place value (multiplier)

atof_loop:
    movzx rsi, byte [rdi+rcx]
    cmp rsi, 46 ; period
    je frac_start

    cmp rsi, 0
    je atof_done

    cmp rsi, 48
    jl atof_done

    cmp rsi, 57
    jg atof_done

    sub rsi, 48          ; convert ASCII to integer
    cvtsi2sd xmm2, rsi   ; convert integer to double
    mulsd xmm0, xmm3     ; multiply integer accumulator by 10
    addsd xmm0, xmm2     ; add integer to accumulator

    inc rcx
    jmp atof_loop

frac_start:
    inc rcx

frac_loop:
    movzx rsi, byte [rdi+rcx]
    cmp rsi, 0
    je atof_done

    cmp rsi, 48
    jl atof_done

    cmp rsi, 57
    jg atof_done

    sub rsi, 48        ; convert ASCII to integer
    cvtsi2sd xmm2, rsi ; convert integer to double
    divsd xmm2, xmm3   ; divide integer by place
    addsd xmm0, xmm2   ; accumulate

    mulsd xmm3, qword [FP_ten] ; increase place value by 1
    inc rcx

    jmp frac_loop

atof_done:
    leave
    ret