; This program prints out the program argument count followed by
; each argument, line by line.
;
; MoC <shhh@mo.town>

global main
extern printf

SECTION .data

SECTION .rodata
count           db  "arg count: %d",10,0
first           db  "arg: %s",10,0
;
SECTION .text

; argc is in rdi -- integer
; argv is in rsi -- array of pointers (char**)

main:
    ; prelude
    push    rbp
    mov     rbp, rsp

    ; save registers that will be clobbered
    push r12
    push r13
    push r14

    mov     r14, rdi    ; save argc... rdi will be clobbered
    mov     r12, rsi    ; save argv... rsi will be clobbered

    ; show arg count
    mov     rsi, rdi    ; pass argc to print_arg_count
    call    print_arg_count

    ; loop from 0 to argc
    mov     r13, 0      ; r13 will be the index

list:
    ; calculate address of argument: rax = r12 + (8 * r13)
    mov   rax, 8
    mul   r13         ; rax is the accumulator for mul
    add   rax, r12
    mov   rdi, [rax]  ; deref rax
    call  print_arg

    inc   r13         ; next
    cmp   r13, r14    ; check if index == argc
    je    exit
    jmp   list

exit:
    pop r14
    pop r13
    pop r12
    pop rbp
    xor rax, rax ; return value = 0
    ret


print_arg_count:
    ; expects argc in first param: rdi
    push  rbp
    mov   rbp, rsp
    mov   rsi, rdi
    lea   rdi, [count]
    xor   rax, rax ; vor variadic args, 0 floating point args
    call  printf
    leave
    ret

print_arg:
   ; expects pointer to string in first param: rdi
    push  rbp
    mov   rbp, rsp
    mov   rsi, rdi
    lea   rdi, [first]  ; get a pointer to "first"
    xor   rax, rax ; vor variadic args, 0 floating point args
    call  printf
    leave
    ret


