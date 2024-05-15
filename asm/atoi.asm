; This program converts the first argument to an integer.
; each argument, line by line.
;
; MoC <shhh@mo.town>

global main
extern printf

SECTION .data

SECTION .rodata
length          db  "length: %d",10,0
success         db  "success: %d",10,0
failed          db  "failed: %s != %d",10,0

test_1          db  "7",0
test_2          db  "42",0
test_3          db  "100",0
test_4          db  "3u2",0
test_5          db  "4096",0
test_6          db  "200000",0
test_7          db  "004096",0

ans_1           dd 7
ans_2           dd 42
ans_3           dd 100
ans_4           dd 0
ans_5           dd 4096
ans_6           dd 200000
ans_7           dd 4096

;
SECTION .text

; argc is in rdi -- integer
; argv is in rsi -- array of pointers (char**)

main:
    ; prelude
    push    rbp
    mov     rbp, rsp

    ; save registers that will be clobbered
    mov   rdi, test_1
    movsxd rsi, dword [ans_1]
    call  do_test

    mov   rdi, test_2
    movsxd rsi, dword [ans_2]
    call  do_test

    mov   rdi, test_3
    movsxd rsi, dword [ans_3]
    call  do_test

    mov   rdi, test_4
    movsxd rsi, dword [ans_4]
    call  do_test

    mov   rdi, test_5
    movsxd rsi, dword [ans_5]
    call  do_test

    mov   rdi, test_6
    movsxd rsi, dword [ans_6]
    call  do_test

    mov   rdi, test_7
    movsxd rsi, dword [ans_7]
    call  do_test

    pop rbp
    xor rax, rax ; return value = 0
    ret

do_test:
    push  rbp
    mov   rbp, rsp
    push  rsi ; push ans
    push  rdi;; push q
    call  strlen
    push  rax ; push length
    lea   rdi, [length]
    mov   rsi, rax
    xor   rax, rax ; vor variadic args, 0 floating point args
    call  printf

    pop   rsi ; pop length
    pop   r12 ; pop q
    mov   rdi, r12
    call  atoi

    pop rsi   ; pop ans
    cmp rsi, rax
    jne incorrect
    lea   rdi, [success]
    mov   rsi, rax
    xor   rax, rax ; vor variadic args, 0 floating point args
    call  printf
    jmp do_test_done


incorrect:
    lea   rdi, [failed]
    mov   rsi, r12
    mov   rdx, rax
    xor   rax, rax ; vor variadic args, 0 floating point args
    call  printf

do_test_done:
    leave
    ret

atoi:
    push rbp
    mov rbp, rsp
    mov rcx, rsi ; length
    mov r11, 0   ; total
    mov r9, 10   ; power of 10 for mul

next_digit:
    dec rcx      ; dec length, iterate backwards over digits

    ; calculate power of 10 to multiply place digit by
    mov r10, rcx ; counter, from current place to rsi (length)
    inc r10      ; current place is rcx + 1
    mov rax, 1   ; one's place power

power:
    cmp r10, rsi ; is counter == len?
    je convert   ; yes, convert to integer
    mul r9       ; no, multiply by 10 and
    inc r10      ; increment counter (place)
    jmp power

convert:
    movzx r10, byte [rdi+rcx]  ; get ASCII char at digit location

    cmp r10, 48                ; make sure it's between '0' and '9'
    jl  convert_error

    cmp r10, 57
    jg  convert_error

    sub r10, 48                ; 48 = "0" in ASCII
    mul r10                    ; multiply by power of 10 calculated above
    add r11, rax               ; add to total
    cmp rcx, 0
    jg next_digit
    mov rax, r11
    leave
    ret

convert_error:
    mov rax, 0
    leave
    ret

strlen:
    push  rbp
    mov   rbp, rsp
    mov rcx, 0

strlen_count:
    cmp [rdi+rcx], byte 0
    je  strlen_done
    inc rcx
    jmp strlen_count

strlen_done:
    mov rax, rcx
    leave
    ret
