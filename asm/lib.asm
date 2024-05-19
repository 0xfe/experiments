; sum list of integers in arguments

global atoi, strlen
section .text

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
