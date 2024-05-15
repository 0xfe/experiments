section .data
  hello: db 'Hello World!', 10, 13
  helloLen: db $ - hello

section .text
  global _start

_start:
  mov eax, 4
  mov ebx, 2
  mov ecx, hello
  mov edx, [ helloLen ]
  int 80h

  mov ebx, eax
  mov eax, 1
  int 80h
