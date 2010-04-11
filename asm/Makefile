all: asm

asm.o: asm.S
	nasm -f elf asm.S

asm: asm.o
	ld -o asm asm.o

clean:
	rm asm asm.o
