targets = printf loop args atoi

all: $(targets) hello64

$(targets): $(targets:%=%.o)

%.o: %.asm
	nasm -f elf64 -g -o $@ $<

%: %.o
	gcc -no-pie -m64 -o $@ $<

clean:
	rm *.o
	rm $(targets) hello64

# This one is special
hello64: hello64.asm
	nasm -f elf64 -g -F dwarf -o $@.o $<
	ld -o hello64 hello64.o -I/lib64/ld-linux-x86-64.so.2

