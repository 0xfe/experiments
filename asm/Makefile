targets = printf loop args atoi factorial mean

all: $(targets) hello64 sum

$(targets): $(targets:%=%.o)

%.o: %.asm
	nasm -f elf64 -g -F dwarf -o $@ $<

%: %.o
	gcc -no-pie -g -m64 -o $@ $<

clean:
	rm *.o
	rm $(targets) hello64 sum

# This one is special
hello64: hello64.asm
	nasm -f elf64 -g -F dwarf -o $@.o $<
	ld -o hello64 hello64.o -I/lib64/ld-linux-x86-64.so.2

sum: sum.o lib.o
	gcc -no-pie -g -m64 -o $@ $< lib.o