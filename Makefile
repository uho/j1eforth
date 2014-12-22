all: j1 j1.bin j1.hex

j1: j1.c
	gcc -o j1 j1.c

j1.bin j1.hex: j1.4th
	gforth j1.4th

run: all
	./j1

clean:
	rm -rf j1 j1.bin j1.hex
