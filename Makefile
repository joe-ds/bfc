bfcmake: bfc.lisp
	sbcl --script bfc.lisp
	nasm -f elf64 bf_out.nasm
	ld bf_out.o -o bf
	rm -v bf_out.nasm bf_out.o