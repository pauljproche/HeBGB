toplevel : toplevel.ml ast.ml parser.mly scanner.mll builtin.o
	dune build

builtin.o : builtin.c
	cc -c builtin.c

.PHONY: run
run : toplevel
	dune exec ./toplevel.exe

.PHONY: clean
clean :
	dune clean;
	rm -f builtin.o -f $1.ll $1.s a.out tests/*.ll tests/*.s
