CC=gcc
CFLAGS= -g -Wall
LDFLAGS= -g

default: compiler.native libfoo.a libref.a libmut.a

compiler.native: compiler.ml scanner.mll parser.mly irgen.ml semant.ml ast.ml sast.ml
	ocamlbuild -use-ocamlfind -pkgs llvm compiler.native

parser.native: parser.mly ast.mli scanner.mll
	ocambuild parser.native

libfoo.a: foo.o
	ar -crs libfoo.a foo.o
	ranlib libfoo.a

foo.o: foo.h foo.c

libref.a: ref.o
	ar -crs libref.a ref.o
	ranlib libref.a

ref.o: ref.h ref.c

libmut.a: mut.o
	ar -crs libmut.a mut.o
	ranlib libmut.a

mut.o: mut.h mut.c

.PHONY: clean
clean:
	ocamlbuild -clean 2>/dev/null
	rm -f scanner.native
	rm -rf _build
	rm -rf *.o *.s *.byte exe *.out *.a


.PHONY: all
all: clean default
