default: compiler.native

compiler.native: compiler.ml
	ocamlbuild -r -use-ocamlfind -Is utils -pkgs llvm compiler.native

parser.native: parser.mly ast.mli scanner.mll
	ocambuild parser.native


.PHONY: clean
clean:
	ocamlbuild -clean 2>/dev/null
	rm -rf _build
