make:
	ocamlc -c interpreter.ml
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamllex lexer.mll
	ocamlc -c lexer.ml
	ocamlc -c prolog.ml
	ocamlfind ocamlc -linkpkg -package unix -o prolog interpreter.cmo parser.cmo lexer.cmo prolog.cmo

clean:
	rm -f lexer.ml parser.ml parser.mli
	rm -f *.c*
	rm -f prolog
