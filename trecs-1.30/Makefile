SOURCE = utilities.ml scc.mli scc.ml syntax.ml parser.mli parser.ml lexer.ml grammar.ml automaton.ml conversion.ml typing.ml propagate.ml stype.ml reduce.ml generalize.ml main.ml

parser.mli parser.ml:
	ocamlyacc parser.mly
lexer.ml:
	ocamllex lexer.mll

trecsbyte: $(SOURCE)
	(ocamlc -o trecs $(SOURCE); rm *.cmi *.cmx *.o; cd ..)

trecs: $(SOURCE)
	(ocamlopt -o trecs unix.cmxa $(SOURCE); cd ..)
#	(ocamlopt -o trecs unix.cmxa $(SOURCE); rm *.cmi *.cmx *.o; cd ..)

.SUFFIXES:
	.ml .cmo .mli .cmi

.ml.cmo:
	ocamlc $(SOURCE)

top: $(SOURCE)
	(ocamlmktop unix.cma $(SOURCE); cd ..)

clean: 
	rm *.cmi *.cmx *.o *.cmo *.exe parser.ml lexer.ml parser.mli lexer.mli



