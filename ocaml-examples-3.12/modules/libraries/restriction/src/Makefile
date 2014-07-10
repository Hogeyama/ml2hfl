all:
	ocamlc -c inside.mli
	ocamlc -c outside.mli
	ocamlc -c inside.ml
	ocamlc -c outside.ml
	ocamlc -a inside.cmo outside.cmo -o my_lib1.cma
	ocamlc -a outside.cmo inside.cmo -o my_lib2.cma
	ocamlmktop -o top1 my_lib1.cma
	ocamlmktop -o top2 my_lib2.cma

install: all
	cp my_lib.cma outside.cmi ../lib

clean:
	rm -f *~ *.cm*
