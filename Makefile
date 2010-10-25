
CSISAT = ./csisat-read-only

SOURCES = flag.ml util.ml utilities.ml automata.ml syntax.ml parser.mly lexer.mll wrapper.ml alpha.ml typing.ml CPS.ml \
	  abstract.ml lift.ml check.ml feasibility.ml infer.ml refine.ml main.ml
LIBS = unix libcsisat
CLIBS = camlpico picosat camlglpk glpk
INCDIRS = $(CSISAT)/lib $(CSISAT)/obj
RESULT = mc
OCAMLFLAGS= -dtypes
YFLAGS = -v
CFLAGS = -g

include OCamlMakefile

all:
	make dc && \
	make SOURCES="$(RUNTIMESOURCES)" RESULT=$(RUNTIMERESULT) bcl

all-nc: 
	make nc && \
	make SOURCES="$(RUNTIMESOURCES)" RESULT=$(RUNTIMERESULT) ncl

clean-all:
	make clean
	make SOURCES="$(RUNTIMESOURCES)" clean
	-rm -rf *~
	-rm -rf *.output
	-rm -rf *.annot
	-rm -rf *.a *.cma *.cmxa
