SOURCES = enum.ml extList.ml extString.ml \
          util.ml flags.ml \
          attr.ml idnt.ml const.ml var.ml arith.ml simType.ml term.ml \
          apronInterface.ml csisatInterface.ml cvc3Interface.ml fdef.ml prog.ml ctree.ml ctreeExpander.ml sizType.ml trace.ml refType.ml absType.ml verifier.ml main.ml
INCDIRS = /usr/lib/ocaml/csisat-read-only/lib /usr/lib/ocaml/csisat-read-only/obj /usr/lib /usr/local/lib
LIBDIRS = /usr/lib /usr/local/lib
LIBS = libcsisat bigarray str unix gmp apron polka
CLIBS = camlpico picosat camlglpk glpk
RESULT = mochi
OCAMLFLAGS = -thread -w A
YFLAGS = -v
CC = gcc
include OCamlMakefile

all: nc

install: $(SOURCES)
	cp $(SOURCES) Makefile ~/hmc/lazy
