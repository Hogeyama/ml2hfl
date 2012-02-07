SOURCES = enum.ml extList.ml extString.ml \
          util.ml \
          attr.ml idnt.ml const.ml var.ml arith.ml simType.ml term.ml formula.ml fdef.ml prog.ml \
          flags.ml \
          apronInterface.ml cvc3Interface.ml csisatInterface.ml \
          compTree.ml compTreeExpander.ml \
          hornClause.ml cgen.ml \
          intType.ml cgenIntType.ml intTypeCheck.ml \
          refType.ml cgenRefType.ml refTypeCheck.ml \
          absType.ml \
          verifier.ml \
          main.ml
DOC_FILES = enum.mli extList.mli extString.mli $(SOURCES)
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
