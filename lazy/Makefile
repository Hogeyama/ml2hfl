SOURCES = enum.ml extList.ml extString.ml \
          util.ml \
          attr.ml idnt.ml const.ml var.ml arith.ml simType.ml term.ml formula.ml fdef.ml prog.ml \
          flags.ml \
          apronInterface.ml cvc3Interface.ml csisatInterface.ml \
          intType.ml intTypeCheck.ml \
          refType.ml refTypeCheck.ml \
          absType.ml \
          trace.ml compTree.ml compTreeExpander.ml \
          tcGen.ml tcGenIntType.ml tcGenRefType.ml \
          tcSolve.ml tcSolveIntType.ml tcSolveRefType.ml \
          hornClause.ml hcGenRefType.ml hcSolveRefType.ml \
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
