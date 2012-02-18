SOURCES = enum.ml extList.ml extString.ml \
          util.ml zipper.ml \
          attr.ml idnt.ml const.ml var.ml simType.ml term.ml linArith.ml nonLinArith.ml parLinArith.ml formula.ml fdef.ml prog.ml \
          flags.ml \
          apronInterface.ml cvc3Interface.ml csisatInterface.ml atpInterface.ml \
          farkas.ml \
          intType.ml intTypeCheck.ml \
          refType.ml refTypeCheck.ml \
          absType.ml \
          trace.ml compTree.ml compTreeExpander.ml \
          traceConstr.ml tcGenIntType.ml tcGenRefType.ml \
          tcSolve.ml tcSolveIntType.ml tcSolveRefType.ml \
          callTree.ml hornClause.ml hcGenRefType.ml hcSolve.ml \
          verifier.ml \
          main.ml
DOC_FILES = enum.mli extList.mli extString.mli $(SOURCES)
INCDIRS = /usr/lib/ocaml/csisat-read-only/lib /usr/lib/ocaml/csisat-read-only/obj /usr/lib/ocaml/atp /usr/lib /usr/local/lib
LIBDIRS = /usr/lib /usr/local/lib
LIBS = libcsisat bigarray str unix gmp apron polka nums atp_batch
CLIBS = camlpico picosat camlglpk glpk
RESULT = mochi
OCAMLFLAGS = -thread -w A
YFLAGS = -v
CC = gcc
include OCamlMakefile

all: nc

install: $(SOURCES)
	cp $(SOURCES) Makefile ~/hmc/lazy
