SOURCES = enum.ml extList.ml extString.ml \
          global.ml \
          util.ml zipper.ml \
          attr.ml idnt.ml const.ml var.ml callId.ml simType.ml \
          term.ml typTerm.ml \
          linArith.ml poly.ml parLinArith.ml formula.ml typSubst.ml formulaUtil.ml fes.ml \
          fdef.ml prog.ml \
          cvc3Interface.ml csisatInterface.ml atpInterface.ml apronInterface.ml \
          farkas.ml nonLinConstrSolve.ml \
          refType.ml refTypeCheck.ml \
          absType.ml \
          trace.ml compTree.ml compTreeExpander.ml \
          callTree.ml atom.ml hornClause.ml hornClauseEc.ml hornClauseUtil.ml typPredSubst.ml \
          hcGenRefType.ml hcSolve.ml hcBwSolve.ml hcGenSolve.ml \
          verifier.ml \
          main.ml
#          intType.ml intTypeCheck.ml \
#          traceConstr.ml tcGenIntType.ml tcGenRefType.ml \
#          tcSolve.ml tcSolveIntType.ml tcSolveRefType.ml
DOC_FILES = enum.mli extList.mli extString.mli $(SOURCES)
INCDIRS = /usr/lib/ocaml/csisat-read-only/lib /usr/lib/ocaml/csisat-read-only/obj /usr/lib/ocaml/atp /usr/lib /usr/local/lib
LIBDIRS = /usr/lib /usr/local/lib
LIBS = libcsisat bigarray str unix nums atp_batch gmp apron polka
CLIBS = camlpico picosat camlglpk glpk
RESULT = mochi
OCAMLFLAGS = -thread -w A
YFLAGS = -v
CC = gcc
include OCamlMakefile

all: nc

install: $(SOURCES)
	cp $(SOURCES) Makefile ~/hmc/lazy
