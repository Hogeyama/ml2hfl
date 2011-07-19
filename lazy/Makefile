SOURCES = enum.ml extList.ml extString.ml \
          util.ml flag.ml attr.ml id.ml type.ml const.ml var.ml arith.ml term.ml apronInterface.ml csisatInterface.ml fdef.ml prog.ml ctree.ml trace.ml refType.ml main.ml
INCDIRS = ../csisat-read-only/obj /usr/local/lib
LIBDIRS = ../csisat-read-only/lib /usr/local/lib
LIBS = ../csisat-read-only/lib/libcsisat /usr/local/lib/ocaml/bigarray /usr/local/lib/gmp /usr/local/lib/apron /usr/local/lib/polka
CLIBS = camlpico picosat camlglpk glpk
RESULT = mochi
OCAMLFLAGS = -thread -w A
YFLAGS = -v
CC = gcc
include OCamlMakefile

all: nc
