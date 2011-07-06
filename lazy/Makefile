SOURCES = enum.ml extList.ml extString.ml \
          util.ml attr.ml id.ml type.ml const.ml var.ml arith.ml term.ml apronInterface.ml csisatInterface.ml fdef.ml prog.ml ctree.ml refType.ml main.ml
INCDIRS = ./csisat-read-only/obj /usr/lib
LIBDIRS = ./csisat-read-only/lib /usr/lib
LIBS = ./csisat-read-only/lib/libcsisat /usr/lib/ocaml/bigarray /usr/lib/gmp /usr/lib/apron /usr/lib/polka
CLIBS = camlpico picosat camlglpk glpk
RESULT = mochi
OCAMLFLAGS = -thread -w A
YFLAGS = -v
CC = gcc
include OCamlMakefile

all: nc
