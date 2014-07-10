#######################################################################
#                                                                     #
#                           Objective Caml                            #
#                                                                     #
#               Pierre Weis, projet Cristal, INRIA Rocquencourt       #
#                                                                     #
#  Copyright 2001 Institut National de Recherche en Informatique et   #
#  en Automatique.  All rights reserved.  This file is distributed    #
#  only by permission.                                                #
#                                                                     #
#######################################################################

# $Id: Makefile,v 1.2 2009-11-15 21:37:39 weis Exp $

LIB_EXT=a
EXE_EXT=out
DLL_EXT=so

CAML_BYT=ocamlc
CAML_BIN=ocamlopt
CAML_INSTALL_DIR=/usr/local
CAML_LIB_DIR=$(CAML_INSTALL_DIR)/lib/ocaml
CAML_BYT_RUN_LIB=$(CAML_LIB_DIR)/libcamlrun.$(LIB_EXT)
CAML_BIN_RUN_LIB=$(CAML_LIB_DIR)/libasmrun.$(LIB_EXT)

C_LINKER=gcc
C_COMP=gcc

client.$(EXE_EXT): client.c fib_dll.$(DLL_EXT)
	$(C_COMP) -o "client.$(EXE_EXT)" client.c fib_dll.$(DLL_EXT)

fib_dll.$(DLL_EXT): fib.o fib_wrappers.o
	$(C_LINKER) -o fib_dll.$(DLL_EXT) -I $(CAML_LIB_DIR) \
	  fib_wrappers.o fib.o \
	  -lm -lncurses $(CAML_BYT_RUN_LIB)

#	link /nologo /dll /libpath:"$(CAML_LIB_DIR)" \
#          /out:fib_dll.$(DLL_EXT) /implib:fib_dll.$(LIB_EXT) \
#	  /export:fib \
#	  /export:format_result \

fib_wrappers.o: fib_wrappers.c
	$(CAML_BYT) -c fib_wrappers.c

fib.o: fib.ml
	$(CAML_BYT) -output-obj -o fib.o fib.ml

clean:
	rm -f *.cmi *.cmo *.o *.$(EXE_EXT) *.$(DLL_EXT) *.$(LIB_EXT)
