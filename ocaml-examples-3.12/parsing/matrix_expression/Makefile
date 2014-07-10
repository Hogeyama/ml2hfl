#######################################################################
#                                                                     #
#                        Caml examples                                #
#                                                                     #
#            Pierre Weis                                              #
#                                                                     #
#                        INRIA Rocquencourt                           #
#                                                                     #
#  Copyright (c) 1994-2011, INRIA                                     #
#  All rights reserved.                                               #
#                                                                     #
#  Distributed under the BSD license.                                 #
#                                                                     #
#######################################################################

# $Id: Makefile,v 1.1 2011-08-08 19:09:58 weis Exp $

ROOT_DIR=.

include $(ROOT_DIR)/config/Config.mk

include $(ROOT_DIR)/Objs.mk

CAMLC_INCLUDES=\
 -I $(ROOT_DIR)/basics

CAMLC_BYT_CONFIG=-w A -warn-error Ae -g $(CAMLC_INCLUDES)
CAMLC_BIN_CONFIG=-w A -warn-error Ae $(CAMLC_INCLUDES)\
 -unsafe -noassert -inline 10000

GENERATED_MLI_FILES=\
 parser.mli\

GENERATED_ML_FILES=\
 parser.ml\
 lexer.ml\
 ast_print.ml deep_ast_print.ml splice_ast_print.ml\

GENERATED_CAML_FILES=\
 $(GENERATED_MLI_FILES)\
 $(GENERATED_ML_FILES)\

BYT_OBJS=$(POLISH_BYT_OBJS)
OTHER_BYT_OBJS=\
 $(BASICS_BYT_OBJS)

BIN_OBJS=$(BYT_OBJS:.cmo=.cmx)

OTHER_BIN_OBJS=$(OTHER_BYT_OBJS:.cmo=.cmx)

LINK_BYT_OBJS=$(OTHER_BYT_OBJS) $(BYT_OBJS)
LINK_BIN_OBJS=$(LINK_BYT_OBJS:.cmo=.cmx)

EXE=polish

SUB_DIRS=basics

default: $(EXE)

all: byt bin default

byt: $(EXE).byt

$(EXE).byt: $(LINK_BYT_OBJS)
	$(CAMLC_BYT) -o $(EXE).byt $(LINK_BYT_OBJS)

bin: $(EXE).bin

$(EXE).bin: $(LINK_BIN_OBJS)
	$(CAMLC_BIN) -o $(EXE).bin $(LINK_BIN_OBJS)

$(EXE): $(EXE).bin
	$(CPC) $(EXE).bin $(EXE)

#lexer.ml: lexer.mll

#parser.ml: parser.mly
#parser.mli: parser.mly

ast_print.ml: ast.mli
	$(RM) $@ &&\
	$(CAMLC_PPG) -f ast.mli -t $@-tmp &&\
	echo "open Ast;;" >> $@ &&\
	cat $@-tmp >> $@ &&\
	$(RM) $@-tmp &&\
	$(CHMOD) a-w $@

deep_ast_print.ml: deep_ast.mli
	$(RM) $@ &&\
	$(CAMLC_PPG) -f deep_ast.mli -t $@-tmp &&\
	echo "open Ast;;" >> $@ &&\
	echo "open Ast_print;;" >> $@ &&\
	echo "open Deep_ast;;" >> $@ &&\
	cat $@-tmp >> $@ &&\
	$(RM) $@-tmp &&\
	$(CHMOD) a-w $@

splice_ast_print.ml: splice_ast.mli
	$(RM) $@ &&\
	$(CAMLC_PPG) -f splice_ast.mli -t $@-tmp &&\
	echo "open Splice_ast;;" >> $@ &&\
	echo "open Ast;;" >> $@ &&\
	echo "open Ast_print;;" >> $@ &&\
	echo "open Deep_ast;;" >> $@ &&\
	echo "open Deep_ast_print;;" >> $@ &&\
	cat $@-tmp >> $@ &&\
	$(RM) $@-tmp &&\
	$(CHMOD) a-w $@

test::

clean::
	$(RM) $(LINK_BYT_OBJS) $(LINK_BIN_OBJS) &&\
	$(RM) $(LINK_BYT_OBJS:.cmo=.o) $(LINK_BYT_OBJS:.cmo=.annot)\
	   $(MLI_FILES:.mli=.cmi) &&\
	$(RM) $(EXE) $(EXE).b* *.output

include $(ROOT_DIR)/config/Caml.mk
