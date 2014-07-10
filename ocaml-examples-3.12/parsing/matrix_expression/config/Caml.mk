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

# $Id: Caml.mk,v 1.1 2011-08-08 19:09:58 weis Exp $

# This Makefile defines generic rules to compile various kinds of Caml source
# files and to automatically handle their dependencies.

# This Makefile also defines two entries:
# - clean (to clean up the compiled files)
# - depend (to recompute the dependency order).

# This Makefile should be included at the end of the Makefile that handles a
# set of Caml files (to build a library or an application).
# Simpy write at the end of your Makefile:
# include path_to_Caml.mk/Caml.mk

# This Makefile uses three Makefile variables to be defined locally, in each
# directory:
# - SUB_DIRS: the list of sub-directories of the current directory,
# - CAML_GENERATED_FILES: the list of files to be removed in the target
#   [clean_all], in case we need to clean everything.
# - CAML_FILES: the list of files used to compute the dependencies,
#
# SUB_DIRS and CAML_GENERATED_FILES must be bound manually in each directory.
# In contrast, CAML_FILES is defined ``semi-automatically'' as follows:
#
# Normally, the shared Objs.mk file defines the list of byt object files for
# the current directory (listed in linking order) in two Makefile variables:
#
#<current directory name>_BYT_OBJS=\
# mdl_compile.cmo\
#
#<current directory name>_CAML_FILES=\
# mdl_compile.mli\
# $(<current directory name>_BYT_OBJS:.cmo=.ml)
#
# So that CAML_FILES is simply defined as:
#CAML_FILES=$(<current directory name>_BYT_OBJS)

.PHONY: all bin byt clean cleandir clean-all configure depend beforedepend

# Main target
all:

# Generic clean up
cleandir:
	$(RM) *.cm[ioxa] *.cmxa *.o *.a \
	 *~ .*~ a.out *.output *.annot *.obj .\#*

clean:: cleandir

clean-all::
	$(MAKE) clean
	$(RM) $(GENERATED_CAML_FILES)
	$(MAKE) depend

configure:: cleandir

# Rebuilding dependencies
depend:: beforedepend
	$(CAMLC_DEP) $(CAML_FILES) > .depend

beforedepend: $(GENERATED_CAML_FILES)

# Compilation rules
.SUFFIXES:
.SUFFIXES: .cmx .cmxa .cmo .cmi .cma .ml .mli .mlin .mliin .mlm .mlms .mll .mly .obj
.SUFFIXES: .htm .html .shtml .data .1 .man

.ml.cmo:
	$(CAMLC_BYT) -c $<
.mli.cmi:
	$(CAMLC_BYT) -c $<

.ml.cmx:
	$(CAMLC_BIN) -c $<

.mll.ml:
	$(CAMLC_LEX) $<

.mly.ml:
	$(CAMLC_YAC) $<

.mly.mli:
	$(CAMLC_YAC) $<

.mlm.ml:
	$(MOCAC) $<

.mlm.mli:
	$(MOCAC) $<

.mlms.ml:
	$(MOCAC) $<

.mlms.mli:
	$(MOCAC) $<

.data.html:
	$(RM) $@
	$(HTMLC) -i $< -o $@

.shtml.html:
	$(RM) $@
	$(HTMLC) -i $< -o $@

.html.htm:
	$(RM) $@
	$(HTMLC) -i $< -o $@

.man.1:
	$(RM) $@
	$(HTMLC) -f $< -t $@

.mlin.ml:
	$(RM) $@
	$(HTMLCCONF) -i $< -o $@

include .depend
