include Makefile.config

.PHONY: main all byte opt top clean doc test

PACKAGES = fpat,str,unix,csisat,extlib,compiler-libs.common

INCLUDES =
FPAT_SRC_DIR = ../fpat
MOCHI_BIN_DIR = mochi_bin

OCAMLCFLAGS = -g -annot $(INCLUDES) -package $(PACKAGES)
OCAMLOPTFLAGS = -g -annot $(INCLUDES) -package $(PACKAGES)

DOC = doc

################################################################################
# main target

NAME = mochi

main: opt
all: depend byte opt top

byte: $(NAME).byte
opt: $(NAME).opt
top: $(NAME).top


ifdef GIT
main: COMMIT
COMMIT: depend .git/index $(FPAT_LIB)
	@echo make COMMIT
	@rm -f COMMIT
	@if [ $$(${GIT} diff | wc -w) != 0 ]; then echo -n _ > COMMIT; fi
	@echo -n `$(GIT) rev-parse --short HEAD` >> COMMIT
	@echo -n ' (' >> COMMIT
	@if [ $$(${GIT} diff | wc -w) != 0 ]; then echo -n 'after ' >> COMMIT; fi
	@$(GIT) log --date=iso --pretty=format:"%ad" -1 >> COMMIT
	@echo ')' >> COMMIT
	@-(cd $(FPAT_SRC_DIR) 2> /dev/null; echo -n `$(GIT) rev-parse --short HEAD`) >> COMMIT
endif



################################################################################
# bytecode and native-code compilation

MLI = lift.mli CPS.mli curry.mli encode_rec.mli encode_list.mli		\
	feasibility.mli refine.mli syntax.mli term_util.mli		\
	CEGAR_print.mli CEGAR_CPS.mli CEGAR_abst.mli spec_parser.mli	\
	horSat_parser.mli CEGAR_parser.mli BRA_transform.mli	\
	CEGAR_lift.mli tupling.mli ref_trans.mli trans.mli tree.mli	\
	rose_tree.mli type.mli color.mli CEGAR_trans.mli
CMI = $(MLI:.mli=.cmi)

CMO = environment.cmo flag.cmo util.cmo color.cmo tree.cmo		\
	rose_tree.cmo id.cmo type.cmo syntax.cmo type_decl.cmo		\
	term_util.cmo spec.cmo spec_parser.cmo spec_lexer.cmo		\
	CEGAR_type.cmo CEGAR_syntax.cmo CEGAR_print.cmo typing.cmo	\
	ref_type.cmo type_check.cmo trans.cmo lift.cmo			\
	CEGAR_ref_type.cmo CEGAR_util.cmo CEGAR_lift.cmo slicer.cmo	\
	useless_elim.cmo inter_type.cmo type_trans.cmo			\
	fpatInterface.cmo CPS.cmo curry.cmo CEGAR_CPS.cmo		\
	parser_wrapper.cmo encode_list.cmo encode_rec.cmo		\
	CEGAR_abst_util.cmo CEGAR_trans.cmo CEGAR_abst_CPS.cmo		\
	CEGAR_abst.cmo CEGAR_parser.cmo CEGAR_lexer.cmo			\
	horSat_syntax.cmo \
	horSat_parser.cmo horSat_lexer.cmo trecs_syntax.cmo		\
	horSatInterface.cmo ModelCheck_util.cmo ModelCheck.cmo		\
	feasibility.cmo refine.cmo CEGAR.cmo writeAnnot.cmo		\
	tupling.cmo ref_trans.cmo ret_fun.cmo BRA_types.cmo		\
	BRA_util.cmo BRA_state.cmo BRA_transform.cmo			\
	extraClsDepth.cmo extraParamInfer.cmo eval.cmo			\
	elim_same_arg.cmo main_loop.cmo termination_loop.cmo		\
	mochi.cmo
CMX = $(CMO:.cmo=.cmx)
CMA =
CMXA = $(CMA:.cma=.cmxa)




$(NAME).byte: $(CMO)
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -linkpkg -o $@ $(CMA) $(CMO)

$(NAME).opt: $(CMX)
	$(OCAMLFIND) ocamlopt $(OCAMLOPTFLAGS) -linkpkg -o $@ $(CMXA) $(CMX)

$(NAME).top: $(CMO)
	$(OCAMLFIND) ocamlmktop $(OCAMLCFLAGS) -linkpkg -o $@ $(CMA) $(CMO)


spec_parser.ml spec_parser.mli: spec_parser.mly
	$(OCAMLYACC) -v $<
spec_lexer.ml: spec_lexer.mll
	$(OCAMLLEX) $<

horSat_parser.ml horSat_parser.mli: horSat_parser.mly
	$(OCAMLYACC) -v $<
horSat_lexer.ml: horSat_lexer.mll
	$(OCAMLLEX) $<

CEGAR_parser.ml CEGAR_parser.mli: CEGAR_parser.mly
	$(OCAMLYACC) -v $<
CEGAR_lexer.ml: CEGAR_lexer.mll
	$(OCAMLLEX) $<


# Dependencies
DEP_FPAT = CEGAR CEGAR_syntax CEGAR_abst_util feasibility mochi \
	main_loop termination_loop refine syntax trans fpatInterface writeAnnot BRA_types
FPAT_LIB = $(FPAT)/fpat.cmi $(FPAT)/fpat.cma $(FPAT)/fpat.cmxa
$(addsuffix .cmi,$(DEP_FPAT)): $(FPAT_LIB)
$(addsuffix .cmo,$(DEP_FPAT)): $(FPAT_LIB)
$(addsuffix .cmx,$(DEP_FPAT)): $(FPAT_LIB)


# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -c $<

.mli.cmi:
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -c $<

.ml.cmx:
	$(OCAMLFIND) ocamlopt $(OCAMLOPTFLAGS) -c $<



################################################################################
# distribution

ifdef GIT
.PHONY: dist

dist:
	$(GIT) archive HEAD -o dist.tar.gz
endif

bin: $(NAME).opt
	@echo make $(MOCHI_BIN_DIR)
	@mkdir -p $(MOCHI_BIN_DIR)/bin
	@cp $(CVC3) ./horsat $(NAME).opt $(MOCHI_BIN_DIR)/bin
	@mkdir -p $(MOCHI_BIN_DIR)/lib
	@ldd $(NAME).opt | while read line; \
	do \
	   if [ $$(echo $$line | wc -w) -eq 2 ]; then \
	     cp $$(echo $$line | cut -d' ' -f1) $(MOCHI_BIN_DIR)/lib ; \
	   elif [ $$(echo $$line | wc -w) -eq 4 ]; then \
	     cp $$(echo $$line | cut -d' ' -f3) $(MOCHI_BIN_DIR)/lib ; \
	   fi; \
	done
	@mkdir -p $(MOCHI_BIN_DIR)/stdlib
	@cp $$($(OCAMLFIND) ocamlc -where)/*.cmi $(MOCHI_BIN_DIR)/stdlib

################################################################################
# documents

doc:
	mkdir -p $(DOC)
	$(OCAMLFIND) ocamldoc -html -d $(DOC) $(MLI)
	perl -pi -e 's/charset=iso-8859-1/charset=utf8/' $(DOC)/*.html


################################################################################
# clean

clean:
	rm -f *.cm[ioxt] *.cmti *.o *.a *.annot *~
	rm -f spec_parser.ml spec_parser.mli spec_lexer.ml horSat_parser.ml horSat_parser.mli horSat_lexer.ml
	rm -f $(NAME).byte $(NAME).opt

clean-test:
	rm */*.trecs_out */*.hors */*.annot


################################################################################
# test

TEST = sum mult max mc91 ack a-copy-print hors exc-simple exc-fact lock file sum_intro copy_intro fact_notpos fold_right forall_eq_pair forall_leq isnil iter length mem nth nth0 harmonic fold_left zip map_filter risers search fold_fun_list fact_notpos-e harmonic-e map_filter-e search-e
LIMIT = 120
OPTION = -bdag -mip-template -only-result -limit $(LIMIT)

test: opt
	for i in $(TEST); \
	do \
	echo $$i; \
	(ulimit -t $(LIMIT); ./mochi.opt test/$$i.ml $(OPTION) 2> /dev/null || echo VERIFICATION FAILED!!!); \
	echo; \
	done

test-error: opt
	for i in $(TEST); \
	do \
	echo $$i; \
	(ulimit -t $(LIMIT); ./mochi.opt test_pepm/$$i.ml $(OPTION) 1> /dev/null); \
	echo; \
	done


################################################################################
# depend

SRC = $(CMO:.cmo=.ml)
GENERATED = spec_parser.ml spec_parser.mli spec_lexer.ml horSat_parser.ml horSat_parser.mli horSat_lexer.ml CEGAR_parser.mli CEGAR_parser.ml CEGAR_lexer.ml

depend: Makefile $(GENERATED) $(MLI) $(SRC)
	$(OCAMLFIND) ocamldep -package $(PACKAGES) $(MLI) $(SRC) > depend

-include depend
