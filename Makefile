include Makefile.config

.PHONY: main all byte opt clean doc test

PACKAGES = fpat,str,unix,csisat,extlib

INCLUDES = \
	-I $(OCAML_SOURCE)/bytecomp \
	-I $(OCAML_SOURCE)/driver \
	-I $(OCAML_SOURCE)/parsing \
	-I $(OCAML_SOURCE)/typing \
	-I $(OCAML_SOURCE)/utils \
	-I $(OCAML_SOURCE)/otherlibs/unix \
	-I $(OCAML_SOURCE)/otherlibs/str \
	-I $(OCAML_SOURCE)/otherlibs/bigarray

OCAMLCFLAGS = -g -annot $(INCLUDES) -package $(PACKAGES)
OCAMLOPTFLAGS = -annot $(INCLUDES) -package $(PACKAGES)

DEPEND += $(OCAML_SOURCE)/utils/config.ml $(OCAML_SOURCE)/parsing/lexer.ml $(OCAML_SOURCE)/parsing/linenum.ml

DOC = doc

################################################################################
# main target

NAME = mochi

main: opt
all: depend byte opt

byte: $(NAME).byte
opt: $(NAME).opt


ifdef GIT
main: COMMIT
COMMIT: depend .git/index
	rm -f COMMIT
	if [ $$(${GIT} diff | wc -w) != 0 ]; then echo -n _ > COMMIT; fi
	echo -n `$(GIT) rev-parse --short HEAD` >> COMMIT
	echo -n ' (' >> COMMIT
	if [ $$(${GIT} diff | wc -w) != 0 ]; then echo -n 'after ' >> COMMIT; fi
	$(GIT) log --date=iso --pretty=format:"%ad" -1 >> COMMIT
	echo ')' >> COMMIT
endif



################################################################################
# bytecode and native-code compilation

MLI = lift.mli CPS.mli curry.mli abstract.mli feasibility.mli refine.mli syntax.mli \
	CEGAR_print.mli CEGAR_CPS.mli CEGAR_abst.mli \
	spec_parser.mli trecs_parser.mli BRA_transform.mli CEGAR_lift.mli id.mli
CMI = $(MLI:.mli=.cmi)

CMO = $(OCAML_CMO) \
	environment.cmo flag.cmo util.cmo id.cmo type.cmo \
	syntax.cmo spec.cmo spec_parser.cmo spec_lexer.cmo \
	CEGAR_type.cmo CEGAR_syntax.cmo CEGAR_print.cmo typing.cmo type_decl.cmo \
	ref_type.cmo type_check.cmo trans.cmo lift.cmo CEGAR_ref_type.cmo CEGAR_util.cmo CEGAR_lift.cmo \
	useless_elim.cmo inter_type.cmo type_trans.cmo fpatInterface.cmo \
	CPS.cmo curry.cmo CEGAR_CPS.cmo parser_wrapper.cmo \
	abstract.cmo CEGAR_abst_util.cmo \
	CEGAR_trans.cmo CEGAR_abst_CPS.cmo CEGAR_abst.cmo \
        trecs_parser.cmo trecs_lexer.cmo \
	trecs_syntax.cmo trecsInterface.cmo \
	ModelCheck_util.cmo ModelCheck_CPS.cmo ModelCheck.cmo \
	feasibility.cmo refine.cmo CEGAR.cmo \
	writeAnnot.cmo \
	BRA_types.cmo BRA_util.cmo BRA_state.cmo BRA_transform.cmo \
	extraClsDepth.cmo extraParamInfer.cmo \
	eval.cmo main_loop.cmo termination_loop.cmo main.cmo
CMX = $(CMO:.cmo=.cmx)
CMA =
CMXA = $(CMA:.cma=.cmxa)


OCAML_UTILS_CMO = misc.cmo warnings.cmo config.cmo clflags.cmo tbl.cmo consistbl.cmo ccomp.cmo
OCAML_PARSING_CMO = linenum.cmo location.cmo longident.cmo printast.cmo \
	syntaxerr.cmo parser.cmo lexer.cmo parse.cmo
OCAML_TYPING_CMO = ident.cmo path.cmo primitive.cmo types.cmo btype.cmo subst.cmo predef.cmo \
	datarepr.cmo env.cmo ctype.cmo oprint.cmo \
	printtyp.cmo typetexp.cmo typedtree.cmo includecore.cmo stypes.cmo parmatch.cmo typecore.cmo \
	includeclass.cmo typedecl.cmo typeclass.cmo mtype.cmo includemod.cmo typemod.cmo \
	unused_var.cmo
OCAML_BYTECOMP_CMO = lambda.cmo printlambda.cmo typeopt.cmo switch.cmo \
	matching.cmo translobj.cmo translcore.cmo translclass.cmo translmod.cmo \
	opcodes.cmo instruct.cmo emitcode.cmo printinstr.cmo bytegen.cmo simplif.cmo
OCAML_DRIVER_CMO = pparse.cmo compile.cmo
OCAML_CMO = $(addprefix $(OCAML_SOURCE)/utils/,$(OCAML_UTILS_CMO)) \
	$(addprefix $(OCAML_SOURCE)/parsing/,$(OCAML_PARSING_CMO)) \
	$(addprefix $(OCAML_SOURCE)/typing/,$(OCAML_TYPING_CMO)) \
	$(addprefix $(OCAML_SOURCE)/bytecomp/,$(OCAML_BYTECOMP_CMO)) \
	$(addprefix $(OCAML_SOURCE)/driver/,$(OCAML_DRIVER_CMO))


$(NAME).byte: $(CMO)
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -linkpkg -o $@ $(CMA) $(CMO)

$(NAME).opt: $(CMX)
	$(OCAMLFIND) ocamlopt $(OCAMLOPTFLAGS) -linkpkg -o $@ $(CMXA) $(CMX)


spec_parser.ml spec_parser.mli: spec_parser.mly
	$(OCAMLYACC) -v $<
spec_lexer.ml: spec_lexer.mll
	$(OCAMLLEX) $<

trecs_parser.ml trecs_parser.mli: trecs_parser.mly
	$(OCAMLYACC) -v $<
trecs_lexer.ml: trecs_lexer.mll
	$(OCAMLLEX) $<


# Dependencies
DEP_FPAT = CEGAR CEGAR_syntax CEGAR_abst_util feasibility main \
	main_loop termination_loop refine syntax trans fpatInterface writeAnnot BRA_types
$(addsuffix .cmo,$(DEP_FPAT)): $(FPAT)/fpat.cmi
$(addsuffix .cmx,$(DEP_FPAT)): $(FPAT)/fpat.cmi


# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -c $<

.mli.cmi:
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -c $<

.ml.cmx:
	$(OCAMLFIND) ocamlopt $(OCAMLOPTFLAGS) -c $<



################################################################################
# libraries

$(OCAML_SOURCE)/config/Makefile:
	cd $(OCAML_SOURCE) && ./configure
	cd $(OCAML_SOURCE) && make coldstart
	mkdir -p stdlib
	cp $(OCAML_SOURCE)/stdlib/*.cmi stdlib
	cd $(OCAML_SOURCE) && make clean
$(OCAML_SOURCE)/utils/config.ml: $(OCAML_SOURCE)/config/Makefile
	cd $(OCAML_SOURCE) && make utils/config.ml
$(OCAML_SOURCE)/parsing/lexer.ml:
	cd $(OCAML_SOURCE) && $(OCAMLLEX) parsing/lexer.mll
$(OCAML_SOURCE)/parsing/linenum.ml:
	cd $(OCAML_SOURCE) && $(OCAMLLEX) parsing/linenum.mll
$(OCAML_SOURCE)/parsing/parser.mli $(OCAML_SOURCE)/parsing/parser.ml:
	cd $(OCAML_SOURCE) && $(OCAMLYACC) -v parsing/parser.mly
$(OCAML_SOURCE)/bytecomp/opcodes.ml:
	cd $(OCAML_SOURCE) && \
	sed -n -e '/^enum/p' -e 's/,//g' -e '/^  /p' byterun/instruct.h | \
	awk -f tools/make-opcodes > bytecomp/opcodes.ml


################################################################################
# distribution

ifdef GIT
.PHONY: dist

dist:
	$(GIT) archive HEAD -o dist.tar.gz
endif


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
	rm -f spec_parser.ml spec_parser.mli spec_lexer.ml trecs_parser.ml trecs_parser.mli trecs_lexer.ml
	rm -f $(NAME).byte $(NAME).opt

clean-test:
	rm */*.trecs_out */*.hors */*.annot

clean-all: clean
	cd $(OCAML_SOURCE) && make clean
	rm -f $(OCAML_SOURCE)/config/Makefile
	rm -rf stdlib


################################################################################
# test

TEST = sum mult max mc91 ack a-cppr l-zipunzip l-zipmap hors e-simple e-fact r-lock r-file sum_intro copy_intro fact_notpos fold_right forall_eq_pair forall_leq isnil iter length mem nth nth0 harmonic fold_left zip map_filter risers search fold_fun_list fact_notpos-e harmonic-e map_filter-e search-e
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
SRC_MOCHI = $(filter-out $(OCAML_SOURCE)%, $(SRC))

depend: $(SRC_MOCHI) $(DEPEND)
	$(OCAMLFIND) ocamldep $(MLI) $(SRC_MOCHI) > depend

-include depend
-include ocaml.depend
