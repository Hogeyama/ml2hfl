include Makefile.config

.PHONY: main all byte opt lib csisat atp clean doc test

CSISAT_LIBS = -lcamlpico -lpicosat -lcamlglpk -lglpk

INCLUDES = -I $(OCAML_LIB) \
	-I $(Z3) \
	-I $(GMP) \
	-I $(ATP) \
	-I $(APRON) \
	-I $(CSISAT) \
	-I $(CSISAT_LIB) \
	-I $(OCAML_SOURCE)/bytecomp \
	-I $(OCAML_SOURCE)/driver \
	-I $(OCAML_SOURCE)/parsing \
	-I $(OCAML_SOURCE)/typing \
	-I $(OCAML_SOURCE)/utils \
	-I $(OCAML_SOURCE)/otherlibs/unix \
	-I $(OCAML_SOURCE)/otherlibs/str \
	-I $(OCAML_SOURCE)/otherlibs/bigarray \
	-I $(OCAMLGRAPH) \
	-I $(YHORN) \
	-I $(VHORN)
OCAMLFLAGS = -g -annot $(INCLUDES) -custom -cclib '$(CSISAT_LIBS)' -nostdlib -w -14
OCAMLOPTFLAGS = -annot $(INCLUDES) -cclib '$(CSISAT_LIBS)' -w -14

DEPEND += spec_parser.ml spec_lexer.ml trecs_parser.ml trecs_lexer.ml $(OCAML_SOURCE)/utils/config.ml $(OCAML_SOURCE)/parsing/lexer.ml $(OCAML_SOURCE)/parsing/linenum.ml

DOC = doc

################################################################################
# main target

NAME = mochi

main: opt
all: lib depend main

byte: $(NAME).byte
opt: $(NAME).opt
lib: ocaml csisat trecs atp vhorn yhorn


################################################################################
# bytecode and native-code compilation

MLI = CPS.mli abstract.mli automata.mli feasibility.mli refine.mli syntax.mli \
	wrapper.mli wrapper2.mli CEGAR_print.mli CEGAR_CPS.mli CEGAR_abst.mli \
	spec_parser.mli trecs_parser.mli
CMI = $(MLI:.mli=.cmi)

CMO = $(OCAML_CMO) \
	flag.cmo util.cmo id.cmo type.cmo automata.cmo \
	syntax.cmo spec.cmo spec_parser.cmo spec_lexer.cmo \
	CEGAR_type.cmo CEGAR_syntax.cmo CEGAR_print.cmo typing.cmo type_decl.cmo \
	wrapper.cmo wrapper2.cmo \
	ref_type.cmo type_check.cmo trans.cmo CEGAR_ref_type.cmo CEGAR_util.cmo \
	useless_elim.cmo inter_type.cmo type_trans.cmo vhornInterface.cmo \
	CPS.cmo CEGAR_CPS.cmo parser_wrapper.cmo \
	abstract.cmo CEGAR_abst_util.cmo \
	CEGAR_trans.cmo CEGAR_abst_CPS.cmo CEGAR_abst.cmo \
        trecs_parser.cmo trecs_lexer.cmo \
	trecs_syntax.cmo trecsInterface.cmo \
	ModelCheck_util.cmo ModelCheck_CPS.cmo ModelCheck.cmo \
	feasibility.cmo RefineDepTyp.cmo refine.cmo CEGAR.cmo \
	writeAnnot.cmo \
	eval.cmo main.cmo
CMX = $(CMO:.cmo=.cmx)
CMA = str.cma unix.cma libcsisat.cma bigarray.cma nums.cma z3.cma graph.cma $(YHORN)/yhorn.cma gmp.cma apron.cma polkaMPQ.cma atp_batch.cma vHorn.cma
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


$(NAME).byte: $(CMO) $(CMI)
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $(CMA) $(CMO)

$(NAME).opt: $(CMX) $(CMI)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ $(CMXA) $(CMX)


spec_parser.ml spec_parser.mli: spec_parser.mly
	$(OCAMLYACC) -v $<
spec_lexer.ml: spec_lexer.mll
	$(OCAMLLEX) $<

trecs_parser.ml trecs_parser.mli: trecs_parser.mly
	$(OCAMLYACC) -v $<
trecs_lexer.ml: trecs_lexer.mll
	$(OCAMLLEX) $<


# Dependencies
DEP_VHORN = CEGAR CEGAR_syntax main refine syntax trans vhornInterface writeAnnot
$(addsuffix .cmo,$(DEP_VHORN)): $(VHORN)/vHorn.cmi
$(addsuffix .cmx,$(DEP_VHORN)): $(VHORN)/vHorn.cmi


# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<



################################################################################
# libraries

$(OCAML_SOURCE)/config/Makefile:
	cd $(OCAML_SOURCE) && ./configure
$(OCAML_SOURCE)/utils/config.ml: $(OCAML_SOURCE)/config/Makefile
	cd $(OCAML_SOURCE); make utils/config.ml
$(OCAML_SOURCE)/parsing/lexer.ml:
	cd $(OCAML_SOURCE); $(OCAMLLEX) parsing/lexer.mll
$(OCAML_SOURCE)/parsing/linenum.ml:
	cd $(OCAML_SOURCE); $(OCAMLLEX) parsing/linenum.mll
$(OCAML_SOURCE)/parsing/parser.mli $(OCAML_SOURCE)/parsing/parser.ml:
	cd $(OCAML_SOURCE); $(OCAMLYACC) -v parsing/parser.mly
$(OCAML_SOURCE)/bytecomp/opcodes.ml:
	cd $(OCAML_SOURCE); \
	sed -n -e '/^enum/p' -e 's/,//g' -e '/^  /p' byterun/instruct.h | \
	awk -f tools/make-opcodes > bytecomp/opcodes.ml

#csisat:
#	cd $(CSISAT) && make all GLPK="-cclib '-lglpk'"
#
#atp:
#	-patch -d atp -N < atp_patch
#	cd $(ATP) && make compiled bytecode
#
#vhorn:
#	cd $(VHORN) && make all
#
#yhorn:
#	cd $(YHORN) && make all


################################################################################
# distribution

dist:
	tar czvf dist.tar.gz *.ml *.mli Makefile


################################################################################
# documents

doc:
	mkdir -p $(DOC)
	ocamldoc -html -d $(DOC) $(MLI)
	perl -pi -e 's/charset=iso-8859-1/charset=utf8/' $(DOC)/*.html


################################################################################
# clean

clean:
	rm -f *.cm[ioxt] *.cmti *.o *.a *.annot *~
	rm -f spec_parser.ml spec_parser.mli spec_lexer.ml trecs_parser.ml trecs_parser.mli trecs_lexer.ml
	rm -f $(NAME).byte $(NAME).opt


################################################################################
# test

TEST = sum mult max mc91 ack a-cppr l-zipunzip l-zipmap hors e-simple e-fact r-lock r-file sum_intro copy_intro fact_notpos fold_right forall_eq_pair forall_leq isnil iter length mem nth nth0 harmonic fold_left zip map_filter risers search fold_fun_list fact_notpos-e harmonic-e map_filter-e search-e
LIMIT = 120
OPTION = -gchi -only-result

test: opt
	for i in $(TEST); \
	do \
	echo $$i; \
	(./mochi.opt test_pepm/$$i.ml $(OPTION) 2> /dev/null || echo VERIFICATION FAILED!!!); \
	echo; \
	done


################################################################################
# depend

SRC = $(CMO:.cmo=.ml)
SRC_MOCHI = $(filter-out $(ATP)%, $(filter-out $(OCAML_SOURCE)%, $(SRC)))

depend:: $(DEPEND)
	$(OCAMLDEP) $(MLI) $(SRC_MOCHI) > depend

-include depend
-include ocaml.depend
