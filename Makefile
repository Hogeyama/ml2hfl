include Makefile.config

.PHONY: main all byte opt lib ocaml csisat atp clean clean-doc clean-ocaml clean-csisat clean-all doc test

# OCAMLC       = $(OCAML_SOURCE)/ocamlc.opt
# OCAMLOPT     = $(OCAML_SOURCE)/ocamlopt.opt
# OCAMLMKTOP   = $(OCAML_SOURCE)/tools/ocamlmktop
# OCAMLDEP     = $(OCAML_SOURCE)/tools/ocamldep.opt
# OCAMLLIB     = $(OCAML_SOURCE)/stdlib
# OCAMLLEX     = $(OCAML_SOURCE)/lex/ocamllex.opt
# OCAMLYACC    = $(OCAML_SOURCE)/yacc/ocamlyacc.opt
OCAMLC       = ocamlc
OCAMLOPT     = ocamlopt
OCAMLMKTOP   = ocamlmktop
OCAMLDEP     = ocamldep
OCAMLLEX     = ocamllex
OCAMLYACC    = ocamlyacc

CSISAT_LIB = -lcamlpico -lpicosat -lcamlglpk -lglpk

INCLUDES = -I /usr/lib \
	-I /usr/lib/ocaml \
	-I /usr/local/lib \
	-I $(GMP) \
	-I $(ATP) \
	-I $(APRON) \
	-I $(CSISAT)/lib \
	-I $(CSISAT)/obj \
	-I $(OCAML_SOURCE)/bytecomp \
	-I $(OCAML_SOURCE)/driver \
	-I $(OCAML_SOURCE)/parsing \
	-I $(OCAML_SOURCE)/typing \
	-I $(OCAML_SOURCE)/utils \
	-I $(OCAML_SOURCE)/otherlibs/unix \
	-I $(OCAML_SOURCE)/otherlibs/str \
	-I $(OCAML_SOURCE)/otherlibs/bigarray \
	-I $(VHORN) \
	-I $(YINT) \
	-I $(TRECS)
#	-I $(OCAMLLIB)
OCAMLFLAGS = -g -dtypes $(INCLUDES) -custom -cclib '$(CSISAT_LIB)' -nostdlib -w -14
OCAMLOPTFLAGS = -dtypes $(INCLUDES) -cclib '$(CSISAT_LIB)' -w -14

DOC = doc

################################################################################
# main target

NAME = mochi

main: opt
all: lib depend main

byte: $(NAME).byte
opt: $(NAME).opt
byte2: $(NAME).byte2
opt2: $(NAME).opt2
lib: ocaml csisat trecs atp vhorn yint


################################################################################
# bytecode and native-code compilation

MLI = CPS.mli abstract.mli automata.mli feasibility.mli refine.mli syntax.mli \
	wrapper.mli wrapper2.mli CEGAR_print.mli CEGAR_CPS.mli CEGAR_abst.mli \
	spec_parser.mli trecs_parser.mli
CMI = $(MLI:.mli=.cmi)

VHORN_CMO = \
	enum.cmo extList.cmo extString.cmo \
	global.cmo util.cmo zipper.cmo \
	attr.cmo idnt.cmo const.cmo var.cmo simType.cmo term.cmo linArith.cmo poly.cmo parLinArith.cmo typTerm.cmo formula.cmo typSubst.cmo formulaUtil.cmo \
	cvc3Interface.cmo interpProver.cmo csisatInterface.cmo yintInterface.cmo atpInterface.cmo apronInterface.cmo \
	atom.cmo hornClause.cmo hornClauseUtil.cmo typPredSubst.cmo \
	farkas.cmo nonLinConstrSolve.cmo \
	hcSolve.cmo hcBwSolve.cmo hcGenSolve.cmo \
	fdef.cmo prog.cmo \
	callId.cmo callTree.cmo \
	trace.cmo compTree.cmo compTreeExpander.cmo \
	paramSubstInfer.cmo \
	refType.cmo hcGenRefType.cmo refTypeInfer.cmo \
	absType.cmo absTypeInfer.cmo

CMO = $(OCAML_CMO) \
	$(ATP)/atp_batch.cmo \
	$(addprefix $(VHORN)/,$(VHORN_CMO)) \
	flag.cmo utilities.cmo id.cmo type.cmo automata.cmo \
	syntax.cmo spec.cmo spec_parser.cmo spec_lexer.cmo \
	CEGAR_type.cmo CEGAR_syntax.cmo CEGAR_print.cmo typing.cmo type_decl.cmo \
	wrapper.cmo wrapper2.cmo \
	ref_type.cmo type_check.cmo trans.cmo CEGAR_ref_type.cmo CEGAR_util.cmo \
	useless_elim.cmo inter_type.cmo type_trans.cmo vhornInterface.cmo \
	CPS.cmo CEGAR_CPS.cmo parser_wrapper.cmo \
	abstract.cmo CEGAR_abst_util.cmo \
	CEGAR_trans.cmo CEGAR_abst_CPS.cmo CEGAR_abst.cmo \
        trecs_parser.cmo trecs_lexer.cmo \
	$(TRECS)/trecs.cmo dependent_type.cmo trecsInterface.cmo \
	ModelCheck_util.cmo ModelCheck_CPS.cmo ModelCheck.cmo \
	feasibility.cmo RefineDepTyp.cmo refine.cmo CEGAR.cmo \
	writeAnnot.cmo \
	eval.cmo main.cmo
CMX = $(CMO:.cmo=.cmx)
CMA = str.cma unix.cma libcsisat.cma bigarray.cma nums.cma $(YINT)/yint.cma gmp.cma apron.cma polka.cma
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
ocaml: $(OCAML_SOURCE)/config/Makefile
	cd $(OCAML_SOURCE) && make world opt world.opt opt.opt

csisat:
	cd $(CSISAT) && make all GLPK="-cclib '-lglpk'"

atp:
	-patch -d atp -N < atp_patch
	cd $(ATP) && make compiled bytecode

vhorn:
	cd $(VHORN) && make all

yint:
	cd $(YINT) && make all


# TODO: refine & write rule for bytecode
trecs::
	cd $(TRECS) && ocamlyacc parser.mly
	cd $(TRECS) && ocamllex lexer.mll
	cd $(TRECS) && ocamlopt -for-pack Trecs -c utilities.ml syntax.ml parser.mli parser.ml lexer.ml grammar.ml automaton.ml conversion.ml typing.ml stype.ml reduce.ml generalize.ml
	cd $(TRECS) && ocamlopt -pack -o trecs.cmx utilities.cmx syntax.cmx parser.cmx lexer.cmx grammar.cmx automaton.cmx conversion.cmx typing.cmx stype.cmx reduce.cmx generalize.cmx

trecs-byte::
	cd $(TRECS) && ocamlyacc parser.mly
	cd $(TRECS) && ocamllex lexer.mll
	cd $(TRECS) && ocamlc -for-pack Trecs -c utilities.ml syntax.ml parser.mli parser.ml lexer.ml grammar.ml automaton.ml conversion.ml typing.ml stype.ml reduce.ml generalize.ml
	cd $(TRECS) && ocamlc -pack -o trecs.cmo utilities.cmo syntax.cmo parser.cmo lexer.cmo grammar.cmo automaton.cmo conversion.cmo typing.cmo stype.cmo reduce.cmo generalize.cmo




################################################################################
# distribution

dist:
	tar czvf dist.tar.gz *.ml *.mli Makefile depend


################################################################################
# documents

doc:
	mkdir -p $(DOC)
	ocamldoc -html -d $(DOC) $(MLI)
	perl -pi -e 's/charset=iso-8859-1/charset=utf8/' $(DOC)/*.html


################################################################################
# clean

clean:
	rm -f *.cm[iox] *.o *.a *.annot *~
	rm -f $(NAME).byte $(NAME).opt

clean-ocaml:
	cd $(OCAML_SOURCE); make clean

clean-csisat:
	cd $(CSISAT); make clean

clean-trecs:
	cd $(TRECS); make clean

clean-doc:
	rm -rf doc

clean-all: clean clean-doc clean-ocaml clean-csisat
	rm -f .depend


################################################################################
# test

TEST=test_new/*.ml
LIMIT=120
OPTION=

test: opt
	for i in $(TEST); \
	do echo $$i; \
	timeout -s 14 $(LIMIT) ./$(NAME).opt $(OPTION) $$i 2>&1 | \
	  egrep 'Safe|Unsafe|cycle:|Verification|Fatal' | \
	  grep -v File | \
	  grep -v Warning; \
	echo; \
	done
test2: opt
	for i in $(TEST); \
	do echo $$i; \
	(ulimit -t $(LIMIT); \
	./$(NAME).opt $(OPTION) $$i 2>&1 | \
	  egrep 'Safe|Unsafe|cycle:|Verification|Fatal') | \
	  grep -v File | \
	  grep -v Warning; \
	echo; \
	done
test-byte: byte
	for i in $(TEST); \
	do echo $$i; \
	(ulimit -t $(LIMIT); ./$(NAME).byte $$i | egrep 'Safe|Unsafe|cycle:') 2>&1 | \
	  grep -v File | \
	  grep -v Warning; \
	echo; \
	done


################################################################################
# depend

SRC = $(CMO:.cmo=.ml)
SRC_MOCHI = $(filter-out $(ATP)%, $(filter-out $(TRECS)%, $(filter-out $(OCAML_SOURCE)%, $(SRC))))

depend::
	$(OCAMLDEP) -I $(VHORN) $(YINT) $(MLI) $(SRC_MOCHI) > depend

-include depend
