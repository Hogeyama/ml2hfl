
.PHONY: main all byte opt lib ocaml csisat clean clean-doc clean-ocaml clean-csisat clean-all doc test

OCAML_SOURCE = ocaml-3.12
CSISAT = csisat-read-only

OCAMLC       = $(OCAML_SOURCE)/ocamlc.opt
OCAMLOPT     = $(OCAML_SOURCE)/ocamlopt.opt
OCAMLMKTOP   = $(OCAML_SOURCE)/tools/ocamlmktop
OCAMLDEP     = $(OCAML_SOURCE)/tools/ocamldep.opt
OCAMLLIB     = $(OCAML_SOURCE)/stdlib
OCAMLLEX     = $(OCAML_SOURCE)/lex/ocamllex.opt
OCAMLYACC    = $(OCAML_SOURCE)/yacc/ocamlyacc.opt

CSISAT_LIB = -lcamlpico -lpicosat -lcamlglpk -lglpk

INCLUDES = -I $(OCAML_SOURCE)/bytecomp \
	-I $(OCAML_SOURCE)/driver \
	-I $(OCAML_SOURCE)/parsing \
	-I $(OCAML_SOURCE)/typing \
	-I $(OCAML_SOURCE)/utils \
	-I /usr/lib \
	-I /usr/local/lib \
	-I $(CSISAT)/lib \
	-I $(CSISAT)/obj \
	-I $(OCAMLLIB) \
	-I $(OCAML_SOURCE)/otherlibs/unix \
	-I $(OCAML_SOURCE)/otherlibs/str \
	-I $(OCAML_SOURCE)/otherlibs/bigarray \
	-I ./lazy
OCAMLFLAGS = -w p -g -dtypes $(INCLUDES) -custom -cclib '$(CSISAT_LIB)' -nostdlib
OCAMLOPTFLAGS = -dtypes $(INCLUDES) -cclib '$(CSISAT_LIB)'

DOC = doc

################################################################################
# main target

NAME = mochi

main: byte opt
all: lib depend main

byte: $(NAME).byte
opt: $(NAME).opt
opt2: $(NAME)2.opt
lib: ocaml csisat


################################################################################
# bytecode and native-code compilation

MLI = CPS.mli abstract.mli alpha.mli automata.mli feasibility.mli refine.mli syntax.mli wrapper.mli wrapper2.mli
CMO = $(addprefix $(OCAML_SOURCE)/utils/,$(OCAML_UTILS_CMO)) \
	$(addprefix $(OCAML_SOURCE)/parsing/,$(OCAML_PARSING_CMO)) \
	$(addprefix $(OCAML_SOURCE)/typing/,$(OCAML_TYPING_CMO)) \
	$(addprefix $(OCAML_SOURCE)/bytecomp/,$(OCAML_BYTECOMP_CMO)) \
	$(addprefix $(OCAML_SOURCE)/driver/,$(OCAML_DRIVER_CMO)) \
	flag.cmo utilities.cmo id.cmo type.cmo automata.cmo syntax.cmo \
        CEGAR_type.cmo CEGAR_syntax.cmo CEGAR_print.cmo typing.cmo CEGAR_util.cmo \
 lazy/enum.cmo lazy/extList.cmo lazy/extString.cmo \
 lazy/util.cmo \
 lazy/attr.cmo lazy/idnt.cmo lazy/const.cmo lazy/var.cmo lazy/simType.cmo lazy/arith.cmo lazy/term.cmo \
 lazy/apronInterface.cmo lazy/csisatInterface.cmo lazy/cvc3Interface.cmo lazy/fdef.cmo lazy/prog.cmo lazy/ctree.cmo lazy/trace.cmo lazy/sizType.cmo lazy/refType.cmo lazy/absType.cmo lazy/verifier.cmo \
 lazyInterface.cmo \
	type_decl.cmo type_check.cmo CPS.cmo CEGAR_CPS.cmo parser_wrapper.cmo \
	wrapper.cmo wrapper2.cmo abstract.cmo CEGAR_abst_util.cmo CEGAR_abst.cmo CEGAR_abst_CPS.cmo ModelCheck_util.cmo ModelCheck.cmo ModelCheck_CPS.cmo feasibility.cmo RefineDepTyp.cmo refine.cmo CEGAR.cmo main.cmo
CMOSUB = $(addprefix $(OCAML_SOURCE)/utils/,$(OCAML_UTILS_CMO)) \
	$(addprefix $(OCAML_SOURCE)/parsing/,$(OCAML_PARSING_CMO)) \
	$(addprefix $(OCAML_SOURCE)/typing/,$(OCAML_TYPING_CMO)) \
	$(addprefix $(OCAML_SOURCE)/bytecomp/,$(OCAML_BYTECOMP_CMO)) \
	$(addprefix $(OCAML_SOURCE)/driver/,$(OCAML_DRIVER_CMO)) \
	flag.cmo utilities.cmo id.cmo type.cmo automata.cmo syntax.cmo \
        CEGAR_type.cmo CEGAR_syntax.cmo CEGAR_print.cmo typing.cmo CEGAR_util.cmo \
	type_decl.cmo type_check.cmo CPS.cmo CEGAR_CPS.cmo parser_wrapper.cmo \
	wrapper.cmo wrapper2.cmo abstract.cmo CEGAR_abst_util.cmo CEGAR_abst.cmo \
	CEGAR_abst_CPS.cmo ModelCheck_util.cmo ModelCheck.cmo ModelCheck_CPS.cmo \
	feasibility.cmo RefineDepTyp.cmo refine.cmo CEGAR.cmo main2.cmo
CMX = $(CMO:.cmo=.cmx)
CMXSUB = $(CMOSUB:.cmo=.cmx)
CMA = str.cma unix.cma libcsisat.cma bigarray.cma gmp.cma apron.cma polka.cma
CMASUB = str.cma unix.cma libcsisat.cma bigarray.cma gmp.cma apron.cma polka.cma
CMXA = $(CMA:.cma=.cmxa)
CMXASUB = $(CMASUB:.cma=.cmxa)


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


$(NAME).byte: $(CMO)
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $(CMA) $(CMO)

$(NAME).opt: $(CMX)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ $(CMXA) $(CMX)

$(NAME)2.opt: $(CMXSUB)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ $(CMXASUB) $(CMXSUB)


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

$(OCAML_SOURCE)/Makefile:
	cd $(OCAML_SOURCE); ./configure
ocaml: $(OCAML_SOURCE)/Makefile
	cd $(OCAML_SOURCE); make world opt world.opt opt.opt

csisat:
	cd $(CSISAT); make


################################################################################
# distribution

dist:
	tar czvf dist.tar.gz *.ml *.mli lazy/*.ml lazy/*.mli Makefile depend


################################################################################
# documents

MLI = $(wildcard *.mli)

doc:
	mkdir -p $(DOC)
	ocamldoc -html -d $(DOC) $(MLI)
	perl -pi -e 's/charset=iso-8859-1/charset=utf8/' $(DOC)/*.html


################################################################################
# clean

clean:
	rm -f *.cm[iox] *.o lazy/*.cm[iox] lazy/*.o *.a *.annot *~
	rm -f $(NAME).byte $(NAME).opt

clean-ocaml:
	cd $(OCAML_SOURCE); make clean

clean-csisat:
	cd $(CSISAT); make clean

clean-doc:
	rm -rf doc

clean-all: clean clean-doc clean-ocaml clean-csisat
	rm -f .depend


################################################################################
# test

TEST=test_new/*.ml
LIMIT=120

test: opt
	for i in $(TEST); do echo $$i; (ulimit -t $(LIMIT); ./$(NAME).opt $$i | egrep 'Safe|Unsafe|cycle:'); echo; done
test-byte: byte
	for i in $(TEST); do echo $$i; (ulimit -t $(LIMIT); ./$(NAME).byte $$i | egrep 'Safe|Unsafe|cycle:'); echo; done


################################################################################
# depend

SRC = $(CMO:.cmo=.ml)
SRCSUB = $(CMOSUB:.cmo=.ml)

depend::
	$(OCAMLDEP) $(INCLUDES) $(MLI) $(SRC) > depend
depend2::
	$(OCAMLDEP) $(INCLUDES) $(MLI) $(SRCSUB) > depend

-include depend

