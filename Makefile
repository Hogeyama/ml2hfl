
.PHONY: all byte opt lib ocaml csisat clean clean-doc clean-ocaml clean-csisat clean-all doc

OCAMLC       = ocamlc
OCAMLOPT     = ocamlopt
OCAMLDEP     = ocamldep
OCAMLLIB     = /usr/lib/ocaml
OCAMLLEX     = ocamllex
OCAMLYACC    = ocamlyacc

OCAML_SOURCE = ocaml-3.11.2
CSISAT = csisat-read-only

CSISAT_LIB = -lcamlpico -lpicosat -lcamlglpk -lglpk

INCLUDES = -I $(OCAML_SOURCE)/bytecomp \
	-I $(OCAML_SOURCE)/driver \
	-I $(OCAML_SOURCE)/parsing \
	-I $(OCAML_SOURCE)/typing \
	-I $(OCAML_SOURCE)/utils \
	-I $(CSISAT)/lib \
	-I $(CSISAT)/obj
OCAMLFLAGS = -w p -g -dtypes $(INCLUDES) -custom -cclib '$(CSISAT_LIB)'
OCAMLOPTFLAGS = -dtypes $(INCLUDES) -cclib '$(CSISAT_LIB)'

DOC = doc

################################################################################
# main target

NAME = mochi

all: .depend byte opt

byte: $(NAME).byte
opt: $(NAME).opt
lib: ocaml csisat


################################################################################
# bytecode and native-code compilation

CMO = $(addprefix $(OCAML_SOURCE)/utils/,$(OCAML_UTILS_CMO)) \
	$(addprefix $(OCAML_SOURCE)/parsing/,$(OCAML_PARSING_CMO)) \
	$(addprefix $(OCAML_SOURCE)/typing/,$(OCAML_TYPING_CMO)) \
	$(addprefix $(OCAML_SOURCE)/bytecomp/,$(OCAML_BYTECOMP_CMO)) \
	$(addprefix $(OCAML_SOURCE)/driver/,$(OCAML_DRIVER_CMO)) \
	flag.cmo util.cmo utilities.cmo automata.cmo syntax.cmo \
	parser_wrapper.cmo alpha.cmo typing.cmo wrapper.cmo CPS.cmo \
	abstract.cmo check.cmo feasibility.cmo infer.cmo refine.cmo main.cmo
CMX = $(CMO:.cmo=.cmx)
CMA = str.cma unix.cma libcsisat.cma
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


$(NAME).byte: $(CMO)
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $(CMA) $(CMO)

$(NAME).opt: $(CMX)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ $(CMXA) $(CMX)


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

ocaml:
	cd $(OCAML_SOURCE); ./configure && make world opt world.opt opt.opt

csisat:
	cd $(CSISAT); make


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
	rm -f *.cm[iox] *.o *.a *.annot *~ .depend
	rm -f $(NAME).byte $(NAME).opt

clean-ocaml:
	cd $(OCAML_SOURCE); make clean

clean-csisat:
	cd $(CSISAT); make clean

clean-doc:
	rm -rf doc

clean-all: clean clean-doc clean-ocaml clean-csisat


################################################################################
# depend

SRC = $(CMO:.cmo=.ml)

.depend:
	$(OCAMLDEP) $(INCLUDES) *.mli $(SRC) > .depend

include .depend


