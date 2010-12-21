.PHONY: all all-nc clean-all ocaml test

CSISAT = ./csisat-read-only
OCAML_SOURCE = ocaml-3.11.2
OSP_PATH = $(OCAML_SOURCE)/parsing
OSU_PATH = $(OCAML_SOURCE)/utils
OST_PATH = $(OCAML_SOURCE)/typing
OSB_PATH = $(OCAML_SOURCE)/bytecomp
OSD_PATH = $(OCAML_SOURCE)/driver

OCAML_SOURCES_UTILS = misc.ml warnings.ml config.ml clflags.ml tbl.ml consistbl.ml ccomp.ml
OCAML_SOURCES_PARSING = asttypes.mli linenum.mli linenum.mll longident.mli parsetree.mli \
	location.mli location.ml longident.ml printast.mli printast.ml \
	syntaxerr.mli syntaxerr.ml parser.mly lexer.mli lexer.mll parse.mli parse.ml
OCAML_SOURCES_TYPING = ident.ml path.ml primitive.ml types.ml btype.ml subst.ml predef.ml \
	annot.mli datarepr.ml env.ml ctype.ml oprint.ml \
	printtyp.ml typetexp.ml typedtree.ml includecore.ml stypes.ml parmatch.ml typecore.ml \
	includeclass.ml typedecl.ml typeclass.ml mtype.ml includemod.ml typemod.ml \
	unused_var.ml
OCAML_SOURCES_BYTECOMP = lambda.ml printlambda.ml typeopt.ml switch.ml \
	matching.ml translobj.ml translcore.ml translclass.ml translmod.ml \
	opcodes.ml instruct.ml emitcode.ml printinstr.ml bytegen.ml simplif.ml
OCAML_SOURCES_DRIVER = pparse.ml compile.ml
SOURCES =  $(addprefix $(OSU_PATH)/,$(OCAML_SOURCES_UTILS)) \
	$(addprefix $(OSP_PATH)/,$(OCAML_SOURCES_PARSING)) \
	$(addprefix $(OST_PATH)/,$(OCAML_SOURCES_TYPING)) \
	$(addprefix $(OSB_PATH)/,$(OCAML_SOURCES_BYTECOMP)) \
	$(addprefix $(OSD_PATH)/,$(OCAML_SOURCES_DRIVER)) \
	flag.ml util.ml utilities.ml automata.ml syntax.ml parser_wrapper.ml alpha.ml typing.ml \
	wrapper.ml CPS.ml abstract.ml check.ml feasibility.ml infer.ml refine.ml main.ml

LIBS = unix libcsisat str
CLIBS = camlpico picosat camlglpk glpk
INCDIRS = $(CSISAT)/lib $(CSISAT)/obj
RESULT = mc
OCAMLFLAGS= -dtypes
YFLAGS = -v
CFLAGS = -g

include OCamlMakefile

all-dc:
	make ocaml && make dc

all-nc: 
	make ocaml && make nc

ocaml:
	cd $(OCAML_SOURCE) && ./configure && make world

clean-all:
	make clean
	make SOURCES="$(RUNTIMESOURCES)" clean
	-rm -rf *~
	-rm -rf *.dot
	-rm -rf *.output
	-rm -rf *.annot
	-rm -rf *.a *.cma *.cmxa




TEST = bcopy_print.ml fact_exn.ml array_init2.ml file.ml map_map.ml ack.ml repeat_eqn.ml \
	recursive.ml hors2.ml zip4.ml zip_unzip.ml zip_map2.ml inc.ml bcopy4.ml \
	dotprod2.ml inc4.ml bcopy5.ml dotprod4.ml

test:
	ulimit -t 10; for i in $(TEST); do echo $$i; ./mc test/$$i | egrep '(Safe|Unsafe)'; echo;  done

