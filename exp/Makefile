include ../Makefile.config

.PHONY: main all byte opt clean doc test

PACKAGES = str,unix,yojson

INCLUDES =

OCAMLCFLAGS = -g -annot $(INCLUDES) -package $(PACKAGES)
OCAMLOPTFLAGS = -annot $(INCLUDES) -package $(PACKAGES)

################################################################################
# main target

NAME = manager

main: depend opt
all: depend byte opt

byte: $(NAME).byte
opt: $(NAME).opt



################################################################################
# bytecode and native-code compilation

MLI =
CMI = $(MLI:.mli=.cmi)

CMO = util.cmo markdown.cmo env.cmo manager_util.cmo programs.cmo options.cmo exp.cmo pages.cmo main.cmo
CMX = $(CMO:.cmo=.cmx)
CMA =
CMXA = $(CMA:.cma=.cmxa)


$(NAME).byte: $(CMO)
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -linkpkg -o $@ $(CMA) $(CMO)

$(NAME).opt: $(CMX)
	$(OCAMLFIND) ocamlopt $(OCAMLOPTFLAGS) -linkpkg -o $@ $(CMXA) $(CMX)



# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -c $<

.mli.cmi:
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -c $<

.ml.cmx:
	$(OCAMLFIND) ocamlopt $(OCAMLOPTFLAGS) -c $<



################################################################################
# clean

clean:
	rm -f *.cm[ioxt] *.cmti *.o *.a *.annot *~
	rm -f depend $(NAME).byte $(NAME).opt



################################################################################
# depend

SRC = $(CMO:.cmo=.ml)

depend: $(SRC)
	$(OCAMLFIND) ocamldep $(MLI) $(SRC) > depend

-include depend
