BASICS_BYT_OBJS=\
 basics/lib_pp.cmo\
 basics/ident.cmo\
 basics/name.cmo\
 basics/path.cmo\
 basics/hash_table.cmo\
 basics/hash_table_print.cmo\
 basics/ulist1.cmo\
 basics/mdl_basics_print.cmo\

BASICS_MLI_FILES=\
 basics/lib_pp.mli\
 basics/ident.mli\
 basics/name.mli\
 basics/path.mli\
 basics/hash_table.mli\
 basics/hash_table_print.mli\
 basics/ulist1.mli\
 basics/mdl_basics.mli\
 basics/mdl_basics_print.mli\

BASICS_ML_FILES=\
 $(BASICS_BYT_OBJS:.cmo=.ml)

POLISH_BYT_OBJS=\
 ast_print.cmo\
 ast_pprint.cmo\
 lexer.cmo\
 parser.cmo\
 scoping.cmo\
 transl.cmo\
 deep_ast_print.cmo\
 deep_ast_pprint.cmo\
 splice_ast_print.cmo\
 splice_ast_pprint.cmo\
 splicing.cmo\
 polish.cmo\
 print_polish.cmo\
 treat_polish.cmo\
 main_polish.cmo\

POLISH_MLI_FILES=\
 ast.mli\
 ast_print.mli\
 deep_ast_print.mli\
 deep_ast_pprint.mli\
 scoping.mli\
 deep_ast.mli\
 ast_print.mli\
 splice_ast_print.mli\
 splice_ast_pprint.mli\
 treat_polish.mli\

POLISH_ML_FILES=\
 $(POLISH_BYT_OBJS:.cmo=.ml)

ML_FILES=\
 $(BASICS_ML_FILES)\
 $(POLISH_ML_FILES)

MLI_FILES=\
 $(BASICS_MLI_FILES)\
 $(POLISH_MLI_FILES)

CAML_FILES=\
 $(MLI_FILES)\
 $(ML_FILES)\
