AC_INIT(MoCHi,0.1)

AC_PROG_CC

AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
  AC_MSG_ERROR([OCaml compiler not found])
fi

AC_PROG_FINDLIB
if test "$OCAMLFIND" = "no"; then
  AC_MSG_ERROR([ocamlfind not found])
fi

AC_PROG_OCAMLLEX
if test "$OCAMLLEX" = "no"; then
  AC_MSG_ERROR([ocamllex not found])
fi

AC_PROG_OCAMLYACC
if test "$OCAMLYACC" = "no"; then
  AC_MSG_ERROR([ocamlyacc not found])
fi

AC_CHECK_OCAML_PKG([ocamlgraph])
if test "$OCAML_PKG_ocamlgraph" = "no"; then
  AC_MSG_ERROR([ocamlgraph not found])
fi

AC_CHECK_OCAML_PKG([fpat])
if test "$OCAML_PKG_fpat" = "no"; then
  AC_MSG_ERROR([fpat not found])
else
  FPAT=`$OCAMLFIND query fpat`
  AC_SUBST(FPAT)
fi

AC_CHECK_OCAML_PKG([extlib])
if test "$OCAML_PKG_extlib" = "no"; then
  AC_MSG_ERROR([extlib not found])
fi

AC_PATH_TOOL(CVC3, cvc3, [no], [$PATH:$PWD])
if test "$CVC3" = "no"; then
  AC_MSG_ERROR([CVC3 not found])
fi
AC_SUBST(CVC3)

AC_PATH_TOOL(TRECS, trecs, [no], [$PATH:$PWD])
if test "$TRECS" = "no"; then
  AC_MSG_ERROR([TRecS not found])
fi
AC_SUBST(TRECS)

AC_CHECK_PROG(GIT, git, git)
AC_SUBST(GIT)

AC_CONFIG_FILES([Makefile.config environment.ml])
AC_OUTPUT
