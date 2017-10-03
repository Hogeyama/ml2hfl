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

AC_CHECK_OCAML_PKG([Z3])
if test "$OCAML_PKG_Z3" = "no"; then
  AC_MSG_ERROR([Z3 not found])
fi

AC_PATH_TOOL(CVC3, cvc3, [no], [$PATH:$PWD])
AC_SUBST(CVC3)

AC_PATH_TOOL(TRECS, trecs, [no], [$PATH:$PWD])
AC_SUBST(TRECS)

AC_PATH_TOOL(HORSAT, horsat, [no], [$PATH:$PWD])
AC_SUBST(HORSAT)

AC_PATH_TOOL(HORSAT2, horsat2, [no], [$PATH:$PWD])
AC_SUBST(HORSAT2)

AC_PATH_TOOL(HORSATP, horsatp, [no], [$PATH:$PWD])
AC_SUBST(HORSATP)

if test "$TRECS" = "no" -a "$HORSAT" = "no" -a "$HORSAT2" = "no" -a "$HORSATP" = "no"; then
  AC_MSG_ERROR([No model checker found])
fi

AC_PATH_TOOL(OMEGA, oc, [no], [$PATH:$PWD])
AC_SUBST(OMEGA)

AC_CHECK_PROG(GIT, git, git)
AC_SUBST(GIT)

OCAML_VER=`ocamlc -version`
AC_SUBST(OCAML_VER)
OCAML_MAJOR_VER=`ocamlc -version | cut -d . -f 1,2`
AC_SUBST(OCAML_MAJOR_VER)

AC_CONFIG_FILES([Makefile.config environment.ml])
AC_OUTPUT
