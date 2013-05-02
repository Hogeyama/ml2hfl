AC_INIT(MoCHi,0.1)

AC_PROG_CC

BUILD_DIR=$PWD
AC_SUBST(BUILD_DIR)

AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
  AC_MSG_ERROR([Please install the OCaml compiler])
fi

AC_PROG_FINDLIB
if test "$OCAMLFIND" = "no"; then
  AC_MSG_ERROR([Please install OCaml findlib (the ocamlfind command)])
fi

AC_PROG_OCAMLLEX
if test "$OCAMLLEX" = "no"; then
  AC_MSG_ERROR([Please install ocamllex])
fi

AC_PROG_OCAMLYACC
if test "$OCAMLYACC" = "no"; then
  AC_MSG_ERROR([Please install ocamlyacc])
fi

AC_CHECK_OCAML_PKG([ocamlgraph])
if test "$OCAML_PKG_ocamlgraph" = "no"; then
  AC_MSG_ERROR([Please install ocamlgraph])
else
  OCAMLGRAPH=`$OCAMLFIND query ocamlgraph`
  AC_SUBST([OCAMLGRAPH])
fi

AC_CHECK_OCAML_PKG([z3])
if test "$OCAML_PKG_z3" = "no"; then
  AC_MSG_ERROR([Please install z3])
else
  Z3=`$OCAMLFIND query z3`
  AC_SUBST([Z3])
fi

AC_CHECK_OCAML_PKG([apron])
if test "$OCAML_PKG_apron" = "no"; then
  AC_MSG_ERROR([Please install apron])
else
  APRON=`$OCAMLFIND query apron`
  AC_SUBST([APRON])
fi

AC_CHECK_OCAML_PKG([gmp])
if test "$OCAML_PKG_gmp" = "no"; then
  AC_MSG_ERROR([Please install mlgmpidl])
else
  GMP=`$OCAMLFIND query gmp`
  AC_SUBST([GMP])
fi

AC_ARG_VAR(LIB_PATH, [          library search paths (separated by spaces)])

AC_ARG_WITH(csisat_obj,
   [  --with-csisat-obj=DIR   CSIsat object directory ],
   [CSISAT_DIR="$withval"])
AC_ARG_WITH(csisat_lib,
   [  --with-csisat-lib=DIR   CSIsat lib directory ], [
   LDFLAGS="$LDFLAGS -L$withval"
   CSISAT_LIB_DIR="$withval"
  ])
AC_ARG_WITH(csisat,
   [  --with-csisat=DIR       CSIsat build directory ], [
   if test -z "$with_csisat_obj" && test -z "$with_csisat_lib" ; then
      CSISAT_DIR="$withval/obj"
      LDFLAGS="$LDFLAGS -L$withval/lib"
      CSISAT_LIB_DIR="$withval/lib"
   else
      AC_MSG_FAILURE([Do not use --with-csisat and --with-csisat-include/--with-csisat-lib options simultaneously.])
   fi
  ])

AC_CHECK_OCAML_MODULE(CSISAT,CSIsat,[CsisatAst],[./csisat/obj ../csisat/obj ./csisat-read-only/obj ../csisat-read-only/obj $CSISAT_DIR $LIB_PATH])
if test "$CSISAT" = "no"; then
  AC_MSG_ERROR([CSIsat not found])
fi
AC_SUBST(CSISAT)

if test -z "$CSISAT_LIB" && test -n "$CSISAT"; then
  CSISAT_LIB_TMP=`echo $CSISAT | sed 's/obj$/lib/'`
  if test "$CSISAT_LIB" != "$CSISAT"; then
    CSISAT_LIB=$CSISAT_LIB_TMP
    LDFLAGS="$LDFLAGS -L$CSISAT_LIB"
  fi
fi

AC_CHECK_LIB(camlpico, abort, , [AC_MSG_ERROR(libcamlpico not found)])
AC_CHECK_LIB(picosat, abort, , [AC_MSG_ERROR(libpicosat not found)])
AC_CHECK_LIB(camlglpk, free, , [AC_MSG_ERROR(libcamlglpk not found)])
AC_SUBST(CSISAT_LIB)

AC_CHECK_OCAML_MODULE(ATP,ATP,[Atp_batch],[./atp ../atp ./vhorn/atp ../vhorn/atp ./VHorn/atp ../VHorn/atp $LIB_PATH])
if test "$ATP" = "no"; then
  AC_MSG_ERROR([ATP not found])
fi

AC_CHECK_OCAML_MODULE(VHORN,VHorn,[VHorn],[./vhorn ../vhorn ./VHorn ../VHorn $LIB_PATH])
if test "$VHORN" = "no"; then
  AC_MSG_ERROR([VHorn not found])
fi

AC_CHECK_OCAML_MODULE(YHORN,Yhorn,[Yhorn],[./yint ../yint ./Yint ../Yint ./yhorn ../yhorn ./Yhorn ../Yhorn $LIB_PATH])
if test "$YHORN" = "no"; then
  AC_MSG_ERROR([YHorn not found])
fi

AC_ARG_WITH(ocaml_src,
   [  --with-ocaml-src=DIR    OCaml source directory],
   [OCAML_SRC="$withval"])
if test -n "$OCAML_SRC"; then
  AC_CHECK_FILE($OCAML_SRC/configure, [HAVE_OCAML_SRC=1])
else
  AC_CHECK_FILE(./ocaml-3.12.1/configure, [
     HAVE_OCAML_SRC=1
     OCAML_SRC=./ocaml-3.12.1
  ])
  if test -z "$HAVE_OCAML_SRC"; then
    AC_CHECK_FILE(../ocaml-3.12.1/configure, [
       HAVE_OCAML_SRC=1
       OCAML_SRC=../ocaml-3.12.1
    ])
  fi
fi
if test -z "$HAVE_OCAML_SRC"; then
  AC_MSG_ERROR([OCaml source file not found])
fi
AC_MSG_CHECKING([for OCaml source version])
if test ! -e "$OCAML_SRC/VERSION"; then
  AC_MSG_RESULT([no])
  AC_MSG_ERROR([OCaml VERSION file not found])
fi
if test "`head -1 $OCAML_SRC/VERSION`" != "3.12.1"; then
  AC_MSG_RESULT([no])
  AC_MSG_ERROR([OCaml 3.12.1 is required])
fi
AC_MSG_RESULT([yes])
AC_SUBST(OCAML_SRC)

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

AC_CONFIG_FILES([Makefile.config environment.ml])
AC_OUTPUT
