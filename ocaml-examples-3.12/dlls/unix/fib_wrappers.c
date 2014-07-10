/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*               Pierre Weis, projet Cristal, INRIA Rocquencourt       */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  only by permission.                                                */
/*                                                                     */
/***********************************************************************/

/* $Id: fib_wrappers.c,v 1.2 2009-11-15 19:08:39 weis Exp $ */

/* To call registered Caml functions from C, the simplest way is to define C
   functions that perform the mandatory interfacing machinery between C and
   Caml and appear as completely regular C functions to the rest of the C
   program.

   These C functions hiding the additional code necessary to call Caml
   functions are named C {i wrappers}.

   For instance, the Caml [fib] function defined and registered in file
   [fib.ml] would have a natural C definition along those lines:

   int fib(int n) {
     ...
   }

   A wrapper is such a C function definition where ... is replaced by a
   call to the Caml callback function.

   The body of a wrapper always follow the same template:
   we first call the (Caml runtime provided) [caml_named_value] function
   to get the functional value associated with the call back name;
   then we apply the functional value to the proper argument(s), using the
   (Caml runtime provided) [callback] function.

   Note that around the Caml function call, we have to properly convert
   values from and to the C world via (Caml runtime provided) conversion
   macros.

   A working template for a wrapper for a simple [int -> int] Caml callback
   [f] could be:

   int f(int n) {
     value * f_caml_value = NULL;
     f_caml_value = caml_named_value("f");
     return Int_val(callback(*f_caml_value, Val_int(n)));
   }

   In fact the Caml value associated to a Caml call back can never change; so
   it is useless to retrieve this value over and over at each call to the
   wrapper function; hence, we declare the Caml value as a {i static}
   variable, so that it is retrieved once and for all at the first call to
   the C wrapper. We finally obtain the slightly more efficient wrapper
   template:

   int f(int n) {
     static value * f_caml_value = NULL;
     if (fib_value == NULL) f_caml_value = caml_named_value("f");
     return Int_val(callback(*f_caml_value, Val_int(n)));
   }

   More complex wrappers are necessary to deal with allocated values and to
   happily live with the Caml garbagge collector.
   For instance, see below the wrapper for [format_result] that must use
   [strdup] to copy a Caml allocated string to the C (heap) memory and
   prevent subsequent memory faults in C.
*/

/* C wrappers around the Caml functions exported by [fib.ml]. */

#include <stdio.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>

int fib(int n) {
  static value * fib_closure = NULL;
  if (fib_closure == NULL) fib_closure = caml_named_value("fib");
  return Int_val(callback(*fib_closure, Val_int(n)));
}

char * format_result(int n) {
  static value * format_result_closure = NULL;
  if (format_result_closure == NULL)
    format_result_closure = caml_named_value("format_result");
  return strdup(String_val(callback(*format_result_closure, Val_int(n))));
  /* We copy the C string returned by String_val to the C heap
     so that it remains valid after a garbage collection cycle. */
}
