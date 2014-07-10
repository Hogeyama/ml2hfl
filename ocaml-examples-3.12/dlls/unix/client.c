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

/* $Id: client.c,v 1.1 2009-11-15 18:15:49 weis Exp $ */

/* An example of a C client program for the fib.dll DLL. */
#include <stdio.h>

/* Importing the functions defined in the DLL, i.e. the wrappers defined
   around the Caml functions of our [fib.ml] file. */
extern int fib(int n);
extern char * format_result(int n);

/* Using them in C code. */
int main()
{
  int res;
  char * s;

  printf("Calling fib(20) from C...\n");
  res = fib(20);
  printf("... the result is the C int %d\n", res);
  printf("Formatting this C int as a string result in Caml...\n");
  s = format_result(res);
  printf("... the result is the C string %s\n", s);
  return 0;
}
