/***********************************************************************/
/*                                                                     */
/*                        Caml examples                                */
/*                                                                     */
/*            Pierre Weis                                              */
/*                                                                     */
/*                        INRIA Rocquencourt                           */
/*                                                                     */
/*  Copyright (c) 1994-2011, INRIA                                     */
/*  All rights reserved.                                               */
/*                                                                     */
/*  Distributed under the BSD license.                                 */
/*                                                                     */
/***********************************************************************/

/* $Id: parser.mly,v 1.3 2011-08-08 19:31:17 weis Exp $ */

%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL

%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

%start main             /* the entry point */
%type <int> main

%%

main:
    expr EOL                { $1 }
;
expr:
  | INT                     { $1 }
  | LPAREN expr RPAREN      { $2 }
  | expr PLUS expr          { $1 + $3 }
  | expr MINUS expr         { $1 - $3 }
  | expr TIMES expr         { $1 * $3 }
  | expr DIV expr           { $1 / $3 }
  | MINUS expr %prec UMINUS { - $2 }
;
