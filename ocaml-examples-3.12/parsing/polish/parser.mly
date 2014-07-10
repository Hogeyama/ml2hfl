%{
(***********************************************************************)
(*                                                                     *)
(*                        Caml examples                                *)
(*                                                                     *)
(*            Pierre Weis                                              *)
(*                                                                     *)
(*                        INRIA Rocquencourt                           *)
(*                                                                     *)
(*  Copyright (c) 1994-2011, INRIA                                     *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  Distributed under the BSD license.                                 *)
(*                                                                     *)
(***********************************************************************)

(* $Id: parser.mly,v 1.5 2011-08-08 19:11:31 weis Exp $ *)

open Parsetree;;

let mk_infix_application e1 op e2 =
  E_apply (E_variable op, [e1; e2])
;;

let mk_prefix_application op e1 =
  E_apply (E_variable op, [e1])
;;

%}

%token EOF

%token <string> IDENT /* Lower case ident (e.g. x, cos, _1, or _xY) */

%token <string> DOT_OP

/* Basic constants */
%token <string> FLOAT

/* Arithmetic operators */
%token <string> DOT_QUOTE_OP
%token <string> DOT_HAT_OP
%token <string> QUOTE_OP
%token <string> HAT_OP
%token <string> STAR_STAR_OP

%token <string> TILDA_OP

%token <string> DOT_STAR_OP
%token <string> DOT_SLASH_OP
%token <string> DOT_BACKSLASH_OP
%token <string> STAR_OP
%token <string> SLASH_OP
%token <string> BACKSLASH_OP
%token <string> PERCENT_OP

%token <string> PLUS_OP
%token <string> DASH_OP

%token <string> COLON_OP

%token <string> LT_OP
%token <string> GT_OP
%token <string> EQ_OP
%token <string> TILDA_EQ_OP

%token <string> BAR_OP
%token <string> AMPER_OP

%token <string> BAR_BAR_OP
%token <string> AMPER_AMPER_OP

/* Nested symbols */
%token LPAREN
%token RPAREN
%token COMMA

/* Keywords */
%token ELSE
%token FI
%token IF
%token THEN

/* Precedences and associativities. */

/* Binary operators. */

%nonassoc ELSE                     /* (if ... then ... else ...) */

%right    BAR_BAR_OP               /* expr (e || e || e) */
%right    AMPER_AMPER_OP           /* expr (e && e && e) */

%right    BAR_OP                   /* expr (e | e | e) */
%right    AMPER_OP                 /* expr (e & e & e) */

%left     LT_OP GT_OP EQ_OP TILDA_EQ_OP

%left     COLON_OP /* ???? */

%left     PLUS_OP DASH_OP
       /* expr (e OP e OP OP e) with OP starting with '+', or '-' */

%left     DOT_STAR_OP DOT_SLASH_OP DOT_BACKSLASH_OP STAR_OP SLASH_OP BACKSLASH_OP PERCENT_OP
       /* expr (e OP e OP e) with OP starting with '*', or '/' */

%nonassoc TILDA_OP prec_unary_minus
          /* expr (~| e) */
          /* unary DASH_OP e.g. DASH_OP is '-' */

%right    DOT_HAT_OP HAT_OP STAR_STAR_OP prec_star_star

%nonassoc DOT_QUOTE_OP QUOTE_OP

/* Predefined precedences to resolve conflicts. */

%nonassoc below_RPAREN
%nonassoc RPAREN
%nonassoc LPAREN

%start file
%type <Parsetree.expression> file
%%

file:
  | expr EOF { $1 }
;

expr:
  | FLOAT
    { E_number $1 }
  | IDENT
    { E_variable $1 }
  | expr LPAREN expr_comma_list RPAREN
    { E_apply ($1, $3) }
  | expr LPAREN RPAREN
    { E_apply ($1, []) }
  | IF expr THEN expr ELSE expr FI
    { E_if ($2, $4, $6) }

  /* Binary operators */

  | expr BAR_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr AMPER_OP expr
    { mk_infix_application $1 $2 $3 }

    | expr EQ_OP expr
      { mk_infix_application $1 $2 $3 }
    | expr LT_OP expr
      { mk_infix_application $1 $2 $3 }
    | expr GT_OP expr
      { mk_infix_application $1 $2 $3 }
    | expr TILDA_EQ_OP expr
      { mk_infix_application $1 $2 $3 }

  | expr PLUS_OP expr
    { mk_infix_application $1 $2 $3 }
    | expr DASH_OP expr
      { mk_infix_application $1 $2 $3 }
  | expr STAR_OP expr
    { mk_infix_application $1 $2 $3 }
    | expr SLASH_OP expr
      { mk_infix_application $1 $2 $3 }
    | expr PERCENT_OP expr
      { mk_infix_application $1 $2 $3 }
  | expr DOT_HAT_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr HAT_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr STAR_STAR_OP expr
    { mk_infix_application $1 $2 $3 }

  /* Unary operators. */
  | DASH_OP expr %prec prec_unary_minus
    { mk_prefix_application $1 $2 }
  | PLUS_OP expr %prec prec_unary_minus
    { mk_prefix_application $1 $2 }
  | TILDA_OP expr %prec prec_unary_minus
    { mk_prefix_application $1 $2 }

  | DOT_QUOTE_OP expr %prec prec_star_star
    { mk_prefix_application $1 $2 }
  | QUOTE_OP expr %prec prec_star_star
    { mk_prefix_application $1 $2 }

  | LPAREN expr RPAREN
    { $2 }
;

expr_comma_list:
  | expr { [ $1 ] }
  | expr COMMA expr_comma_list { $1 :: $3 }
;
