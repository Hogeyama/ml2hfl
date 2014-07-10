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

(* $Id: parser.mly,v 1.1 2011-08-08 19:09:58 weis Exp $ *)

open Ast;;

let mk_infix_application e1 op e2 = E_binary (op, e1, e2);;

let mk_unary_application op e1 = E_unary (op, e1);;

let cons1 x (y, l) = x, y :: l;;

let mk_switch s cases =
  let default, cases =
    match List.rev cases with
    | ([], default) :: cases -> default, List.rev cases
    | _ -> assert false in
  E_switch {
    switch_subject = s;
    switch_cases = cases;
    switch_default = default;
  }
;;
%}

%token EOF

%token <string> IDENT /* Lower case ident (e.g. x, cos, _1, or _xY) */
%token <string> DOT_IDENT /* DOT lower case ident (e.g. .x, .cos or ._xY) */
%token <string> PARAM /* Bloc parameter (e.g. u, u1, or u20) */

%token <string> DOT_OP

/* Basic constants */
%token <string> FLOAT
%token <string> STRING

/* Arithmetic operators */
%token <string> NOT_OP
%token <string> HAT_OP

%token <string> DOT_QUOTE_OP
%token <string> DOT_HAT_OP
%token <string> DOT_STAR_OP
%token <string> DOT_SLASH_OP
%token <string> DOT_BACKSLASH_OP

%token <string> STAR_OP
%token <string> SLASH_OP
%token <string> BACKSLASH_OP
%token <string> PERCENT_OP

%token <string> PLUS_OP
%token <string> DASH_OP

%token <string> COMPARE_OP

%token <string> BAR_BAR_OP
%token <string> AMPER_AMPER_OP

%token QUOTE
%token COLON
%token COLON_EQUAL
%token COMMA
%token DOLLAR
%token DOT
%token SEMI

/* Nested symbols */
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE

/* Keywords */

%token DOT_IF
%token DOT_THEN
%token DOT_ELSE
%token DOT_END

%token DOT_SWITCH
%token DOT_OTHERWISE
%token DOT_CASE

%token IF
%token THEN
%token ELSE
%token END

/* Precedences and associativities. */

/* Binary operators. */

%nonassoc ELSE DOT_ELSE   /* (.if ... .then ... .else ... .end) */
%nonassoc DOT_SWITCH      /* (.switch ... .case ... .case ... .end) */

%right    BAR_BAR_OP      /* expr (e || e || e) */
%right    AMPER_AMPER_OP  /* expr (e && e && e) */

%right    BAR_OP          /* expr (e | e | e) */
%right    AMPER_OP        /* expr (e & e & e) */

%right    COLON_EQUAL     /* expr (e := e := e) */

%left     COMPARE_OP      /* expr (e == e < e) */

%left     SEMI
%left     COMMA

%right    COLON

%left     PLUS_OP DASH_OP
          /* expr (e OP e OP OP e) with OP starting with '+', or '-' */

%left     STAR_OP SLASH_OP BACKSLASH_OP PERCENT_OP
          /* expr (e OP e OP e) with OP starting with '*', or '/' */

%nonassoc prec_unary_minus
          /* unary DASH_OP e.g. DASH_OP is '-' */

%left     DOT_STAR_OP DOT_SLASH_OP DOT_BACKSLASH_OP

%right    DOT_HAT_OP HAT_OP

%right    NOT_OP /* expr (! e) */

%left     DOT_OP DOT_IDENT
%nonassoc QUOTE DOT_QUOTE_OP

/* Predefined precedences to resolve conflicts. */

%nonassoc below_RPAREN
%nonassoc RPAREN
%nonassoc LPAREN
%nonassoc RBRACKET
%nonassoc LBRACKET
%nonassoc RBRACE
%nonassoc LBRACE

%start file
%type <Ast.expression> file
%%

file:
  | expr EOF { $1 }
;

expr:
  | FLOAT
    { E_number $1 }
  | STRING
    { E_string $1 }
  | IDENT
    { E_variable $1 }
  | DOLLAR
    { E_dollar }
  | COLON
    { E_colon }
  | PARAM
    { E_parameter $1 }
  | expr LBRACKET expr_comma_list RBRACKET
    { E_apply ($1, $3) }
  | expr LPAREN expr_comma_list RPAREN
    { E_apply ($1, $3) }
  | DOT_IF expr DOT_THEN expr DOT_ELSE expr DOT_END
    { E_if ($2, $4, $6) }

  | DOT_SWITCH expr switch_cases
    { mk_switch $2 $3 }

  /* Binary operators */

  | expr BAR_BAR_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr AMPER_AMPER_OP expr
    { mk_infix_application $1 $2 $3 }

    | expr COLON_EQUAL expr
      { mk_infix_application $1 ":=" $3 }

    | expr COMPARE_OP expr
      { mk_infix_application $1 $2 $3 }

  | expr PLUS_OP expr
    { mk_infix_application $1 $2 $3 }
    | expr DASH_OP expr
      { mk_infix_application $1 $2 $3 }
  | expr STAR_OP expr
    { mk_infix_application $1 $2 $3 }
    | expr SLASH_OP expr
      { mk_infix_application $1 $2 $3 }
    | expr BACKSLASH_OP expr
      { mk_infix_application $1 $2 $3 }
    | expr PERCENT_OP expr
      { mk_infix_application $1 $2 $3 }
  | expr HAT_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr DOT_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr DOT_IDENT
    { E_record_access ($1, $2) }
  | expr DOT_SLASH_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr DOT_STAR_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr DOT_BACKSLASH_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr DOT_HAT_OP expr
    { mk_infix_application $1 $2 $3 }

  /* Unary operators. */
  | DASH_OP expr %prec prec_unary_minus
    { mk_unary_application $1 $2 }
  | PLUS_OP expr %prec prec_unary_minus
    { mk_unary_application $1 $2 }
  | NOT_OP expr %prec prec_unary_minus
    { mk_unary_application $1 $2 }

  | expr DOT_QUOTE_OP
    { mk_unary_application $2 $1 }
  | expr QUOTE
    { mk_unary_application "\'" $1 }

  | expr COLON expr
    { mk_infix_application $1 ":" $3 }

  | LPAREN expr RPAREN
    { E_paren $2 }

  | LBRACKET concat RBRACKET
    { E_bracket (E_matrix $2) }
;

concat:
  | { [] }
  | expr_comma_list1
    { [ $1 ] }
  | expr_comma_list1 SEMI concat
    { $1 :: $3 }
;

expr_comma_list:
  | { [ ] }
  | expr_comma_list1 { $1 }
;

expr_comma_list1:
  | expr { [ $1 ] }
  | expr COMMA expr_comma_list1 { $1 :: $3 }
;

switch_cases:
  | DOT_OTHERWISE expr DOT_END
    { [ [], $2 ] }
  | switch_case switch_cases
    { $1 :: $2 }
;

switch_pattern:
  | expr
    { [ $1 ] }
  | LBRACE expr_comma_list1 RBRACE
    { $2 }
;

switch_case:
 | DOT_CASE switch_pattern COMMA expr
   { $2, $4 }
;
