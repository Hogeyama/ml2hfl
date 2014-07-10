open Splice_ast;;
open Ast;;
open Ast_print;;
open Deep_ast;;
open Deep_ast_print;;
(* Camlpp generated file *)

open Lib_pp;;
open Format;;

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
(* $Id: splice_ast_print.ml,v 1.2 2011-08-08 19:31:17 weis Exp $ *)
open Ast;;

(* For list1 *)
open Deep_ast;;

(* For unop and binop and label *)
let rec print_expression = function
  | S_colon -> printf "S_colon" | S_dollar -> printf "S_dollar"
  | S_number s ->
     printf "@[<1>(%s@ " "S_number"; print_quoted_string s; printf ")@]"
  | S_string s ->
     printf "@[<1>(%s@ " "S_string"; print_quoted_string s; printf ")@]"
  | S_matrix l_l_e ->
     printf "@[<1>(%s@ " "S_matrix";
     print_list (print_list (print_expression)) l_l_e; printf ")@]"
  | S_variable s ->
     printf "@[<1>(%s@ " "S_variable"; print_quoted_string s; printf ")@]"
  | S_parameter i ->
     printf "@[<1>(%s@ " "S_parameter"; print_quoted_int i; printf ")@]"
  | S_if (e, e0, e1) ->
     printf "@[<1>(%s@ " "S_if"; printf "@[<1>("; print_expression e;
     printf ",@ "; print_expression e0; printf ",@ "; print_expression e1;
     printf ")@]"; printf ")@]"
  | S_switch s ->
     printf "@[<1>(%s@ " "S_switch"; print_switch_desc s; printf ")@]"
  | S_unary (u, e) ->
     printf "@[<1>(%s@ " "S_unary"; printf "@[<1>("; print_unop u;
     printf ",@ "; print_expression e; printf ")@]"; printf ")@]"
  | S_binary (b, e, e0) ->
     printf "@[<1>(%s@ " "S_binary"; printf "@[<1>("; print_binop b;
     printf ",@ "; print_expression e; printf ",@ "; print_expression e0;
     printf ")@]"; printf ")@]"
  | S_ternary (t, e, e0, e1) ->
     printf "@[<1>(%s@ " "S_ternary"; printf "@[<1>("; print_ternop t;
     printf ",@ "; print_expression e; printf ",@ "; print_expression e0;
     printf ",@ "; print_expression e1; printf ")@]"; printf ")@]"
  | S_quaternary (q, e, e0, e1, e2) ->
     printf "@[<1>(%s@ " "S_quaternary"; printf "@[<1>("; print_quaternop q;
     printf ",@ "; print_expression e; printf ",@ "; print_expression e0;
     printf ",@ "; print_expression e1; printf ",@ "; print_expression e2;
     printf ")@]"; printf ")@]"
  | S_apply (e, l_e) ->
     printf "@[<1>(%s@ " "S_apply"; printf "@[<1>("; print_expression e;
     printf ",@ "; print_list (print_expression) l_e; printf ")@]";
     printf ")@]"
  | S_record_access (e, l) ->
     printf "@[<1>(%s@ " "S_record_access"; printf "@[<1>(";
     print_expression e; printf ",@ "; print_label l; printf ")@]";
     printf ")@]"
  | S_paren e ->
     printf "@[<1>(%s@ " "S_paren"; print_expression e; printf ")@]"
  | S_bracket e ->
     printf "@[<1>(%s@ " "S_bracket"; print_expression e; printf ")@]"
  | S_splice e ->
     printf "@[<1>(%s@ " "S_splice"; print_expression e; printf ")@]"

and print_switch_desc = function
  {sswitch_subject = e; sswitch_cases = c; sswitch_default = e0; } ->
    printf "@[<1>{"; printf "@[<1>sswitch_subject =@ "; print_expression e;
    printf ";@]@ "; printf "@[<1>sswitch_cases =@ "; print_clauses c;
    printf ";@]@ "; printf "@[<1>sswitch_default =@ "; print_expression e0;
    printf ";@]@ "; printf "@,}@]"

and print_clauses = (function l_c -> print_list (print_clause) l_c)

and print_clause =
  (function (l_p, e) ->
    printf "@[<1>("; print_list (print_pattern) l_p; printf ",@ ";
    print_expression e; printf ")@]")

and print_pattern = (function e -> print_expression e);;

(* End of Camlpp generated code *)
