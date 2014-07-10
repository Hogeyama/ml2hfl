open Ast;;
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
(* $Id: ast_print.ml,v 1.2 2011-08-08 19:31:17 weis Exp $ *)
let rec print_operator = (function s -> print_quoted_string s);;

let rec print_label = (function s -> print_quoted_string s);;

let rec print_list1 pr_a =
  (function (a, l_a) ->
    printf "@[<1>("; pr_a a; printf ",@ "; print_list (pr_a) l_a;
    printf ")@]");;

let rec print_expression = function
  | E_colon -> printf "E_colon" | E_dollar -> printf "E_dollar"
  | E_number s ->
     printf "@[<1>(%s@ " "E_number"; print_quoted_string s; printf ")@]"
  | E_string s ->
     printf "@[<1>(%s@ " "E_string"; print_quoted_string s; printf ")@]"
  | E_matrix l_l_e ->
     printf "@[<1>(%s@ " "E_matrix";
     print_list (print_list (print_expression)) l_l_e; printf ")@]"
  | E_variable s ->
     printf "@[<1>(%s@ " "E_variable"; print_quoted_string s; printf ")@]"
  | E_parameter s ->
     printf "@[<1>(%s@ " "E_parameter"; print_quoted_string s; printf ")@]"
  | E_if (e, e0, e1) ->
     printf "@[<1>(%s@ " "E_if"; printf "@[<1>("; print_expression e;
     printf ",@ "; print_expression e0; printf ",@ "; print_expression e1;
     printf ")@]"; printf ")@]"
  | E_binary (o, e, e0) ->
     printf "@[<1>(%s@ " "E_binary"; printf "@[<1>("; print_operator o;
     printf ",@ "; print_expression e; printf ",@ "; print_expression e0;
     printf ")@]"; printf ")@]"
  | E_unary (o, e) ->
     printf "@[<1>(%s@ " "E_unary"; printf "@[<1>("; print_operator o;
     printf ",@ "; print_expression e; printf ")@]"; printf ")@]"
  | E_apply (e, l_e) ->
     printf "@[<1>(%s@ " "E_apply"; printf "@[<1>("; print_expression e;
     printf ",@ "; print_list (print_expression) l_e; printf ")@]";
     printf ")@]"
  | E_record_access (e, l) ->
     printf "@[<1>(%s@ " "E_record_access"; printf "@[<1>(";
     print_expression e; printf ",@ "; print_label l; printf ")@]";
     printf ")@]"
  | E_switch s ->
     printf "@[<1>(%s@ " "E_switch"; print_switch_desc s; printf ")@]"
  | E_bracket e ->
     printf "@[<1>(%s@ " "E_bracket"; print_expression e; printf ")@]"
  | E_paren e ->
     printf "@[<1>(%s@ " "E_paren"; print_expression e; printf ")@]"

and print_switch_desc = function
  {switch_subject = e; switch_cases = l_t_l_p_e; switch_default = e1; } ->
    printf "@[<1>{"; printf "@[<1>switch_subject =@ "; print_expression e;
    printf ";@]@ "; printf "@[<1>switch_cases =@ ";
    print_list
     ((function (l_p, e0) ->
        printf "@[<1>("; print_list (print_pattern) l_p; printf ",@ ";
        print_expression e0; printf ")@]"))
     l_t_l_p_e;
    printf ";@]@ "; printf "@[<1>switch_default =@ "; print_expression e1;
    printf ";@]@ "; printf "@,}@]"

and print_pattern = (function e -> print_expression e);;

(* End of Camlpp generated code *)
