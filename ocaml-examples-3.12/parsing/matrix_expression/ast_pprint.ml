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

(* $Id: ast_pprint.ml,v 1.1 2011-08-08 19:09:58 weis Exp $ *)

open Ast;;
open Format;;

let rec pprint ppf = function
  | E_colon -> fprintf ppf "%s" ":"
  | E_dollar -> fprintf ppf "%s" "$"
  | E_number n -> fprintf ppf "%s" n
  | E_string n -> fprintf ppf "%S" n
  | E_matrix ll -> fprintf ppf "[%a]" print_matrix_elems ll
  | E_variable v -> fprintf ppf "%s" v
  | E_parameter v -> fprintf ppf "%s" v
  | E_if (cond, e_then, e_else) ->
    fprintf ppf "dot_if_then_else (%a, %a, %a)"
    pprint cond pprint e_then pprint e_else
  | E_switch {
      switch_subject = e;
      switch_cases = clauses;
      switch_default = d;
    } ->
    fprintf ppf
      "@[<hv 2>dot_switch {@ \
         @[<hv 2>\
         switch_subject =@ %a;@]@ \
         @[<hv 2>\
         switch_cases =@ %a;@]@ \
         @[<hv 2>\
         switch_default =@ %a@]@ \
      }@]"
    pprint e pprint_clauses clauses pprint d
  | E_apply (e_f, e_args) ->
    fprintf ppf "%a (%a)"
    pprint e_f print_args e_args
  | E_unary (op, x) ->
    fprintf ppf "%s (%a)"
      op
      pprint x
  | E_binary (op, x, y) ->
    fprintf ppf "%a %s %a"
      pprint x
      op
      pprint y
  | E_paren e ->
    fprintf ppf "(%a)" pprint e
  | E_bracket e ->
    fprintf ppf "[%a]" pprint e
  | E_record_access (e1, label) ->
    fprintf ppf "%a.%s"
      pprint e1
      label

and print_args ppf = function
  | [] -> ()
  | arg :: args ->
    pprint ppf arg;
    List.iter (function arg -> fprintf ppf ", %a" pprint arg) args

and print_matrix_elems ppf = function
  | [] -> ()
  | line :: lines ->
    print_args ppf line;
    List.iter (fun line -> fprintf ppf "; %a" print_args line) lines

and pprint_clauses ppf = function
  | [] -> ()
  | cl :: cls ->
    fprintf ppf "list(%a%a)"
      pprint_clause cl
      (fun ppf ->
       List.iter
         (fun cl -> fprintf ppf ", %a" pprint_clause cl))
      cls

and pprint_clause ppf = function
  | (pats, e) -> fprintf ppf "list(%a, %a)" pprint_pats pats pprint e

and pprint_pats ppf pats =
  fprintf ppf "list(%a)" print_args pats
;;
