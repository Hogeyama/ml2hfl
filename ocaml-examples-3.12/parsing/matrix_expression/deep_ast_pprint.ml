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

(* $Id: deep_ast_pprint.ml,v 1.1 2011-08-08 19:09:58 weis Exp $ *)

open Deep_ast;;
open Format;;

let rec print_unop ppf op =
  fprintf ppf "%s" (Transl.string_of_unop op)
;;

let rec print_binop ppf op =
  fprintf ppf "%s" (Transl.string_of_binop op)
;;

let rec print_ternop ppf op =
  fprintf ppf "%s" (Transl.string_of_ternop op)
;;

let rec print_quaternop ppf op =
  fprintf ppf "%s" (Transl.string_of_quaternop op)
;;

let rec pprint ppf = function
  | D_dollar -> fprintf ppf "%s" "$"
  | D_colon -> fprintf ppf "%s" ":"
  | D_number n -> fprintf ppf "%s" n
  | D_string s -> fprintf ppf "%S" s
  | D_matrix exprs -> fprintf ppf "%a" print_matrix_elems exprs
  | D_variable v -> fprintf ppf "%s" v
  | D_parameter i -> fprintf ppf "u%d" i
  | D_accu exp -> fprintf ppf "(accu %a)" pprint exp
  | D_if (cond, e_then, e_else) ->
    fprintf ppf ".if %a .then %a .else %a .fi"
      pprint cond pprint e_then pprint e_else
  | D_switch {
      dswitch_subject = s;
      dswitch_cases = cls;
      dswitch_default = d;
    } ->
    let pprint_clauses ppf cls =
      let pprint_clause (pats, e) =
        match pats with
        | [] -> assert false
        | [ pat ] ->
          fprintf ppf "@[<2>.case %a,@ %a@]"
            pprint pat pprint e
        | pat :: pats ->
          fprintf ppf "@[<2>.case@ @[<2>{ %a%a },@]@ %a@]"
            pprint pat
            (fun ppf ->
              List.iter (fun pat -> fprintf ppf ",@ %a" pprint pat))
            pats
            pprint e in
      List.iter pprint_clause cls in
    let pprint_default ppf d = fprintf ppf ".otherwise %a" pprint d in

    fprintf ppf "@[<hv 2>.switch %a@ %a@ %a@ .end@]"
      pprint s pprint_clauses cls pprint_default d
  | D_apply (e_f, e_args) ->
    fprintf ppf "%a (%a)"
      pprint e_f print_args e_args
  | D_unary (op, x) ->
    fprintf ppf "%a (%a)"
      print_unop op
      pprint x
  | D_binary (op, x, y) when Transl.is_prefix_binop op ->
    fprintf ppf "%a (%a, %a)"
      print_binop op
      pprint x
      pprint y
  | D_binary (op, x, y) ->
    fprintf ppf "%a %a %a"
      pprint x
      print_binop op
      pprint y
  | D_ternary (op, x, y, z) ->
    fprintf ppf "%a (%a, %a, %a)"
      print_ternop op
      pprint x
      pprint y
      pprint z
  | D_quaternary (op, x, y, z, t) ->
    fprintf ppf "%a (%a, %a, %a, %a)"
      print_quaternop op
      pprint x
      pprint y
      pprint z
      pprint t
  | D_record_access (e1, label) ->
    fprintf ppf "%a.%s"
      pprint e1
      label
  | D_bracket e ->
    fprintf ppf "[%a]" pprint e
  | D_paren e ->
    fprintf ppf "(%a)" pprint e

and print_args ppf = function
  | [] -> ()
  | arg :: args ->
    pprint ppf arg;
    List.iter (function arg -> fprintf ppf ", %a" pprint arg) args

and print_matrix_elems ppf = function
  | [] -> ()
  | line :: lines ->
    print_args ppf line;
    List.iter (function line -> fprintf ppf "; %a" print_args line) lines
;;
