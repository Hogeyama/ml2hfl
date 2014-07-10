(***********************************************************************)
(*                                                                     *)
(*                        SciCos compiler                              *)
(*                                                                     *)
(*            Pierre Weis                                              *)
(*                                                                     *)
(*                               INRIA Rocquencourt                    *)
(*                                                                     *)
(*  Copyright 2009 INRIA                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: splice_ast_pprint.ml,v 1.2 2011-08-11 15:52:58 weis Exp $ *)

open Format;;
open Splice_ast;;

let rec pprint ppf = function
  | S_colon -> fprintf ppf "%s" ":"
  | S_dollar -> fprintf ppf "%s" "$"
  | S_number n -> fprintf ppf "%s" n
  | S_string s -> fprintf ppf "%S" s
  | S_matrix exprs -> fprintf ppf "%a" print_matrix_elems exprs
  | S_variable v -> fprintf ppf "%s" v
  | S_parameter p -> fprintf ppf "u%d" p
  | S_if (cond, e_then, e_else) ->
    fprintf ppf "%s(%a, %a, %a)"
    (Transl.string_of_ternop Deep_ast.Tdotif)
    pprint cond pprint e_then pprint e_else
  | S_switch {
      sswitch_subject = s;
      sswitch_cases = cls;
      sswitch_default = d;
    } ->
    let pprint_clauses ppf cls =
      let pprint_clause ppf (pats, e) =
        match pats with
        | [] -> assert false
        | [ pat ] ->
          fprintf ppf "%a, %a, "
            pprint pat
            pprint e
        | pat :: pats ->
          fprintf ppf "list (%a%a), %a"
            pprint pat
            (fun ppf ->
             List.iter
               (fun pat -> fprintf ppf ", %a" pprint pat))
            pats
            pprint e in
      List.iter (pprint_clause ppf) cls in

    let pprint_default ppf d =
      fprintf ppf "%a" pprint d in

    fprintf ppf "%s(%a, %a%a)"
    (Transl.string_of_sci_op Deep_ast.Dotswitch)
    pprint s pprint_clauses cls pprint_default d
  | S_apply (e_f, e_args) ->
    fprintf ppf "%a(%a)"
    pprint e_f print_args e_args
  | S_unary (op, x) ->
    let op = Transl.string_of_unop op in
    fprintf ppf "%s(%a)" op pprint x
  | S_binary (op, x, y) ->
    if Transl.is_prefix_binop op then
      fprintf ppf "%s(%a, %a)"
        (Transl.string_of_binop op)
        pprint x
        pprint y else
    if op = Deep_ast.Bhconc || op = Deep_ast.Bvconc then
      fprintf ppf "%a%s %a"
        pprint x
        (Transl.string_of_binop op)
        pprint y else
    fprintf ppf "%a%s%a"
      pprint x
      (Transl.string_of_binop op)
      pprint y
  | S_ternary (op, x, y, z) ->
    fprintf ppf "%s(%a, %a, %a)"
      (Transl.string_of_ternop op)
      pprint x
      pprint y
      pprint z
  | S_quaternary (op, x, y, z, t) ->
    fprintf ppf "%s(%a, %a, %a, %a)"
      (Transl.string_of_quaternop op)
      pprint x
      pprint y
      pprint z
      pprint t
  | S_record_access (e1, label) ->
    fprintf ppf "%a.%s"
      pprint e1
      label
  | S_paren e ->
    fprintf ppf "(%a)" pprint e
  | S_bracket e ->
    fprintf ppf "[%a]" pprint e
  | S_splice e ->
    fprintf ppf "splice (%a)"
      pprint e

and print_args ppf = function
  | [] -> ()
  | arg :: args ->
    pprint ppf arg;
    List.iter
      (function arg -> fprintf ppf ",%a" pprint arg)
      args

and print_matrix_elems ppf = function
  | [] -> ()
  | line :: lines ->
    print_args ppf line;
    List.iter
      (function line -> fprintf ppf ";%a" print_args line)
      lines
;;

let pstring_of_spliced_expression e =
  pprint Format.str_formatter e;
  Format.flush_str_formatter ();
;;
