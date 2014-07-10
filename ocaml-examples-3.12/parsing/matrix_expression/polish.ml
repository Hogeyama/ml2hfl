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

(* $Id: polish.ml,v 1.2 2011-08-11 15:50:40 weis Exp $ *)

open Splice_ast;;

let rec polish_exp
    ((accu_args : string list),
     (accu_ops : string list) as accu) = function
  | S_colon -> assert false
  | S_dollar -> assert false
  | S_number n -> n :: accu_args, accu_ops
  | S_string _ -> assert false
  | S_matrix lines ->
     ( match lines with
       | [] -> accu_args, accu_ops
       | line1 :: lines2 ->
         let hconc accu elt =
           let accu_args, accu_ops = polish_exp accu elt in
           accu_args, Transl.string_of_binop Deep_ast.Bhconc :: accu_ops in
         let code_line accu line =
           ( match line with
             | [] -> accu_args, accu_ops
             | e1 :: line2 ->
               let accu_args, accu_ops = polish_exp accu e1 in
               let accu_args, accu_ops =
                 List.fold_left hconc (accu_args, accu_ops) line2 in
               accu_args, Transl.string_of_binop Deep_ast.Bvconc :: accu_ops
           ) in
         let code_line2 accu line =
           ( match line with
             | [] -> accu_args, accu_ops
             | e1 :: line2 ->
               let accu_args, accu_ops = polish_exp accu e1 in
               List.fold_left hconc (accu_args, accu_ops) line2
           ) in
         let accu_args, accu_ops = code_line2 accu line1 in
         List.fold_left code_line (accu_args, accu_ops) lines2
     )

  | S_variable v -> v :: accu_args, accu_ops
  | S_parameter v ->  accu_args, (string_of_int v) :: accu_ops
  | S_if (cond, e_then, e_else) ->
    let accu_args, accu_ops =
      polish_exp (polish_exp (polish_exp accu cond) e_then) e_else in
    accu_args, Transl.string_of_ternop Deep_ast.Tdotif :: accu_ops
  | S_switch  {
      sswitch_subject = e;
      sswitch_cases = cls;
      sswitch_default = d;
    } ->

    let rec multi_push accu_ops = function
    | 0 -> accu_ops
    | n -> multi_push ("push" :: accu_ops) (n - 1) in

    let ncls = List.length cls in

    let accu_args, accu_ops = polish_exp accu e in
    let accu = accu_args, multi_push accu_ops (ncls - 1) in

    let accu = polish_exp accu d in

    List.fold_left
      (fun accu (pats, eci) ->
         match pats with
         | [ pat ] ->
           let accu_args, accu_ops =
             polish_exp (polish_exp accu pat) eci in
           accu_args,
           Transl.string_of_quaternop Deep_ast.Qdotswitchcase :: accu_ops
         | _ -> assert false)
      accu
      (List.rev cls)

  | S_apply (e_f, e_args) ->
    polish_exp (polish_exp_args accu e_args) e_f
  | S_unary (op, x) ->
    let accu_args, accu_ops = polish_exp accu x in
    accu_args, Transl.string_of_unop op :: accu_ops
  | S_binary (op, x, y) ->
    let accu_args, accu_ops =
      polish_exp (polish_exp accu x) y in
    accu_args, Transl.string_of_binop op :: accu_ops
  | S_ternary (op, x, y, z) ->
    let accu_args, accu_ops =
      polish_exp (polish_exp (polish_exp accu x) y) z in
    accu_args, Transl.string_of_ternop op :: accu_ops
  | S_quaternary (op, x, y, z, t) ->
    let accu_args, accu_ops =
      polish_exp (polish_exp (polish_exp (polish_exp accu x) y) z) t in
    accu_args, Transl.string_of_quaternop op :: accu_ops
  | S_record_access (e1, label) ->
    let accu_args, accu_ops = polish_exp accu e1 in
    let accu_args, accu_ops = "tmp" :: accu_args, "bind" :: accu_ops in
    Printf.sprintf "tmp.%s" label :: accu_args,
    "eval" :: accu_ops
  | S_bracket e ->
    polish_exp accu e
  | S_splice e ->
    let e = Splicing.fully_un_splice e in
    let arg = Splice_ast_pprint.pstring_of_spliced_expression e in
    arg :: accu_args, "eval" :: accu_ops
  | S_paren e ->
    polish_exp accu e

and polish_exp_args accu = function
  | [] -> accu
  | arg :: args ->
    polish_exp_args (polish_exp accu arg) args
;;

let polish e =
  let accu_args, accu_ops = polish_exp ([], []) e in
  List.rev accu_args, List.rev accu_ops
;;
