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

(* $Id: transl.ml,v 1.5 2011-08-08 18:21:34 weis Exp $ *)

open Parsetree;;

let ( + ) e1 e2 = E_binary ("+", e1, e2)
and ( - ) e1 e2 = E_binary ("-", e1, e2)
and ( / ) e1 e2 = E_binary ("/", e1, e2)
and ( ^ ) e1 e2 = E_binary ("^", e1, e2)
and ( * ) e1 e2 = E_binary ("*", e1, e2)
;;

let make_paren exp =
  match exp with
  | E_number s ->
    if String.length s = 0 then assert false else
    if s.[0] = '-' then E_paren exp else exp
  | E_variable _ -> exp
  | E_if (_, _, _)
  | E_prefix (_, _)
  | E_binary (_, _, _)
  | E_apply (_, _)
  | E_apply_unknown (_, _) -> E_paren exp
  | E_record_access (_, _) -> exp
  | E_paren _ -> exp
;;

let sqrt e =
  let half = E_number "0.5" in
  make_paren e ^ half
;;

let square e = make_paren e * make_paren e;;

let int_part e = E_prefix ("int", e)

let transl_variable = function
 | "u" -> E_variable "u1"
 | "fabs" -> E_variable "abs"
 | "ln" -> E_variable "log"
 | "sgn" -> E_variable "sign"
 | op -> E_variable op
;;

let transl_prefix_variable = function
 | "!" -> "~"
 | op -> op

let transl_infix_variable = function
 | "&&" -> "&"
 | "||" -> "|"
 | "!=" -> "~="
 | op -> op
;;

let rec transl exp =
  match exp with
  | E_number _ -> exp
  | E_variable v -> transl_variable v
  | E_if (cond, e_then, e_else) ->
    E_if (transl cond, transl e_then, transl e_else)
  | E_prefix (op, x) ->
    let x = transl x in
    E_prefix (transl_prefix_variable op, x)
  | E_binary (op, x, y) ->
    let x = transl x and y = transl y
    and op = transl_infix_variable op in
    E_binary (op, x, y)
  | E_apply (E_variable "u", [ arg; ]) ->
    begin
      try
        match arg with
        | E_number i ->
            let i = int_of_string i in
            E_variable (Printf.sprintf "u%d" i)
        | arg -> failwith "not a constant" with
      | Failure _ ->
          failwith (Printf.sprintf
            "invalid u variable specification: \
             in u (exp), exp is not a positive integer")
    end
  | E_apply (E_variable op, [ x; ]) ->
    let x = transl x in
    begin match op with
    | "sqrt" -> sqrt x
    | op -> E_apply (transl_variable op, [ x; ])
    end
  | E_apply (E_variable op, [ x; y; ]) ->
    let x = transl x and y = transl y in
    begin match op with
    | "hypot" -> sqrt (square x + square y)
    | "pow" | "power" -> make_paren x ^ make_paren y
    | "rem" -> x - int_part (make_paren x / make_paren y) * make_paren y
    | op -> E_apply (transl_variable op, [ x; y; ])
    end
  | E_apply (e_f, e_args) ->
    E_apply (transl e_f, List.map transl e_args)
  | E_record_access (e1, label) ->
    E_record_access (transl e1, label)
  | E_apply_unknown (e1, e_args) ->
    E_apply_unknown (transl e1, List.map transl e_args)
  | E_paren exp -> E_paren (transl exp)
;;
