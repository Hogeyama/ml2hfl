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

(* $Id: scoping.ml,v 1.4 2011-08-08 18:21:34 weis Exp $ *)

open Parsetree;;

let scope_binary_operator op =
  match op with
  | "+" | "*" | "-" | "**" | "^"
  | "==" | "~=" | "<>"
  | "<" | ">" | ">=" | "<="
  | "||" | "&&" | "!=" -> ()
  | s -> failwith (Printf.sprintf "unknown operator %S" s)
;;

let scope_unary_operator op =
  match op with
  | "+" | "-"
  | "!" -> ()
  | s -> failwith (Printf.sprintf "unknown operator %S" s)
;;

let scope_intrinsic_function_name name =
  name,
  match name with
  | "abs" | "acos" | "asin"
  | "atan" | "atan2"
  | "ceil" | "cos" | "cosh"
  | "exp"
  | "fabs" | "floor" -> 1
  | "hypot" -> 2
  | "ln" | "log" | "log10" -> 1
  | "pow" | "power"
  | "rem" -> 2
  | "sgn" | "sin" | "sinh"| "sqrt"
  | "tan" | "tanh"
  | "u" -> 1
  | s -> raise Not_found
;;

let rec scope_function = function
  | E_paren f -> scope_function f
  | E_variable v -> scope_intrinsic_function_name v
  | E_number s ->
    failwith (Printf.sprintf "%s is not a function" s)
  | E_if (_, _, _)
  | E_apply (_, _)
  | E_binary (_, _, _)
  | E_record_access (_, _)
  | E_apply_unknown (_, _)
  | E_prefix (_, _) ->
    failwith (Printf.sprintf "illegal function")
;;

let scope_variable s =
  let length_s = String.length s in
  assert (length_s > 0);
  let c = s.[0] in
  match c with
  | 'u' ->
    let lim = length_s in
    let rec check_rest i =
      if i >= lim then s else
      match s.[i] with
      | '0'..'9' -> check_rest (i + 1)
      | _ -> failwith (Printf.sprintf "illegal variable %S" s) in
    check_rest 1
  | _ -> s
;;

let rec scope exp =
  match exp with
  | E_number n -> exp
  | E_variable v -> E_variable (scope_variable v)
  | E_if (cond, e_then, e_else) ->
    E_if (scope cond, scope e_then, scope e_else)
  | E_apply (e_f, e_args) ->
    begin try
      let f_name, f_arity = scope_function e_f in
      let e_arity = List.length e_args in
      if f_arity <> e_arity then
        failwith (
          Printf.sprintf "function %s has arity %d, but is used with arity %d"
            f_name f_arity e_arity) else
      E_apply (E_variable f_name, scope_args e_args)
     with
     | Not_found ->
       E_apply_unknown (scope e_f, scope_args e_args)
    end
  | E_prefix (op, e1) ->
    scope_unary_operator op;
    E_prefix (op, scope e1)
  | E_binary (op, e1, e2) ->
    scope_binary_operator op;
    E_binary (op, scope e1, scope e2)
  | E_record_access (e1, label) ->
    E_record_access (scope e1, label)
  | E_apply_unknown (e1, e_args) ->
    E_apply_unknown (scope e1, scope_args e_args)
  | E_paren e -> scope e

and scope_args = function
  | [] -> []
  | arg :: args ->
    scope arg ::
    List.map scope args
;;
