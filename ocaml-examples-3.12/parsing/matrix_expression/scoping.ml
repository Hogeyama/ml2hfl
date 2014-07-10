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

(* $Id: scoping.ml,v 1.1 2011-08-08 19:09:58 weis Exp $ *)

open Ast;;

let scope_binary_operator op =
  match op with
  | "+" | "-" | "*" | "/"
  | "**" | "^"
  | ":=" | "==" | "<" | ">" | ">=" | "<="
  | "~=" | "<>"
  | "||" | "&&" | "!="
  | ".*"
  | "./" | ".\\" | ".^"
  | "," | ";" | "\\"
  | ":" -> ()
  | s -> failwith (Printf.sprintf "unknown binary operator %S" s)
;;

let scope_unary_operator op =
  match op with
  | "+" | "-"
  | "!" | "~" | "\'" | ".\'" -> ()
  | s -> failwith (Printf.sprintf "unknown unary operator %S" s)
;;

let scope_intrinsic_function_name name =
  name,
  match name with
  | "abs"
  | "and"
  | "acos" | "acosh"
  | "asin" | "asinh"
  | "atan"  | "atanh"
  | "ceil"
  | "cos" | "cosh"
  | "exp"  | "expm" | "eye"
  | "fabs" | "floor"
  | "int"  | "inv"
  | "ln" | "log" | "logm" | "log10"
  | "or"
  | "pow" | "power"
  | "reshape" | "round"
  | "sgn"
  | "sin" | "sinh"
  | "sqrt"
  | "svd"
  | "tan" | "tanh"
  | "u" -> Some 1
  | "atan2" | "hypot" | "rem"
  | "ones" | "zeros" -> Some 2
  | "min" | "max"
  | "size" | "sum" -> None
  | _ -> raise Not_found
;;

let scope_parameter = function
  | "u" -> "1"
  | s ->
    let l = String.length s in
    assert (l > 1);
    String.sub s 1 (l - 1)
;;

let rec scope exp =
  match exp with
  | E_colon | E_dollar -> exp
  | E_number _ | E_string _ -> exp
  | E_matrix ll -> E_matrix (List.map (List.map scope) ll)
  | E_variable _ -> exp
  | E_parameter p -> E_parameter (scope_parameter p)
  | E_if (cond, e_then, e_else) ->
    E_if (scope cond, scope e_then, scope e_else)
  | E_switch  {
      switch_subject = s;
      switch_cases = cls;
      switch_default = d;
    } ->
    E_switch  {
      switch_subject = scope s;
      switch_cases = scope_clauses cls;
      switch_default = scope d;
    }
  | E_apply (E_parameter "u", [ arg; ]) ->
    begin
      try
        match arg with
        | E_number i -> E_parameter i
        | E_string _
        | E_colon | E_dollar
        | E_paren _ | E_bracket _ | E_matrix _
        | E_record_access (_, _)
        | E_apply (_, _) | E_unary (_, _) | E_binary (_, _, _)
        | E_if (_, _, _) | E_switch _
        | E_parameter _ | E_variable _ ->
          failwith "not a constant"
      with
      | Failure _ ->
          failwith (Printf.sprintf
            "invalid u variable specification: \
             in u (exp), exp is not a positive integer")
    end
  | E_apply (e_f, [ E_colon; ]) ->
    let e_f = scope e_f in
    E_unary (":", e_f)
  | E_apply (e_f, e_args) ->
    let e_args = scope_args e_args in
    let e_arity = List.length e_args in
    begin
      try
        let f_name, f_arity = scope_function e_f in
        match f_arity with
        | None -> scope_application f_name e_args
        | Some f_arity ->
          if f_arity <> e_arity then
            failwith (
              Printf.sprintf
                "function %s has arity %d, but is used with arity %d"
                f_name f_arity e_arity)
          else scope_application f_name e_args
      with
      | Not_found ->
        E_apply (scope e_f, e_args)
    end
  | E_unary (op, e1) ->
    scope_unary_operator op;
    E_unary (op, scope e1)
  | E_binary (op, e1, e2) ->
    scope_binary_operator op;
    E_binary (op, scope e1, scope e2)
  | E_record_access (e1, label) ->
    E_record_access (scope e1, label)
  | E_paren e -> E_paren (scope e)
  | E_bracket e -> E_bracket (scope e)

and scope_application f_name e_args =
  match e_args with
  | [ e_arg; ] -> E_unary (f_name, e_arg)
  | [ e_arg1; e_arg2; ] -> E_binary (f_name, e_arg1, e_arg2)
  | e_args ->
    E_apply (E_variable f_name, scope_args e_args)

and scope_args args = List.map scope args

and scope_clauses cls =
  let scope_clause (pats, e) = scope_args pats, scope e in
  List.map scope_clause cls

and scope_function exp =
  match exp with
  | E_paren f -> scope_function f
  | E_parameter p -> scope_intrinsic_function_name p
  | E_variable v -> scope_intrinsic_function_name v
  | E_number s ->
    failwith (Printf.sprintf "Number %s is not a function" s)
  | E_string s ->
    failwith (Printf.sprintf "String %S is not a function" s)
  | E_matrix _ ->
    failwith (Printf.sprintf "A matrix is not a function.")
  | E_apply (_, _)
  | E_binary (_, _, _)
  | E_bracket _
  | E_if (_, _, _) | E_switch _
  | E_record_access (_, _)
  | E_unary (_, _) -> raise Not_found
  | E_colon | E_dollar ->
    failwith (Printf.sprintf "illegal function")
;;
