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

(* $Id: scoping.ml,v 1.3 2011-08-08 19:11:31 weis Exp $ *)

open Parsetree;;

let scope_operator = function
  | "+" | "*" | "-" | "**" | "^"
  | "==" | "~=" | "<>"
  | "<" | ">" | ">=" | "<="
  | "|" | "&" | "~" -> ()
  | s -> failwith (Printf.sprintf "unknown operator %S" s)
;;

let scope_intrinsic_function_name = function
  | "sin" | "cos" | "tan" | "atan" | "atan2"
  | "log" | "log10"
  | "exp" -> ()
  | s -> failwith (Printf.sprintf "Unknown function %S" s)
;;

let scope_variable s =
  let length_s = String.length s in
  assert (length_s > 0);
  let c = s.[0] in
  match c with
  | 'x' | 'y' | 'z' | 'u' ->
    let lim = length_s in
    let rec check_rest i =
      if i >= lim then () else
      (match s.[i] with
       | '0'..'9' -> check_rest (i + 1)
       | _ -> failwith (Printf.sprintf "illegal variable %S" s)) in
    check_rest 1
  | _ -> failwith (Printf.sprintf "unknown variable %S" s)
;;

let scope_function_variable s =
  let length_s = String.length s in
  assert (length_s > 0);
  let c = s.[0] in
  match c with
  | 'a'..'z' | 'A'..'Z' -> ()
  | _ -> scope_operator s
;;

let scope_function = function
  | E_variable v -> scope_function_variable v
  | E_number _
  | E_if (_, _, _)
  | E_apply (_, _) ->
    failwith (Printf.sprintf "illegal function")
;;

let rec scope = function
  | E_number n -> ()
  | E_variable v -> scope_variable v
  | E_if (cond, e_then, e_else) ->
    scope cond; scope e_then; scope e_else
  | E_apply (e_f, e_args) ->
    scope_function e_f;
    scope_args e_args

and scope_args = function
  | [] -> ()
  | arg :: args ->
    scope arg;
    List.iter scope args
;;
