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

(* $Id: print_ast.ml,v 1.5 2011-08-08 18:21:34 weis Exp $ *)

open Format;;
open Parsetree;;

let rec print ppf = function
  | E_number n -> fprintf ppf "%s" n
  | E_variable v -> fprintf ppf "%s" v
  | E_if (cond, e_then, e_else) ->
    fprintf ppf "if %a then %a; else %a; end"
    print cond print e_then print e_else
  | E_apply (e_f, e_args) ->
    fprintf ppf "%a (%a)"
    print e_f print_args e_args
  | E_apply_unknown (e_f, e_args) ->
    fprintf ppf "%a (%a)"
    print e_f print_args e_args
  | E_prefix (op, x) ->
    fprintf ppf "%s (%a)"
      op
      print x
  | E_binary (op, x, y) ->
    fprintf ppf "%a %s %a"
      print x
      op
      print y
  | E_paren e ->
    fprintf ppf "(%a)" print e
  | E_record_access (e1, label) ->
    fprintf ppf "%a.%s"
      print e1
      label

and print_args ppf = function
  | [] -> ()
  | arg :: args ->
    print ppf arg;
    List.iter (function arg -> fprintf ppf ", %a" print arg) args
;;
