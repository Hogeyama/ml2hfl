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

(* $Id: print_ast.ml,v 1.4 2011-08-08 19:11:31 weis Exp $ *)

open Format;;
open Parsetree;;

let rec print ppf = function
  | E_number n -> fprintf ppf "%s" n
  | E_variable v -> fprintf ppf "%s" v
  | E_if (cond, e_then, e_else) ->
    fprintf ppf "@[<v>if@ %a@ then@ %a@ else@ %a@ fi@]"
    print cond print e_then print e_else
  | E_apply (e_f, e_args) ->
    let e_f =
      match List.length e_args with
      | 1 ->
        (match e_f with
         | E_variable ("+" | "-" as op) -> E_variable ("1" ^ op)
         | e_f -> e_f)
      | _ -> e_f in
    fprintf ppf "@[<v>%a@ %a@]"
      print_args e_args print e_f

and print_args ppf = function
  | [] -> ()
  | arg :: args ->
    print ppf arg;
    List.iter (function arg -> fprintf ppf "@ %a" print arg) args
;;
