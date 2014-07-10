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

(* $Id: print_polish.ml,v 1.1 2011-08-08 19:09:58 weis Exp $ *)

open Format;;

let rec print ppf = function
  | accu_args, accu_ops ->
    fprintf ppf "@[<v>args_stack =@ %a,@ ops_stack =@ %a@]"
       print_accu_args accu_args
       print_accu_ops accu_ops

and print_accu_args ppf args =
  let print_args ppf l = List.iter (fun arg -> fprintf ppf "%s;@ " arg) l in
  fprintf ppf "@[<v>%a@]" print_args args

and print_accu_ops ppf = print_accu_args ppf
;;
