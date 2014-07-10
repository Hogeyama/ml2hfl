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

(* $Id: splice_ast.mli,v 1.1 2011-08-08 19:09:58 weis Exp $ *)

open Ast;;
(* For list1 *)
open Deep_ast;;
(* For unop and binop and label *)

type expression =
   | S_colon
   | S_dollar
   | S_number of string
   | S_string of string
   | S_matrix of expression list list
   | S_variable of string
   | S_parameter of int
   | S_if of expression * expression * expression
   | S_switch of switch_desc
   | S_unary of unop * expression
   | S_binary of binop * expression * expression
   | S_ternary of ternop * expression * expression * expression
   | S_quaternary of
       quaternop * expression * expression * expression * expression
   | S_apply of expression * expression list
   | S_record_access of expression * label
   | S_paren of expression
   | S_bracket of expression
   | S_splice of expression

and switch_desc = {
  sswitch_subject : expression;
  sswitch_cases : clauses;
  sswitch_default : expression;
}

and clauses = clause list

and clause = pattern list * expression

and pattern = expression
;;
