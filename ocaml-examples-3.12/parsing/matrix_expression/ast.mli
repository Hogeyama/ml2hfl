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

(* $Id: ast.mli,v 1.1 2011-08-08 19:09:58 weis Exp $ *)

type operator = string;;
type label = string;;

type 'a list1 = 'a * 'a list
;;

type expression =
   | E_colon
   | E_dollar
   | E_number of string
   | E_string of string
   | E_matrix of expression list list
   | E_variable of string
   | E_parameter of string
   | E_if of expression * expression * expression
   | E_binary of operator * expression * expression
   | E_unary of operator * expression
   | E_apply of expression * expression list
   | E_record_access of expression * label
   | E_switch of switch_desc
   | E_bracket of expression
   | E_paren of expression

and switch_desc = {
  switch_subject : expression;
  switch_cases : (pattern list * expression) list;
  switch_default : expression;
}

and pattern = expression
;;
