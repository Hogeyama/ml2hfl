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

(* $Id: parsetree.mli,v 1.3 2011-08-08 18:21:34 weis Exp $ *)

type operator = string;;
type label = string;;

type expression =
   | E_number of string
   | E_variable of string
   | E_if of expression * expression * expression
   | E_binary of operator * expression * expression
   | E_prefix of operator * expression
   | E_apply of expression * expression list
   | E_apply_unknown of expression * expression list
   | E_record_access of expression * label
   | E_paren of expression
;;
