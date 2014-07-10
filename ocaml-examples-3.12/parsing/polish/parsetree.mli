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

(* $Id: parsetree.mli,v 1.2 2011-08-08 19:11:31 weis Exp $ *)

type expression =
   | E_number of string
   | E_variable of string
   | E_if of expression * expression * expression
   | E_apply of expression * expression list
;;
