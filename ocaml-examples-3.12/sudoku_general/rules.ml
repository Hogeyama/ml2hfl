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

(* $Id: rules.ml,v 1.2 2011-08-08 19:31:17 weis Exp $ *)

(**
Two positive rules:
 (0) Each line and each column should be a complete set of digits.
 (1) Each 3x3 square should be a complete set of digits.

Derivative (negative) rules:
 (0) No two identical digits can be in any line or column.
 (0) No two identical digits can be in any 3x3 square.
*)

