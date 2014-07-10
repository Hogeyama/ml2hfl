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
open Expr;;

type �tat =
  { mutable transitions : (char * �tat) list;
    mutable epsilon_transitions : �tat list;
    mutable terminal : bool;
    num�ro : int };;

val expr_vers_automate : expr -> �tat;;
