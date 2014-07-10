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

type état =
  { mutable transitions : (char * état) list;
    mutable epsilon_transitions : état list;
    mutable terminal : bool;
    numéro : int };;

val expr_vers_automate : expr -> état;;
