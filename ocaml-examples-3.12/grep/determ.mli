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
type �tat =
  { mutable dtransitions : transition array;
    dterminal : bool }
and transition =
  | Vers of �tat
  | Rejet;;

val d�terminise : Auto.�tat -> �tat;;
val reconna�t : �tat -> string -> bool;;
