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
type état =
  { mutable dtransitions : transition array;
    dterminal : bool }
and transition =
  | Vers of état
  | Rejet;;

val déterminise : Auto.état -> état;;
val reconnaît : état -> string -> bool;;
