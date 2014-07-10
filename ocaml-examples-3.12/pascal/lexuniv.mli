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
type lexème =
   | MC of string
   | Ident of string
   | Entier of int;;

val construire_analyseur :
     string list -> (char Stream.t -> lexème Stream.t);;

val string_of_lexème : lexème -> string;;
