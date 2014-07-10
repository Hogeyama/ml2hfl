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
type lex�me =
   | MC of string
   | Ident of string
   | Entier of int;;

val construire_analyseur :
     string list -> (char Stream.t -> lex�me Stream.t);;

val string_of_lex�me : lex�me -> string;;
