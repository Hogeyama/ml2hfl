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

(* $Id: lexuniv.mli,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

type lex�me =
   | MC of string
   | Ident of string
   | Entier of int
;;

val construire_analyseur :
     string list -> (char Stream.t -> lex�me Stream.t)
;;

val string_of_lex�me : lex�me -> string;;
