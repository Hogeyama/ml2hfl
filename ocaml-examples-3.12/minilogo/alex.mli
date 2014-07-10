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

(* $Id: alex.mli,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

type lexème =
   | Mot of string
   | Symbole of char
   | Constante_entière of int
   | Constante_flottante of float
;;

val analyseur_lexical : char Stream.t -> lexème Stream.t;;

val string_of_lexème :  lexème -> string;;
