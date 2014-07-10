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

(* $Id: prop.mli,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

type proposition =
   | Vrai
   | Faux
   | Non of proposition
   | Et of proposition * proposition
   | Ou of proposition * proposition
   | Implique of proposition * proposition
   | Équivalent of proposition * proposition
   | Variable of string
;;

exception Réfutation of (string * bool) list;;

val vérifie_tautologie : proposition -> string list -> unit;;
val variables_libres : proposition -> string list;;

