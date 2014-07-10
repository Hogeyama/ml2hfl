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

(* $Id: synthese.mli,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

open Syntaxe;;
open Types;;

type environnement = (string * schéma_de_types) list;;

val type_exp : environnement -> expression -> type_simple;;
val type_déf : environnement -> définition -> environnement;;

exception Erreur of string;;
