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

(* $Id: eval.mli,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

open Syntaxe;;

type valeur =
   | Val_nombre of int
   | Val_booléenne of bool
   | Val_paire of valeur * valeur
   | Val_nil
   | Val_cons of valeur * valeur
   | Val_fermeture of fermeture
   | Val_primitive of (valeur -> valeur)

and fermeture =
  { définition : (motif * expression) list;
    mutable environnement : environnement }

and environnement = (string * valeur) list
;;

val évalue: environnement -> expression -> valeur;;
val évalue_définition: environnement -> définition -> environnement;;
val imprime_valeur: valeur -> unit;;

exception Erreur of string;;
