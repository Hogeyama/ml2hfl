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
type valeur =
   | Inconnue
   | Ent of int
   | Bool of bool
   | Tableau of int * valeur array;;

exception Erreur_exécution of string;;

val ent_val : valeur -> int;;
val bool_val : valeur -> bool;;
val tableau_val : valeur -> int * valeur array;;
val affiche_valeur : valeur -> unit;;
val lire_valeur : unit -> valeur;;
