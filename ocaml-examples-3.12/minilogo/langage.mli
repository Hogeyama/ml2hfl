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

(* $Id: langage.mli,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

type nombre =
   | Entier of int
   | Flottant of float
;;

type expression =
   | Constante of nombre
   | Somme of expression * expression
   | Produit of expression * expression
   | Diff�rence of expression * expression
   | Quotient of expression * expression
   | Variable of string
;;

type ordre =
   | Av of expression | Re of expression
   | Td of expression | Tg of expression
   | Lc | Bc
   | Ve
   | Rep of expression * ordre list
   | Stop
   | Si of expression * expression * ordre list * ordre list
   | Ex�cute of string * expression list
;;

type proc�dure = {param�tres : string list; corps : ordre list};;

type phrase_logo =
   | Pour of string * proc�dure
   | Ordre of ordre
;;

type programme_logo = Programme of phrase_logo list;;

val ex�cute_phrase : phrase_logo -> unit;;
val ex�cute_programme : programme_logo -> unit;;
