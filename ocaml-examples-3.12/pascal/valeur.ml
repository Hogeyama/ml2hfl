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

(* $Id: valeur.ml,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

type valeur =
   | Inconnue
   | Ent of int
   | Bool of bool
   | Tableau of int * valeur array
;;

exception Erreur_exécution of string;;

let ent_val = function
  | Ent n -> n
  | Inconnue | Bool _ | Tableau (_, _ ) ->
      raise (Erreur_exécution "entier attendu")
and bool_val = function
  | Bool b -> b
  | Inconnue | Ent _ | Tableau (_, _ ) ->
      raise (Erreur_exécution "booléen attendu")
and tableau_val = function
  | Tableau (inf, t) -> (inf, t)
  | Inconnue | Ent _ | Bool _ ->
      raise (Erreur_exécution "tableau attendu")
;;

let affiche_valeur v =
  print_int (ent_val v); print_newline ();;

let lire_valeur () =
  let entrée = read_line () in
  try Ent (int_of_string entrée)
  with Failure _ -> raise (Erreur_exécution "erreur de lecture")
;;
