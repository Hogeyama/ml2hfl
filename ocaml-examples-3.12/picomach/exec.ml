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

(* $Id: exec.ml,v 1.2 2011-08-08 19:31:17 weis Exp $ *)

open Code;;
open Simul;;

exception Fichier_incorrect;;

let exécute_fichier nom_fichier taille_mémoire =
  let canal = open_in_bin nom_fichier in
  let programme =
    try (input_value canal : instruction array)
    with Failure _ -> raise Fichier_incorrect in
  close_in canal;
  exécute programme taille_mémoire
;;

exception Mauvais_arguments;;

if !Sys.interactive then () else
try
  if Array.length Sys.argv < 2 then raise Mauvais_arguments;
  let taille_mémoire =
    if Array.length Sys.argv < 3
    then 1024
    else try int_of_string Sys.argv.(2)
         with Failure _ -> raise Mauvais_arguments in
  exécute_fichier Sys.argv.(1)
                  (taille_du_mot * taille_mémoire);
  exit 0
with
| Mauvais_arguments ->
    prerr_endline "Usage: pico_run <fichier> [taille mémoire]";
    exit 2
| Fichier_incorrect ->
    prerr_endline "Le fichier ne contient pas du code exécutable";
    exit 2
| Erreur(message, param) ->
    prerr_string "Erreur à l'exécution: ";
    prerr_string message;
    prerr_string " ("; prerr_int param; prerr_endline ")";
    exit 2
| Sys_error message ->
    prerr_string "Erreur du système: "; prerr_endline message;
    exit 2
;;
