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

(* $Id: ipascal.ml,v 1.2 2011-08-08 19:31:17 weis Exp $ *)

open Syntaxe;;

let interprète_fichier nom =
  try
    let canal = open_in nom in
    try
      let prog = lire_programme (Stream.of_channel canal) in
      close_in canal;
      Typage.type_programme prog;                 (* ligne ajoutée *)
      Interp.exécute_programme prog
    with
    | Stream.Error s ->
        prerr_string
          ("Erreur de syntaxe : " ^ s ^ " aux alentours du caractère numéro ");
        prerr_int (pos_in canal);
        prerr_endline ""
    | Typage.Erreur_typage err ->
        Typage.affiche_erreur err;
        exit 2
    | Valeur.Erreur_exécution message ->
        prerr_string "Erreur pendant l'exécution: ";
        prerr_endline message
  with
  | Sys_error message ->
      prerr_string "Erreur du système: ";
      prerr_endline message
  | Stream.Failure -> ()
;;

if not !Sys.interactive
  then begin interprète_fichier Sys.argv.(1); exit 0 end;;
