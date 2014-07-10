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

let interpr�te_fichier nom =
  try
    let canal = open_in nom in
    try
      let prog = lire_programme (Stream.of_channel canal) in
      close_in canal;
      Typage.type_programme prog;                 (* ligne ajout�e *)
      Interp.ex�cute_programme prog
    with
    | Stream.Error s ->
        prerr_string
          ("Erreur de syntaxe : " ^ s ^ " aux alentours du caract�re num�ro ");
        prerr_int (pos_in canal);
        prerr_endline ""
    | Typage.Erreur_typage err ->
        Typage.affiche_erreur err;
        exit 2
    | Valeur.Erreur_ex�cution message ->
        prerr_string "Erreur pendant l'ex�cution: ";
        prerr_endline message
  with
  | Sys_error message ->
      prerr_string "Erreur du syst�me: ";
      prerr_endline message
  | Stream.Failure -> ()
;;

if not !Sys.interactive
  then begin interpr�te_fichier Sys.argv.(1); exit 0 end;;
