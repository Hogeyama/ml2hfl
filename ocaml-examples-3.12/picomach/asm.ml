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

(* $Id: asm.ml,v 1.2 2011-08-08 19:31:17 weis Exp $ *)

let assemble_fichier nom_entr�e nom_sortie =
  let entr�e = open_in nom_entr�e in
  let sortie = open_out_bin nom_sortie in
    try
      output_value sortie
        (Lecture.programme (Stream.of_channel entr�e));
      close_in entr�e;
      close_out sortie;
      0
    with exc ->
      close_in entr�e;
      close_out sortie;
      Sys.remove nom_sortie;
      match exc with
      | Stream.Error _ ->
          prerr_string
            "Erreur de syntaxe aux alentours du caract�re num�ro ";
          prerr_int (pos_in entr�e);
          prerr_endline "";
          1
      | Stockage.Erreur message ->
          prerr_string "Erreur d'assemblage: ";
          prerr_endline message;
          1
      | _ ->
          raise exc
;;

exception Mauvais_arguments;;

if !Sys.interactive then () else
try
  if Array.length Sys.argv <> 3 then raise Mauvais_arguments;
  exit (assemble_fichier Sys.argv.(1) Sys.argv.(2))
with
| Mauvais_arguments ->
    prerr_endline
      "Usage: pico_asm <fichier assembleur> <fichier de code>";
    exit 2
| Sys_error message ->
    prerr_string "Erreur du syst�me: "; prerr_endline message;
    exit 2
;;
