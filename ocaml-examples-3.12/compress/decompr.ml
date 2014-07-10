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

(* $Id: decompr.ml,v 1.4 2011-08-08 19:31:17 weis Exp $ *)

open Sys;;

exception Erreur;;

exception Mauvais_suffixe;;

let enlève_suffixe_cpr chaîne =
  let longueur = String.length chaîne in
  if longueur < 4 || String.sub chaîne (longueur - 4) 4 <> ".cpr"
  then raise Mauvais_suffixe
  else String.sub chaîne 0 (longueur - 4)
;;

let erreur = ref false;;

for i = 1 to Array.length argv - 1 do
  let (nom_entrée, nom_sortie) =
    try
      (argv.(i), enlève_suffixe_cpr argv.(i))
    with Mauvais_suffixe ->
      (argv.(i) ^ ".cpr", argv.(i)) in
  try
    let entrée =
      try
        open_in_bin nom_entrée
      with Sys_error message ->
        prerr_endline
          ("Erreur à l'ouverture de " ^ nom_entrée ^ " : " ^ message);
        raise Erreur in
    let sortie =
      try
        open_out_bin nom_sortie
      with Sys_error message ->
        close_in entrée;
        prerr_endline
          ("Erreur à la création de " ^ nom_sortie ^ " : " ^ message);
        raise Erreur in
    try
      Huffman.décompresse entrée sortie;
      close_in entrée; close_out sortie; remove nom_entrée
    with Sys_error message ->
      close_in entrée; close_out sortie; remove nom_sortie;
      prerr_endline
        ("Erreur pendant la compression de " ^ nom_entrée ^ " : " ^ message);
      raise Erreur
  with Erreur ->
    erreur := true
done;

exit (if !erreur then 2 else 0)
;;
