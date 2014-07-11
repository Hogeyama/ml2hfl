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

let enl�ve_suffixe_cpr cha�ne =
  let longueur = String.length cha�ne in
  if longueur < 4 || String.sub cha�ne (longueur - 4) 4 <> ".cpr"
  then raise Mauvais_suffixe
  else String.sub cha�ne 0 (longueur - 4)
;;

let erreur = ref false;;

for i = 1 to Array.length argv - 1 do
  let (nom_entr�e, nom_sortie) =
    try
      (argv.(i), enl�ve_suffixe_cpr argv.(i))
    with Mauvais_suffixe ->
      (argv.(i) ^ ".cpr", argv.(i)) in
  try
    let entr�e =
      try
        open_in_bin nom_entr�e
      with Sys_error message ->
        prerr_endline
          ("Erreur � l'ouverture de " ^ nom_entr�e ^ " : " ^ message);
        raise Erreur in
    let sortie =
      try
        open_out_bin nom_sortie
      with Sys_error message ->
        close_in entr�e;
        prerr_endline
          ("Erreur � la cr�ation de " ^ nom_sortie ^ " : " ^ message);
        raise Erreur in
    try
      Huffman.d�compresse entr�e sortie;
      close_in entr�e; close_out sortie; remove nom_entr�e
    with Sys_error message ->
      close_in entr�e; close_out sortie; remove nom_sortie;
      prerr_endline
        ("Erreur pendant la compression de " ^ nom_entr�e ^ " : " ^ message);
      raise Erreur
  with Erreur ->
    erreur := true
done;

exit (if !erreur then 2 else 0)
;;