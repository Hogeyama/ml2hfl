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

(* $Id: fileprio.ml,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

exception File_vide;;

type 'a t =
   | Vide | File of int * 'a * 'a t * 'a t
;;

let vide = Vide;;

let rec enlève_sommet = function
  | Vide -> raise File_vide
  | File (_prio, _elt, Vide, Vide) -> Vide
  | File (_prio, _elt, gauche, Vide) -> gauche
  | File (_prio, _elt, Vide, droite) -> droite
  | File (_prio, _elt,
          (File (prio_g, elt_g, _, _) as gauche),
          (File (prio_d, elt_d, _, _) as droite)) ->
    if prio_g < prio_d
    then File (prio_g, elt_g, enlève_sommet gauche, droite)
    else File (prio_d, elt_d, gauche, enlève_sommet droite)
;;

let extraire = function
  | Vide -> raise File_vide
  | File (prio, elt, _, _) as file -> (prio, elt, enlève_sommet file)
;;

let rec ajoute file prio elt =
  match file with
  | Vide -> File (prio, elt, Vide, Vide)
  | File (prio1, elt1, gauche, droite) ->
    if prio <= prio1
    then File (prio, elt, ajoute droite prio1 elt1, gauche)
    else File (prio1, elt1, ajoute droite prio elt, gauche)
;;
