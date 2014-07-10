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

(* $Id: rgb.ml,v 1.4 2011-08-08 19:31:17 weis Exp $ *)

open Camltk;;

let rgb () =
  let fen�tre_principale = openTk () in

  let cr�er_glissi�re nom =
    Scale.create fen�tre_principale
      [Label nom; From 0.0; To 255.0;
       Length(Centimeters 10.0); Orient Horizontal] in

  let rouge = cr�er_glissi�re "Rouge"
  and vert = cr�er_glissi�re "Vert"
  and bleu = cr�er_glissi�re "Bleu"
  and �chantillon =
    Frame.create fen�tre_principale
      [Height (Centimeters 1.5); Width (Centimeters 6.0)]
  and quitter =
    Button.create fen�tre_principale
      [Text "Quitter"; Command closeTk] in

  let rafra�chir_couleur x =
    let r = int_of_float (Scale.get rouge)
    and v = int_of_float (Scale.get vert)
    and b = int_of_float (Scale.get bleu) in
    let couleur = Printf.sprintf "#%02x%02x%02x" r v b in
    Frame.configure �chantillon [Background (NamedColor couleur)] in

  Scale.configure rouge [ScaleCommand rafra�chir_couleur];
  Scale.configure vert [ScaleCommand rafra�chir_couleur];
  Scale.configure bleu [ScaleCommand rafra�chir_couleur];
  pack [rouge; vert; bleu] [Side Side_Top];
  pack [quitter] [Side Side_Bottom];
  pack [�chantillon] [Side Side_Bottom; PadY (Millimeters 2.0)];
  mainLoop ()
;;

if !Sys.interactive then () else begin rgb (); exit 0 end;;
