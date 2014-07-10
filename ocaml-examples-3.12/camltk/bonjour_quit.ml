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

(* $Id: bonjour_quit.ml,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

open Tk;;

let action_press () = print_string "Bonjour!"; print_newline ();;

let hello_quit () =
  let fenêtre_principale = openTk () in
  let bouton_press =
    Button.create fenêtre_principale
                   [Text "Pressez-moi"; Command action_press] in
  let bouton_quit =
    Button.create fenêtre_principale
      [Text "Quittez-moi"; Command closeTk] in
  pack [bouton_press; bouton_quit] [Side Side_Left];
  mainLoop ()
;;

if !Sys.interactive then () else begin hello_quit(); exit 0 end;;
