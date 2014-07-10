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

(* $Id: bonjour.ml,v 1.4 2011-08-08 19:31:17 weis Exp $ *)

(* Le programme interactif [bonjour]: on crée un bouton qui, lorsqu'il
   est pressé, appelle une fonction qui écrit "Bonjour!" au terminal.

Compilation:
 ocamlc -I +labltk -c bonjour.ml
Édition des liens
 ocamlc -I +labltk -custom labltk.cma  bonjour.cmo -o bonjour

Essayez en lançant ./bonjour

*)

open Camltk;;

let action () = print_string "Bonjour!"; print_newline ();;

let bonjour () =
  let fenêtre_principale = openTk () in
  let bouton_press =
    Button.create fenêtre_principale
      [Text "Pressez-moi"; Command action] in
  pack [bouton_press] [];
  mainLoop ()
;;

if !Sys.interactive then () else begin bonjour (); closeTk () end;;

