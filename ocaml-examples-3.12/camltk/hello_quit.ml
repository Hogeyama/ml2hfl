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

(* $Id: hello_quit.ml,v 1.4 2011-08-08 19:31:17 weis Exp $ *)

(* The toggling buttons hello world program: creates two buttons, one
simple button to quit the program, and another one with two mutually
recursive callbacks functions. Those callback functions configure the
button with a new text and simultaneously change the current call back
function of the button to reinstall the other callback.

Compile using:
 ocamlc -I +labltk -c hello_quit.ml
Link using:
 ocamlc -I +labltk -custom labltk.cma  hello_quit.cmo -o hello_quit

Try with ./hello_quit

*)

open Camltk;;

let hello_quit () =
  let main_window = openTk () in
  let bouton_quit =
    Button.create main_window
      [Text "Quit"; Command closeTk] in
  let bouton_press = Button.create main_window [] in
  let rec action_press () =
    Button.configure bouton_press
      [Text "Hello!"; Command press_init]
  and press_init () =
    Button.configure bouton_press
      [Text "Press"; Command action_press] in
  press_init ();
  pack [bouton_press; bouton_quit] [Side Side_Left];
  mainLoop ()
;;

if !Sys.interactive then () else begin hello_quit(); exit 0 end;;
