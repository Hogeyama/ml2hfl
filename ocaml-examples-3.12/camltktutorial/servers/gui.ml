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

(* $Id: gui.ml,v 1.6 2011-08-08 19:31:17 weis Exp $ *)

(* The GUI of the simple computation server.
   The GUI has two buttons to interact with the computation server:
   - a button to clik on,
   - a button to report the number of clicks so far.
   A third button is available to quit the GUI.

The GUI only recognizes the new click count that the computation
server should return. *)

open Camltk;;

let parse_compute_message() =
  let s = input_line stdin in
  try
    ignore (int_of_string s); s
  with
  | Failure _ -> "Invalid server response";;

let bclick_cb b () =
  (* Output a message to the computation server. *)
  output_string stdout "button\n"; flush stdout;
  (* Parse its answer and act accordingly. *)
  let s = parse_compute_message () in
  Button.configure b [Text s]
;;

let topwindow = openTk ();;

let bcount = Button.create topwindow [Text "never clicked"];;

let bclick = Button.create topwindow
  [Text "hello world!"; Command (bclick_cb bcount)];;

let bquit = Button.create topwindow [Text "Quit"; Command closeTk];;

let gui_server () =
  Button.configure bquit
    [Background (NamedColor "red"); Foreground (NamedColor "white")];
  pack [bclick; bcount; bquit] [Side Side_Left];
  mainLoop()
;;

gui_server ()
;;
