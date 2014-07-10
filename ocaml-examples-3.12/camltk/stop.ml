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

(* $Id: stop.ml,v 1.4 2011-08-08 19:31:17 weis Exp $ *)

(* The Stop program: creates a single button with a call back function
that quits the program.

Compile using:
 ocamlc -I +labltk -c stop.ml
Link using:
 ocamlc -I +labltk -custom labltk.cma  stop.cmo -o stop

Try with ./stop

*)

open Camltk;;

let action () = closeTk ();;

let stop () =
  let main_window = openTk () in
  let bouton_quit =
    Button.create main_window
      [Text "Stop"; Command action] in
  pack [bouton_quit] [];
  mainLoop ()
;;

if !Sys.interactive then () else begin stop (); exit 0 end;;
