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

(* $Id: hello.ml,v 1.4 2011-08-08 19:31:17 weis Exp $ *)

(* The interactive Hello world program: creates a single button with a
call back function to print "Hello world!" on standard input.

Compile using:
 ocamlc -I +labltk -c hello.ml
Link using:
 ocamlc -I +labltk -custom labltk.cma  hello.cmo -o hello

Try with ./hello

*)

open Camltk;;

let action () = print_string "Hello world!"; print_newline ();;

let hello () =
  let main_window = openTk () in
  let bouton_press =
    Button.create main_window
      [Text "Press me"; Command action] in
  pack [bouton_press] [];
  mainLoop ()
;;

if !Sys.interactive then () else begin hello (); closeTk () end;;
