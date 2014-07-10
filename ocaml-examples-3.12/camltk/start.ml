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

(* $Id: start.ml,v 1.4 2011-08-08 19:31:17 weis Exp $ *)

(* The Hello world program in CamlTk: creates a single button with a
text on it.

Compile using:
 ocamlc -I +labltk -c start.ml
Link using:
 ocamlc -I +labltk -custom labltk.cma  start.cmo -o start

Try with ./start

*)

open Camltk;;

let start () =
  let main_window = openTk () in
  let button =
    Button.create main_window
      [Text "Hello World!"] in
  pack [button] [];
  mainLoop ()
;;

if !Sys.interactive then () else start ()
;;
