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

(* $Id: complexe0.ml,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

let c = ref 0;;

let ic = open_in "ebi";;
let oc = open_out "ebo";;


let parse_gui_message () =
  let s = input_line ic in
  match s with
  | "bouton" -> incr c; string_of_int !c
  | s -> "syntax error: " ^ s
;;

let treat () =
  let answer = parse_gui_message () in
  output_string oc answer;
  output_char oc '\n';
  flush oc
;;

while true do treat () done;;
