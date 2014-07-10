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

(* $Id: trad.ml,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

let mots = Libperl.lines_of_file "mots_islandais";;

let trad () =
  let ln = Random.int (List.length mots) in

  let mot, traduction, typ, cas =
    match Libperl.split '&' (List.nth mots ln) with
    | [mot; traduction; typ; cas] -> mot, traduction, typ, cas
    | _ -> assert false in

  Printf.printf "%i\t%s ?" ln mot; print_newline ();
  let _input = input_line stdin in
  Printf.printf "%s\t%s\t%s\n" traduction typ cas; print_newline ();;

while true do trad () done
;;
