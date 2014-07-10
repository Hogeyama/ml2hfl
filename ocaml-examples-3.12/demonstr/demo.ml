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

(* $Id: demo.ml,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

open Prop;;
open Asynt;;

let examine cha�ne =
  let proposition = analyse_proposition cha�ne in
  let variables = variables_libres proposition in
  try
    v�rifie_tautologie proposition variables;
    begin match variables with
    | [] ->
        print_string "Th�or�me: "
    | [var] ->
        print_string ("Th�or�me: pour toute proposition "^var^", ")
    | _ ->
        print_string "Th�or�me: pour toutes propositions ";
        List.iter (function var -> print_string (var^", ")) variables
    end;
    print_string cha�ne;
    print_newline()
  with R�futation liaisons ->
    print_string (cha�ne ^ " n'est pas un th�or�me,\n");
    print_string "car la proposition est fausse quand\n";
    List.iter
     (function (var, b) ->
       print_string (var ^ " est ");
       print_string (if b then "vraie" else "fausse");
       print_newline ())
     liaisons
;;

let boucle () =
  try
    while true do
      print_string ">>> "; examine(read_line())
    done
  with End_of_file -> ()
;;

if !Sys.interactive then () else begin boucle(); exit 0 end;;
