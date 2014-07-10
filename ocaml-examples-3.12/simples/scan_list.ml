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

(* $Id: scan_list.ml,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

(** Defining [split] in Caml. *)

(** We define various list scanners using the [Scanf] Caml module,
    we use those scanners to define split. *)

open Scanf;;

(* Using kscanf only. *)
let rec scan_elems ib scan_elem accu =
  kscanf ib (fun ib exc -> accu)
   scan_elem
   (fun i ->
      let accu = i :: accu in
      kscanf ib (fun ib exc -> accu)
       " %1[;] "
       (fun s -> if s = "" then accu else scan_elems ib scan_elem accu))
;;

let scan_list scan_elem ib =
  bscanf ib "[ " ();
  let accu = scan_elems ib scan_elem [] in
  bscanf ib " ]" ();
  List.rev accu
;;

let scan_list elem sep ib =
  let rec loop accu =
    Scanf.bscanf ib elem
     (fun e ->
        Scanf.kscanf ib
          (fun ib exc ->
            if Scanning.end_of_input ib then e :: accu else raise exc)
          sep
          (fun s -> if s = "" then (e :: accu) else loop (e :: accu))) in
  List.rev (loop [])
;;

(* Examples:
let f = scan_list " %[^#]" " %1[#] ";;
let ib = Scanning.from_string "asda#asd#asc";;

# f ib;;
- : string list = ["asda"; "asd"; "asc"]
*)

let scan_list elem sep ib =
  let rec loop accu =
    if Scanning.end_of_input ib then accu else
    Scanf.bscanf ib elem
     (fun e ->
        Scanf.kscanf ib
          (fun ib exc ->
            if Scanning.end_of_input ib then e :: accu else raise exc)
          "%1c"
          (fun c ->
            if c = sep then loop (e :: accu) else
            failwith (Printf.sprintf "scan_list: wrong separator %C" c))) in
  List.rev (loop [])
;;

(* Examples:
let f =  scan_list "%[^#]" '#';;
let ib = Scanning.from_string "asda#asd#asc";;
f ib;;
# - : string list = ["asda"; "asd"; "asc"]

let f = scan_list " %[^# ] " '#';;
let ib = Scanning.from_string "asda # asd # asc";;
#  f ib;;
- : string list = ["asda"; "asd"; "asc"]

let f =  scan_list "%[^# ] " '#';;
let ib = Scanning.from_string "asda # asd # asc";;
f ib;;
*)

let split c = scan_list "%[^&]" c;;

let scan_int_list = scan_list "%i";;
let scan_string_list = scan_list "%S";;
let scan_bool_list = scan_list "%B";;
let scan_char_list = scan_list "%C";;
let scan_float_list = scan_list "%f";;

(* Tests:
let test33 () =
  scan_int_list (Scanning.from_string "[]") = [] &&
  scan_int_list (Scanning.from_string "[ ]") = [] &&
  scan_int_list (Scanning.from_string "[ 1 ]") = [1] &&
  scan_int_list (Scanning.from_string "[ 1; 2; 3; 4 ]") = [1; 2; 3; 4] &&
  scan_int_list (Scanning.from_string "[1;2;3;4;]") = [1; 2; 3; 4]
;;

test (test33 ())
;;

let test34 () =
  scan_string_list (Scanning.from_string "[]") = [] &&
  scan_string_list (Scanning.from_string "[ ]") = [] &&
  scan_string_list (Scanning.from_string "[ \"1\" ]") = ["1"] &&
  scan_string_list
    (Scanning.from_string "[\"1\"; \"2\"; \"3\"; \"4\"]") =
    ["1"; "2"; "3"; "4"] &&
  scan_string_list
    (Scanning.from_string "[\"1\"; \"2\"; \"3\"; \"4\";]") =
    ["1"; "2"; "3"; "4"]
;;

test (test34 ())
;;

fun c ib ->
  let fb =
    Scanf.Scanning.from_string (Printf.sprintf " %%[^%c ] " c) in
  Scanf.bscanf fb "%{%s%}"
  (fun fmt ->
   Scanf.bscanf ib fmt)
;;
*)

let split c =
  let fb =
    Scanf.Scanning.from_string (Printf.sprintf " %%[^%c ] " c) in
  Scanf.bscanf fb "%{%s%}"
  (fun fmt -> scan_list fmt c)
;;
