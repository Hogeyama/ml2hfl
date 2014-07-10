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

(* $Id: libperl.ml,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

(** A mini library to write Perl like programs in Caml. *)

let chomp s =
  Scanf.sscanf s "%s " (fun cs -> cs)
;;

let split c s =
  let l = String.length s in
  let rec loop accu i =
    if i >= l then "" :: accu else
    try
      let j = String.index_from s i c in
      loop (String.sub s i (j - i) :: accu) (j + 1)
    with Not_found ->
      String.sub s i (l - i) :: accu in
  List.rev (loop [] 0)
;;

let lines_of_ic ic =
  let rec loop accu =
    try let l = input_line ic in
        loop (l :: accu)
    with End_of_file -> accu in
  List.rev (loop [])
;;

let lines_of_file fname =
  let ic = open_in fname in
  let lines = lines_of_ic ic in
  close_in ic;
  lines
;;
