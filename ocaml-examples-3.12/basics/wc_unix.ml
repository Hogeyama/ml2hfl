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

(* $Id: wc_unix.ml,v 1.5 2011-08-08 19:31:17 weis Exp $ *)

(**

- Record types must be defined before usage in Caml.
  The field contents of a record cannot be modified,
  unless the field has explicitly been declared mutable when its associated
  record type was defined.

- If [f] is a field of record [r], then [r.f] denotes the contents of the
  field [f].

- If [f] is a mutable field of record [r], then [r.f <- v] writes [v] in
  the field [f] of [r].
*)
type counts = {
 mutable chars : int;
 mutable lines : int;
 mutable words : int;
}
;;

let wc_count = {chars = 0; lines = 0; words = 0}
and wc_count_total = {chars = 0; lines = 0; words = 0}
;;

let reset_wc_count () =
 wc_count.chars <- 0; wc_count.lines <- 0; wc_count.words <- 0;;

let accumulate_wc_count () =
 wc_count_total.chars <- wc_count_total.chars + wc_count.chars;
 wc_count_total.lines <- wc_count_total.lines + wc_count.lines;
 wc_count_total.words <- wc_count_total.words + wc_count.words
;;

let rec counter ic iw =
 let c = input_char ic in
 wc_count.chars <- wc_count.chars + 1;
 match c with
 | '\n' ->
   wc_count.lines <- wc_count.lines + 1;
   counter ic false
 | ' ' | '\t' ->
   counter ic false
 | _ ->
   if not iw then wc_count.words <- wc_count.words + 1 else ();
   counter ic true
;;

let count_channel ic =
 reset_wc_count ();
 try counter ic false with
 | End_of_file -> accumulate_wc_count (); close_in ic
;;

(**
- The [Printf.printf] function is the Caml analogous of
  the C [printf] function.
*)
let print_wc l w c f =
 Printf.printf "%10d%10d%10d %s\n" l w c f
;;

let print_wc_file f =
 print_wc wc_count.lines wc_count.words wc_count.chars f
;;

let print_wc_total () =
 print_wc
  wc_count_total.lines wc_count_total.words wc_count_total.chars "total"
;;

let count_file file_name =
 try
   count_channel (open_in file_name);
   print_wc_file file_name with
 | Sys_error s -> print_string s; print_newline (); exit 2
;;

let main () =
 let args = Sys.argv in
 let nb_files = Array.length args - 1 in
 for i = 1 to nb_files do
  count_file args.(i)
 done;
 if nb_files > 1 then print_wc_total ();
 exit 0
;;

if !Sys.interactive then () else main ()
;;
