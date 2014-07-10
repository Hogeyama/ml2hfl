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

(* $Id: read.ml,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

open Types;;

let digit_of_char = function
  | '1' -> Types.One
  | '2' -> Two
  | '3' -> Three
  | '4' -> Four
  | '5' -> Five
  | '6' -> Six
  | '7' -> Seven
  | '8' -> Eight
  | '9' -> Nine
  | 'A' -> Ten
  | 'B' -> Eleven
  | 'C' -> Twelve
  | 'D' -> Thirteen
  | 'E' -> Fourteen
  | 'F' -> Fifteen
  | c -> assert false
;;

let current_position ib =
  Scanf.bscanf ib "%l%n" (fun ln cn -> ln, cn)
;;

let skip_delimitors ib = Scanf.bscanf ib " %[-] " (fun s ->
 (* prerr_endline (Printf.sprintf "skip delimitors `%s'" s); *)
 s)
;;

let rec read_one_digit ib =
  Scanf.bscanf ib "%c" (function
  | '1'..'9' | 'a' .. 'f' as c -> Some (digit_of_char c)
  | ' ' -> None
  | '|' | '-' -> read_one_digit ib
  | '\n' -> Scanf.bscanf ib " %r" read_one_digit (fun d -> d)
  | c ->
    let line_number, character_number = current_position ib in
    failwith
      (Printf.sprintf
         "%s: line %i, character %i, invalid character `%c'"
         (Scanf.Scanning.name_of_input ib) line_number character_number c))
;;

let read_row len ib =
  Array.init len
    (fun i ->
     Utils.debug_message (Printf.sprintf "one digit %i" i);
     read_one_digit ib)
;;

let read_board_gen h w ib =
  ignore (skip_delimitors ib);
  let oboard =
    Array.init (h * w)
      (fun i ->
       Utils.debug_message (Printf.sprintf "one row %i" i);
       ignore (skip_delimitors ib);
       let row = read_row (h * w) ib in
       row) in
  { out_h = h; out_w = w;
    out_height = h * w; out_width = w * h;
    out_board = oboard;
  }
;;

let read_board h w ib = read_board_gen h w ib
;;

let input_board h w = read_board h w Scanf.Scanning.stdib
;;

let board_of_string h w s = read_board h w (Scanf.Scanning.from_string s)
;;

let board_of_file h w fname = read_board h w (Scanf.Scanning.from_file fname)
;;
