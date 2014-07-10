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

(* $Id: reverse_rec.ml,v 1.5 2011-08-08 19:31:17 weis Exp $ *)

(** Reverses the lines contained into a file.
   Simple recursive version; just reads the data from [stdin]. *)
let rec reverse_input ic =
  try
    let l = input_line ic in
    reverse_input ic;
    print_endline l
  with
  | End_of_file -> ()
;;

if !Sys.interactive then () else reverse_input stdin
;;
