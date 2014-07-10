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

(* $Id: mk_seq.ml,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

let f x = x + 1;;

let main () =
  let arg = int_of_string (input_line stdin) in

  let y = f arg in

  print_int y;
  print_newline ()
;;

while true do main () done
;;

