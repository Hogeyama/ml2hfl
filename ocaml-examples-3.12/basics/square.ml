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

(* $Id: square.ml,v 1.6 2011-08-08 19:31:17 weis Exp $ *)

(**

  We learn here:

  - Arguments of a program are the elements of the predefined array
    [Sys.argv].

  - The length (or number of elements) of array [v] is returned by
    [Array.length v].

  - You access to elem number [i] of array [v] with the notation
    [v.(i)].
    (The numbering starts from 0.
     Hence, the first element of [v] is [v.(0)].)

  - The conditional (or alternative construct) has syntax
       [if] ... [then] ... [else] ...

  - Convertions are explicit in Caml. The general naming convention for
    convertions is <type1>[_of_]<type2>, that converts a value of type type2
    into a value of type type1.
    Hence, use
       [int_of_string s]
    to obtain the integer represented by the string [s].

  - The general naming convention for printing functions is
    [print_]<type>.
    Hence, use
       [print_int]
    to print an integer value.

  - The predefined function
    [print_newline]
    outputs a newline then flushes the standard output (normally bound to the
    terminal).
*)
let args = Sys.argv in
  if Array.length args = 2 then
    let n = int_of_string Sys.argv.(1) in
    print_int (n * n);
    print_newline ()
  else
    print_string "Usage: square <number>\n"
;;
