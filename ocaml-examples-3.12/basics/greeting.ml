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

(* $Id: greeting.ml,v 1.6 2011-08-08 19:31:17 weis Exp $ *)

(** A simple program to welcome a user.

 We learn here that:

- A sequence of commands is denoted by the symbol ``;''.
   To evaluate [stm1] then [stm2], use
     [stm1; stm2]

- Output is buffered:
   to flush the standard output, use [flush stdout].

- To define identifier [name] to have the value of [e], use

     [let name = e in
     ...]

- Reading strings:
   to read a single line from standard input, use

   [input_line stdin].

- To catenate strings [s1] and [s2], use

  [s1 ^ s2]

- To terminate a program, use
  - [exit 0] (for normal termination)
  - [exit 2] (for erroneous or abnormal termination)
*)

print_string "What's your name ? ";
flush stdout;
let answer = input_line stdin in
print_string ("Hello " ^ answer);
print_string ", nice to meet you!\n";
exit 0
;;
