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

(* $Id: hello.ml,v 1.5 2011-08-08 19:31:17 weis Exp $ *)

(** [(\* This is a remark in Caml. *\)] *)

(** This program just prints hello world! on the terminal. *)

(** We learn here:

 - how to print a string on standard output:
   print_string "a string"

 - that inside strings, the newline character is denoted by \n.

*)
print_string "Hello world!\n"
;;
