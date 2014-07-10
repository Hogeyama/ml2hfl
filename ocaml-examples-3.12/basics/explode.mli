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

(* $Id: explode.mli,v 1.5 2011-08-08 19:31:17 weis Exp $ *)

(** This is the interface of the [Explode] module.

This file lists the values offered by the module with their respective types.

The implementation file must define the same set of values with the
corresponding types. Otherwise, the compiler will complain and abort the
compilation of the module.

Fancy [(** blabla *)] comments are documentation annotations written in the
syntax of the [ocamldoc] tool.

Ocamldoc automatically extracts the documentation from annotated Caml source
files and outputs files in various formats (including TeX or ASCII texts)
including a set of hyper text files.

*)

val explode : string -> char list
(** [explode s] returns the list of characters of string [s]. *)
;;

val implode : char list -> string
(** [implode chars] returns the string that has [chars] as list of characters.
    [implode] is the converse function of [implode]:
    [implode (explode s)] is equal to [s], and [explode (implode chars)]
    is equal to [chars]. *)
;;
