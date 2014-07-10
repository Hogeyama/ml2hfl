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

(* $Id: realloc.mli,v 1.4 2011-08-08 19:31:17 weis Exp $ *)

(** Module Realloc: reallocation of vectors. *)

(** The file realloc.mli contains the interface of module [Realloc]:
   it defines the signature of functions defined in the module. *)

val realloc : 'a array -> int -> 'a array
;;
 (** [realloc v n] returns an array of length at least [n] that
     contains the elements of array [v] as its initial segment. *)
