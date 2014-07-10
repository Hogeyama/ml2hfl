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

(* $Id: realloc.ml,v 1.5 2011-08-08 19:31:17 weis Exp $ *)

(** Module [Realloc]: reallocation of vectors. *)

(** File [realloc.ml] contains the implementation of module [Realloc]. *)
let realloc v n =
  let l = Array.length v in
  if n <= l then v else
  if l = 0 then invalid_arg "cannot realloc empty vectors" else
  let res = Array.make n v.(0) in
  Array.blit v 1 res 1 (l - 1);
  res
;;
