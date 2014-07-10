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

(* $Id: ftype.mli,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

module type FTYPE = functor (I : Itype.ITYPE) ->
  sig
    type len2 = I.len * I.len;;
    val read : unit -> len2;;
    val print : len2 -> unit;;
  end
;;
