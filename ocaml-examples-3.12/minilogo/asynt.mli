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

(* $Id: asynt.mli,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

val analyse_phrase : Alex.lexème Stream.t -> Langage.phrase_logo;;
val analyse_programme : Alex.lexème Stream.t -> Langage.programme_logo;;
