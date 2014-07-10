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

(* $Id: crayon.mli,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

val vide_écran : unit -> unit;;
val fixe_crayon : bool -> unit;;
val tourne : float -> unit;;
val avance : float -> unit;;
