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
open Code;;

exception Erreur of string;;

val initialise : unit -> unit;;
val assemble : instruction -> unit;;
val poser_étiquette : string -> unit;;
val valeur_étiquette : string -> int;;
val extraire_code : unit -> instruction array;;
