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

exception Erreur of string * int;;

val lire_mémoire : int -> int;;
val écrire_mémoire : int -> int -> unit;;
val lire_registre : int -> int;;
val écrire_registre : int -> int -> unit;;
val tableau_des_appels_système: (int -> int) array;;

val exécute: instruction array -> int -> unit;;
