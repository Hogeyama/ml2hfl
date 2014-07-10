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

(* $Id: types.mli,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

type type_simple and schéma_de_types;;

val type_int : type_simple;;
val type_bool : type_simple;;
val type_flèche : type_simple -> type_simple -> type_simple;;
val type_produit : type_simple -> type_simple -> type_simple;;
val type_liste : type_simple -> type_simple;;

val nouvelle_inconnue : unit -> type_simple;;
val unifie : type_simple -> type_simple -> unit;;
val généralisation : type_simple -> schéma_de_types;;
val spécialisation : schéma_de_types -> type_simple;;
val schéma_trivial : type_simple -> schéma_de_types;;
val début_de_définition : unit -> unit;;
val fin_de_définition : unit -> unit;;

exception Conflit of type_simple * type_simple;;
exception Circularité of type_simple * type_simple;;

val imprime_type : type_simple -> unit;;
val imprime_schéma : schéma_de_types -> unit;;
