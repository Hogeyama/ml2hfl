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

(* $Id: hash_table.ml,v 1.1 2011-08-08 19:09:58 weis Exp $ *)

type ('a, 'b) t = ('a, 'b) Hashtbl.t;;

let bindings t =
  Hashtbl.fold (fun k v l -> (k, v) :: l) t []
;;

let values t =
  Hashtbl.fold (fun _k v l -> v :: l) t []
;;

let of_bindings bindings =
  let l = List.length bindings in
  let t = Hashtbl.create l in
  List.iter (fun (k, v) -> Hashtbl.add t k v) bindings;
  t
;;
