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

(* $Id: hash_table_print.ml,v 1.1 2011-08-08 19:09:58 weis Exp $ *)

open Format;;

let print pa pb ppf t =

  let print_binding ppf (k, v) =
    fprintf ppf "@ @[<2>%a =>@ %a@]"
      pa k pb v in

  fprintf ppf "@[<hv 3>{| @[<hv 0>";

  let rec pl ppf = function
    | [] -> fprintf ppf "@]@;<0 -3>|}@]"
    | x :: xs -> fprintf ppf "%a ;%a" print_binding x pl xs in

  pl ppf (Hash_table.bindings t)
;;

let print_t pa pb = print (fun _ -> pa) (fun _ -> pb) std_formatter
;;
