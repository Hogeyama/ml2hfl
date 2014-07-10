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

(* $Id: path.ml,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

type path = int list
;;

let print ppf l =
  let rec loop ppf = function
    | [] -> ()
    | p :: ps -> Format.fprintf ppf "%i; %a" p loop ps in
  Format.fprintf ppf "[%a]" loop l
;;
