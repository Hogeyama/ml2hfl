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

(* $Id: date.ml,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

let norm_chiffre c =
  if c >= 10 then string_of_int c else "0" ^ string_of_int c
;;

(* Should be a full regular expression matching. *)
let ( =~ ) s pat =
  let lp = String.length pat and ls = String.length s in
  lp <= ls && pat = String.sub s 0 lp
;;

let norm_date d =
  if d =~ "bientot" ||
     d =~ "bientôt" ||
     d =~ "interdit" then d else
  Scanf.sscanf d
   " %2u %_1[:/.-] %2u %_1[:/.-] %4u  %!"
   (fun j m a ->
     let a = if a > 2000 then a - 2000 else a in
     String.concat "/" (List.map norm_chiffre [j; m; a]))
;;
