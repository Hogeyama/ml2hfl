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

(* $Id: fmod.ml,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

module F : Ftype.FTYPE = functor (I : Itype.ITYPE) ->
  struct
    type len2 = I.len * I.len;;
    let read () =
      let l1 = I.read () in
      let l2 = I.read () in
      (l1, l2)
    ;;
    let print (l1, l2) =
      print_char '(';
      I.print l1;
      print_string ", ";
      I.print l2;
      print_char ')';
      print_newline ()
    ;;
  end
;;
