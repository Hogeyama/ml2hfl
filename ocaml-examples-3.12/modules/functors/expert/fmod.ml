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

module F : Ftype.FTYPE = functor (Iarg : Itype.ITYPE) ->
  struct
    type len = Iarg.len;;
    type len2 = len * len;;
    let read () =
      let l1 = Iarg.read () in
      let l2 = Iarg.read () in
      (l1, l2)
    ;;
    let print (l1, l2) =
      print_char '(';
      Iarg.print l1;
      print_string ", ";
      Iarg.print l2;
      print_char ')';
      print_newline ()
    ;;
  end
;;
