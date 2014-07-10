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

(* $Id: loadall.ml,v 1.4 2011-08-08 19:31:17 weis Exp $ *)

#use "fib.ml";;
#use "sieve.ml";;
#use "queens.ml";;
#use "wc.ml";;
#use "strstr.ml";;
#use "soli.ml";;

print_string "To run:
        fib <some number>;;
        sieve <upper bound>;;
        queens <chess size>;;
        count \"file name\";;
        strstr \"pattern\" \"string\";;
        solve_solitaire ();;
";
print_newline ()
;;

