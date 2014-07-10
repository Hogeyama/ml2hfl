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

#use "start.ml";;
#use "hello.ml";;
#use "hello_quit.ml";;
#use "addition.ml";;
#use "convertion_euro.ml";;
#use "convert_euro.ml";;
#use "convertion.ml";;
#use "convert.ml";;
#use "rgb.ml";;
#use "camleyes.ml";;
#use "taquin.ml";;
#use "tetris.ml";;

print_string "\n
To run type in one of:
        start ();;
        Camltk.closeTk();;
        hello ();;
        Camltk.closeTk();;
        hello_quit ();;
        addition ();;
        convertion_en_francs ();;
        convert_in_francs ();;
        convertion ();;
        convert ();;
        rgb ();;
        caml_eyes ();;
        taquin \"file name\" <number> <number>
        (* For instance *)
        taquin \"joconde.gif\" 3 5;;
        tetris ();;
";;

print_newline ();;

