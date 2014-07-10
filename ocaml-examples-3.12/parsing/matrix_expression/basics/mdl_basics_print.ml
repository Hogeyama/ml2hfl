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
open Mdl_basics;;
open Hash_table;;
(* Camlpp generated file *)

open Lib_pp;;
open Format;;

(* $Id: mdl_basics_print.ml,v 1.2 2011-08-08 19:17:33 weis Exp $ *)
open Ident;;

(* In fact natural should be private, to ensure
  that a natural is indeed positive. *)
let rec print_natural = (function i -> print_quoted_int i);;

let rec print_file_name = (function s -> print_quoted_string s)

and print_dir_name = (function s -> print_quoted_string s)

and print_explicit_file_name = (function s -> print_quoted_string s)

and print_suffix = (function s -> print_quoted_string s);;

let rec print_ident_binding pr_a = function
  {bound_ident = i; bound_value = a; } ->
    printf "@[<1>{"; printf "@[<1>bound_ident =@ "; print_ident i;
    printf ";@]@ "; printf "@[<1>bound_value =@ "; pr_a a; printf ";@]@ ";
    printf "@,}@]"

and print_ident_bindings pr_a =
  (function l_i_a -> print_list (print_ident_binding (pr_a)) l_i_a)

and print_ident_binding_table pr_a =
  (function h_t_i_i_a ->
    Hash_table_print.print_t (print_ident) (print_ident_binding (pr_a)) h_t_i_i_a);;

let rec print_font = function
  {font_name = f; font_angle = s; font_size = n; font_weight = s0; } ->
    printf "@[<1>{"; printf "@[<1>font_name =@ "; print_font_name f;
    printf ";@]@ "; printf "@[<1>font_angle =@ "; print_quoted_string s;
    printf ";@]@ "; printf "@[<1>font_size =@ "; print_natural n;
    printf ";@]@ "; printf "@[<1>font_weight =@ "; print_quoted_string s0;
    printf ";@]@ "; printf "@,}@]"

and print_font_name = (function s -> print_quoted_string s);;

(* End of Camlpp generated code *)
