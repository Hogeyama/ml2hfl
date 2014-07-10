open Ast;;
open Ast_print;;
open Deep_ast;;
(* Camlpp generated file *)

open Lib_pp;;
open Format;;

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
(* $Id: deep_ast_print.ml,v 1.2 2011-08-08 19:31:17 weis Exp $ *)
open Ast;;

let rec print_unop = function
  | Uabs -> printf "Uabs" | Uand -> printf "Uand" | Uacos -> printf "Uacos"
  | Uacosh -> printf "Uacosh" | Uasin -> printf "Uasin"
  | Uasinh -> printf "Uasinh" | Uatan -> printf "Uatan"
  | Uatanh -> printf "Uatanh" | Uceil -> printf "Uceil"
  | Ucolon -> printf "Ucolon" | Ucos -> printf "Ucos"
  | Ucosh -> printf "Ucosh" | Uexp -> printf "Uexp" | Uexpm -> printf "Uexpm"
  | Ufloor -> printf "Ufloor" | Uint -> printf "Uint" | Uinv -> printf "Uinv"
  | Ulog -> printf "Ulog" | Ulog10 -> printf "Ulog10"
  | Ulogm -> printf "Ulogm" | Uor -> printf "Uor" | Umin -> printf "Umin"
  | Umax -> printf "Umax" | Uminus -> printf "Uminus" | Unot -> printf "Unot"
  | Ureshape -> printf "Ureshape" | Uround -> printf "Uround"
  | Usign -> printf "Usign" | Usin -> printf "Usin" | Usinh -> printf "Usinh"
  | Usizeprod -> printf "Usizeprod" | Usizecol -> printf "Usizecol"
  | Usizerow -> printf "Usizerow" | Usum -> printf "Usum"
  | Usumcol -> printf "Usumcol" | Usumrow -> printf "Usumrow"
  | Usvd -> printf "Usvd" | Utranspose -> printf "Utranspose"
  | Uctranspose -> printf "Uctranspose" | Usqrt -> printf "Usqrt"
  | Utan -> printf "Utan" | Utanh -> printf "Utanh" | Ueye -> printf "Ueye";;

let rec print_binop = function
  | Badd -> printf "Badd" | Bsub -> printf "Bsub" | Bmul -> printf "Bmul"
  | Bdiv -> printf "Bdiv" | Bexp -> printf "Bexp" | Bldiv -> printf "Bldiv"
  | Beq -> printf "Beq" | Blt -> printf "Blt" | Bgt -> printf "Bgt"
  | Ble -> printf "Ble" | Bge -> printf "Bge" | Bne -> printf "Bne"
  | Blor -> printf "Blor" | Bland -> printf "Bland" | Blne -> printf "Blne"
  | Bmin -> printf "Bmin" | Bmax -> printf "Bmax" | Batan2 -> printf "Batan2"
  | Bmulm -> printf "Bmulm" | Bdivm -> printf "Bdivm"
  | Bldivm -> printf "Bldivm" | Bexpm -> printf "Bexpm"
  | Bhconc -> printf "Bhconc" | Bvconc -> printf "Bvconc"
  | Bextract -> printf "Bextract"
  | Bextract_all_columns -> printf "Bextract_all_columns"
  | Bextract_all_rows -> printf "Bextract_all_rows"
  | Bremove -> printf "Bremove"
  | Bremove_all_columns -> printf "Bremove_all_columns"
  | Bremove_all_rows -> printf "Bremove_all_rows" | Brange -> printf "Brange"
  | Bassign -> printf "Bassign" | Bones -> printf "Bones"
  | Bzeros -> printf "Bzeros";;

let rec print_ternop = function
  | Textract -> printf "Textract" | Trange -> printf "Trange"
  | Tassign -> printf "Tassign"
  | Tassign_all_columns -> printf "Tassign_all_columns"
  | Tassign_all_rows -> printf "Tassign_all_rows"
  | Tmatrix -> printf "Tmatrix" | Tdotif -> printf "Tdotif";;

let rec print_sci_op = function  | Dotswitch -> printf "Dotswitch";;

let rec print_quaternop = function
  | Qassign -> printf "Qassign" | Qdotswitchcase -> printf "Qdotswitchcase";;

let rec print_label = (function s -> print_quoted_string s);;

let rec print_expression = function
  | D_colon -> printf "D_colon" | D_dollar -> printf "D_dollar"
  | D_number s ->
     printf "@[<1>(%s@ " "D_number"; print_quoted_string s; printf ")@]"
  | D_string s ->
     printf "@[<1>(%s@ " "D_string"; print_quoted_string s; printf ")@]"
  | D_matrix l_l_e ->
     printf "@[<1>(%s@ " "D_matrix";
     print_list (print_list (print_expression)) l_l_e; printf ")@]"
  | D_variable s ->
     printf "@[<1>(%s@ " "D_variable"; print_quoted_string s; printf ")@]"
  | D_parameter i ->
     printf "@[<1>(%s@ " "D_parameter"; print_quoted_int i; printf ")@]"
  | D_accu e ->
     printf "@[<1>(%s@ " "D_accu"; print_expression e; printf ")@]"
  | D_if (e, e0, e1) ->
     printf "@[<1>(%s@ " "D_if"; printf "@[<1>("; print_expression e;
     printf ",@ "; print_expression e0; printf ",@ "; print_expression e1;
     printf ")@]"; printf ")@]"
  | D_switch s ->
     printf "@[<1>(%s@ " "D_switch"; print_switch_desc s; printf ")@]"
  | D_unary (u, e) ->
     printf "@[<1>(%s@ " "D_unary"; printf "@[<1>("; print_unop u;
     printf ",@ "; print_expression e; printf ")@]"; printf ")@]"
  | D_binary (b, e, e0) ->
     printf "@[<1>(%s@ " "D_binary"; printf "@[<1>("; print_binop b;
     printf ",@ "; print_expression e; printf ",@ "; print_expression e0;
     printf ")@]"; printf ")@]"
  | D_ternary (t, e, e0, e1) ->
     printf "@[<1>(%s@ " "D_ternary"; printf "@[<1>("; print_ternop t;
     printf ",@ "; print_expression e; printf ",@ "; print_expression e0;
     printf ",@ "; print_expression e1; printf ")@]"; printf ")@]"
  | D_quaternary (q, e, e0, e1, e2) ->
     printf "@[<1>(%s@ " "D_quaternary"; printf "@[<1>("; print_quaternop q;
     printf ",@ "; print_expression e; printf ",@ "; print_expression e0;
     printf ",@ "; print_expression e1; printf ",@ "; print_expression e2;
     printf ")@]"; printf ")@]"
  | D_apply (e, l_e) ->
     printf "@[<1>(%s@ " "D_apply"; printf "@[<1>("; print_expression e;
     printf ",@ "; print_list (print_expression) l_e; printf ")@]";
     printf ")@]"
  | D_record_access (e, l) ->
     printf "@[<1>(%s@ " "D_record_access"; printf "@[<1>(";
     print_expression e; printf ",@ "; print_label l; printf ")@]";
     printf ")@]"
  | D_bracket e ->
     printf "@[<1>(%s@ " "D_bracket"; print_expression e; printf ")@]"
  | D_paren e ->
     printf "@[<1>(%s@ " "D_paren"; print_expression e; printf ")@]"

and print_switch_desc = function
  {dswitch_subject = e; dswitch_cases = c; dswitch_default = e0; } ->
    printf "@[<1>{"; printf "@[<1>dswitch_subject =@ "; print_expression e;
    printf ";@]@ "; printf "@[<1>dswitch_cases =@ "; print_clauses c;
    printf ";@]@ "; printf "@[<1>dswitch_default =@ "; print_expression e0;
    printf ";@]@ "; printf "@,}@]"

and print_clauses = (function l_c -> print_list (print_clause) l_c)

and print_clause =
  (function (l_p, e) ->
    printf "@[<1>("; print_list (print_pattern) l_p; printf ",@ ";
    print_expression e; printf ")@]")

and print_pattern = (function e -> print_expression e);;

(* End of Camlpp generated code *)
