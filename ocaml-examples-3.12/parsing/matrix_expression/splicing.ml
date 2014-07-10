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

(* $Id: splicing.ml,v 1.1 2011-08-08 19:09:58 weis Exp $ *)

open Deep_ast;;
open Splice_ast;;

let un_splice exp =
  match exp with
  | S_splice exp -> exp
  | S_parameter _ -> exp
  | S_colon | S_dollar
  | S_paren _ | S_bracket _ | S_record_access (_, _) | S_apply (_, _)
  | S_unary (_, _) | S_binary (_, _, _)
  | S_ternary (_, _, _, _) | S_quaternary (_, _, _, _, _)
  | S_if (_, _, _) | S_switch _
  | S_variable _ | S_number _ | S_string _ | S_matrix _ -> assert false
;;

let is_spliced exp =
  match exp with
  | S_splice _ -> true
  | S_paren _ | S_bracket _ | S_record_access (_, _)
  | S_apply (_, _)
  | S_binary (_, _, _) | S_unary (_, _)
  | S_quaternary (_, _, _, _, _) | S_ternary (_, _, _, _)
  | S_if (_, _, _) | S_switch _
  | S_parameter _ | S_variable _ | S_number _ | S_string _ | S_matrix _
  | S_dollar | S_colon -> false
;;

let splice_extract e_f e_args =
  match e_args with
  | [] -> assert false
  | [ e_arg; ] ->
    S_binary (Bextract, e_f, e_arg)
  | [ e_arg1; e_arg2; ] ->
    S_ternary (Textract, e_f, e_arg1, e_arg2)
  | _ ->
    failwith "Unsupported function application"
;;

let splice_matrix e_elems =

  let splice_column col =
    let rec loop accu_in saccu accu  =
      match accu_in with
      | [] ->
        List.rev
          (if saccu <> [] then
             [ S_splice (S_bracket (S_matrix (List.rev saccu))) ] ::
             accu else
           accu)
      | col_elt :: accu_in ->
        let (saccu, accu) =
          if List.for_all is_spliced col_elt then
            (List.map un_splice col_elt) :: saccu, accu else
          if saccu <> [] then
            let accu =
              col_elt ::
                [ S_splice (S_bracket (S_matrix (List.rev saccu))) ] ::
                accu in
            [], accu else
          [], col_elt :: accu in
        loop accu_in saccu accu in
    loop col [] []

  and splice_row row =
    let rec loop accu_in saccu accu =
      match accu_in, saccu with
      | [], [] -> List.rev accu
      | [], saccu ->
        List.rev
          (S_splice (S_bracket (S_matrix [ List.rev saccu ])) :: accu)
      | row_elt :: accu_in, saccu when is_spliced row_elt ->
        loop accu_in (un_splice row_elt :: saccu) accu
      | row_elt :: accu_in, [] ->
        loop accu_in [] (row_elt :: accu)
      | row_elt :: accu_in, saccu ->
        loop accu_in []
          (row_elt ::
           S_splice (S_bracket (S_matrix [ List.rev saccu ])) ::
           accu) in
    loop row [] [] in

  splice_column
    (List.fold_right
      (fun y accu -> splice_row y :: accu)
      e_elems [])
;;

let rec splice exp =
  match exp with
  | D_dollar -> S_splice S_dollar
  | D_colon -> S_splice S_colon
  | D_number n -> S_splice (S_number n)
  | D_string s -> S_splice (S_string s)
  | D_matrix e_elems ->
    let e_elems = List.map (List.map splice) e_elems in
    if List.for_all (List.for_all is_spliced) e_elems
    then S_splice (S_matrix (List.map (List.map un_splice) e_elems))
    else S_matrix (splice_matrix e_elems)
  | D_variable v -> S_splice (S_variable v)
  | D_parameter i -> S_parameter i
  | D_accu e ->
    let e = splice e in
    if is_spliced e then S_splice (un_splice e) else S_splice e
  | D_if (cond, e_then, e_else) ->
    let cond, e_then, e_else =
      splice cond, splice e_then, splice e_else in
    if is_spliced cond && is_spliced e_then && is_spliced e_else
    then S_splice (S_if (un_splice cond, un_splice e_then, un_splice e_else))
    else S_if (cond, e_then, e_else)
  | D_switch {
      dswitch_subject = e;
      dswitch_cases = clauses;
      dswitch_default = d;
    } ->
    let splice_clauses cls =
      let splice_clause (pats, e) =
        (List.map splice pats, splice e) in
      List.map splice_clause cls in
    let un_splice_clauses cls =
      let un_splice_clause (pats, e) =
        (List.map un_splice pats, un_splice e) in
      List.map un_splice_clause cls in
    let is_spliced_clauses cls =
      let is_spliced_clause (pats, e) = List.for_all is_spliced (e :: pats) in
      List.for_all is_spliced_clause cls in
    let e = splice e in
    let d = splice d in
    let clauses = splice_clauses clauses in
    if is_spliced e && is_spliced_clauses clauses && is_spliced d then
      S_splice (S_switch {
        sswitch_subject = un_splice e;
        sswitch_cases = un_splice_clauses clauses;
        sswitch_default = un_splice d;
      }) else
      S_switch {
      sswitch_subject = e;
      sswitch_cases = clauses;
      sswitch_default = d;
      }
  | D_unary ((Usizeprod | Usizerow | Usizecol as op), e) ->
    let e = splice e in
    if is_spliced e
    then S_splice (S_unary (op, un_splice e))
    else S_splice (S_unary (op, un_splice e))
  | D_unary (
      ( Uabs
      | Uand
      | Uacos | Uacosh
      | Uasin | Uasinh
      | Uatan | Uatanh
      | Uceil
      | Ucolon
      | Ucos | Ucosh
      | Uexp
      | Uexpm
      | Ufloor
      | Uint
      | Uinv
      | Ulog | Ulog10 | Ulogm
      | Umin | Umax
      | Uminus
      | Unot
      | Uor
      | Ureshape
      | Uround
      | Usign
      | Usin | Usinh
      | Usum
      | Usumcol
      | Usumrow
      | Usvd
      | Utranspose
      | Uctranspose
      | Usqrt
      | Utan | Utanh
      | Ueye as op), x) ->
    let x = splice x in
    if is_spliced x
    then S_splice (S_unary (op, un_splice x))
    else S_unary (op, x)
  | D_binary (op, x, y) ->
    let x = splice x and y = splice y in
    if is_spliced x && is_spliced y
    then S_splice (S_binary (op, un_splice x, un_splice y))
    else S_binary (op, x, y)
  | D_ternary (op, x, y, z) ->
    let x = splice x and y = splice y and z = splice z in
    if is_spliced x && is_spliced y && is_spliced z
    then S_splice (S_ternary (op, un_splice x, un_splice y, un_splice z))
    else S_ternary (op, x, y, z)
  | D_quaternary (op, x, y, z, t) ->
    let x = splice x and y = splice y
    and z = splice z and t = splice t in
    if is_spliced x && is_spliced y && is_spliced z && is_spliced t
    then S_splice
           (S_quaternary
              (op, un_splice x, un_splice y, un_splice z, un_splice t))
    else S_quaternary (op, x, y, z, t)
  | D_apply (e_f, e_args) ->
    let e_f = splice e_f
    and e_args = List.map splice e_args in
    if is_spliced e_f && List.for_all is_spliced e_args
      then S_splice (S_apply (un_splice e_f, List.map un_splice e_args))
    else splice_extract e_f e_args
  | D_record_access (e, label) ->
    let e = splice e in
    if is_spliced e
    then S_splice (S_record_access (un_splice e, label))
    else S_record_access (e, label)
  | D_bracket e ->
    let e = splice e in
    if is_spliced e
    then S_splice (S_bracket (un_splice e))
    else S_bracket e
  | D_paren e ->
    let e = splice e in
    if is_spliced e
    then S_splice (S_paren (un_splice e))
    else S_paren e
;;

let rec fully_un_splice exp =
  match exp with
  | S_splice exp -> exp
  | S_colon | S_dollar -> exp
  | S_paren e -> S_paren (fully_un_splice e)
  | S_bracket e -> S_bracket (fully_un_splice e)
  | S_record_access (e1, e2) -> S_record_access (fully_un_splice e1, e2)
  | S_apply (e1, e2) ->
    S_apply (fully_un_splice e1, List.map fully_un_splice e2)
  | S_unary (e1, e2) -> S_unary (e1, fully_un_splice e2)
  | S_binary (e1, e2, e3) ->
    S_binary (e1, fully_un_splice e2, fully_un_splice e3)
  | S_ternary (e1, e2, e3, e4) ->
    S_ternary (e1, fully_un_splice e2, fully_un_splice e3, fully_un_splice e4)
  | S_quaternary (e1, e2, e3, e4, e5) ->
    S_quaternary (
      e1, fully_un_splice e2, fully_un_splice e3,
      fully_un_splice e4, fully_un_splice e5)
  | S_if (e1, e2, e3) ->
    S_if (fully_un_splice e1, fully_un_splice e2, fully_un_splice e3)
  | S_switch {
      sswitch_subject = s;
      sswitch_cases = cls;
      sswitch_default = d;
    } ->
    S_switch {
      sswitch_subject = fully_un_splice s;
      sswitch_cases = fully_un_splice_clauses cls;
      sswitch_default = fully_un_splice d;
    }
  | S_parameter _ -> exp
  | S_variable _ -> exp
  | S_number _ -> exp
  | S_string _ -> exp
  | S_matrix exps -> S_matrix (List.map (List.map fully_un_splice) exps)

and fully_un_splice_clauses cls =
  let fully_un_splice_clause (pats, e) =
    (List.map fully_un_splice pats, fully_un_splice e) in
  List.map fully_un_splice_clause cls
;;
