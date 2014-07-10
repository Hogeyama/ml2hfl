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
open Machine;;

open Deep_ast;;

let sum _x = 0.0;;
let sumcol _x = 0.0;;
let sumrow _x = 0.0;;
let sizeprod _x = 0.0;;
let sizerow _x = 0.0;;
let sizecol _x = 0.0;;
let colonwise x = x;;
let transpose x = x;;
let ctranspose x = x;;
let tassign _accu _x y = y;;
let tassign_all_columns _accu _x y = y;;
let tassign_all_rows _accu _x y = y;;
let qassign _accu _x _y z = z;;
let qdotswitchcase _cei _ei d _ec = d;;
let tdotif _accu _x y = y;;
let tmatrix _accu _x y = y;;

let ueye _x = 1.0;;
let bones _x _y = 1.0;;
let bzeros _x _y = 0.0;;

let apply_unary unop x =
  match unop with
  | Deep_ast.Uabs -> abs_float x
  | Uand -> 1.
  | Uacos -> acos x
  | Uacosh -> log (x +. sqrt (x *. x -. 1.))
  | Uasin -> asin x
  | Uasinh -> log (x +. sqrt (x *. x +. 1.))
  | Uatan -> atan x
  | Uatanh -> log ((1. +. x) /. (1. -. x)) /. 2.0
  | Uceil -> ceil x
  | Ucolon -> colonwise x
  | Ucos -> cos x
  | Ucosh -> cosh x
  | Uexp -> exp x
  | Uexpm -> exp x
  | Ufloor -> floor x
  | Uint -> if x > 0.0 then floor x else ceil x
  | Uinv -> 1. /. x
  | Ulog -> log x
  | Ulogm -> log x
  | Ulog10 -> log10 x
  | Uminus -> -. x
  | Umin -> -. x
  | Umax -> -. x
  | Unot -> if x = 0.0 then 1.0 else 0.0
  | Uor -> 1.
  | Ureshape -> x
  | Uround -> floor (x +. 0.5)
  | Usign -> if x > 0.0 then 1.0 else if x < 0.0 then -1.0 else 0.0
  | Usin -> sin x
  | Usinh -> sinh x
  | Usizeprod -> sizeprod x
  | Usizerow -> sizerow x
  | Usizecol -> sizecol x
  | Usum -> sum x
  | Usumrow -> sumrow x
  | Usumcol -> sumcol x
  | Usqrt -> sqrt x
  | Usvd -> 1.
  | Utan -> tan x
  | Utanh -> tanh x
  | Utranspose -> transpose x
  | Uctranspose -> ctranspose x
  | Ueye -> ueye x
;;

let apply_binary binop x y =
  match binop with
  | Deep_ast.Badd -> x +. y
  | Bsub -> x -. y
  | Bmul -> x *. y
  | Bdiv -> x /. y
  | Bexp -> x ** y
  | Bldiv -> assert false
  | Beq -> if x = y then 1.0 else 0.0
  | Blt -> if x < y then 1.0 else 0.0
  | Bgt -> if x > y then 1.0 else 0.0
  | Ble -> if x <= y then 1.0 else 0.0
  | Bge -> if x >= y then 1.0 else 0.0
  | Bne -> if x <> y then 1.0 else 0.0
  | Blor -> if x <> 0.0 || y <> 0.0 then 1.0 else 0.0
  | Bland -> if x *. y <> 0.0 then 1.0 else 0.0
  | Blne -> if x <> y then 1.0 else 0.0
  | Bmin -> if x < y then x else y
  | Bmax -> if x > y then x else y
  | Batan2 -> atan2 x y
  | Bones -> bones x y
  | Bzeros -> bzeros x y
  | Bmulm | Bdivm | Bldivm | Bexpm
  | Bhconc | Bvconc -> assert false
  | Bextract | Bextract_all_columns | Bextract_all_rows
  | Bremove | Bremove_all_columns | Bremove_all_rows
  | Bassign
  | Brange -> assert false
;;

let apply_ternary ternop x y z =
  match ternop with
  | Deep_ast.Textract -> z +. x +. y
  | Deep_ast.Trange -> z +. x +. y
  | Deep_ast.Tassign -> tassign x y z
  | Deep_ast.Tassign_all_columns -> tassign_all_columns x y z
  | Deep_ast.Tassign_all_rows -> tassign_all_rows x y z
  | Deep_ast.Tmatrix -> tmatrix x y z
  | Deep_ast.Tdotif -> tdotif x y z
;;

let apply_quaternary quaternop x y z t =
  match quaternop with
  | Deep_ast.Qassign -> qassign x y z t
  | Qdotswitchcase -> qdotswitchcase x y z t
;;

(* Variables are either constants or statically evaluated expressions stored
  in the vector globals. *)

(*
   Params are floating point values taht are parameters of blocks.
   They are evaluated by scilab.
   The array params is thus an array of floating point values.
 *)

let exec params globals code =

  let max_params = Array.length params
  and max_globals = Array.length globals in

  let get_param i =
    if i < 0 || i >= max_params then
      failwith (Printf.sprintf "erroneous variable reference %i" i) else
    params.(i) in

  let get_global i =
    if i < 0 || i >= max_globals then
      failwith (Printf.sprintf "segmentation fault %i" i) else
    globals.(i) in

  let rec loop accu stack = function
  | [] -> accu
  | ins :: code ->
    match ins with
    | Accu -> loop accu stack code
    | Quote f -> loop f stack code
    | Push -> loop accu (accu :: stack) code
    | Pop ->
      begin match stack with
      | [] -> failwith "stack underflow"
      | x :: stack -> loop x stack code
      end
    | Get_param i -> loop (get_param i) stack code
    | Get_global i -> loop (get_global i) stack code
    | Unary op -> loop (apply_unary op accu) stack code
    | Binary op ->
      begin match stack with
      | [] -> failwith "stack underflow"
      | x :: stack ->
        loop (apply_binary op x accu) stack code
      end
    | Ternary op ->
      begin match stack with
      | [] | [ _ ] -> failwith "stack underflow"
      | y :: x :: stack ->
        loop (apply_ternary op x y accu) stack code
      end
    | Quaternary op ->
      begin match stack with
      | [] | [ _ ] | [ _; _ ] -> failwith "stack underflow"
      | z :: y :: x :: stack ->
        loop (apply_quaternary op x y z accu) stack code
      end in

  match code with
  | [] -> failwith "No code to define a value"
  | code -> loop 0.0 [] code
;;

(*
let essai () =
  let r = exec [| 1.0; 2.0; |] [||]
  [ Get_var 0; Push; Get_var 1; Binary Add; ] in
  Printf.eprintf "if u1 = 1 and u2 = 2 then u1 + u2 = %g\n" r
;;

essai ()
;;

let essai () =
  let r = exec [| 1.0; 2.0; |] [||]
  [ Get_var 0; Push; Get_var 1; Binary Add; ] in
  Printf.eprintf "if u1 = 1 and u2 = 2 then u1 + u2 = %g\n" r
;;

let essai2 () =
  let r = exec [| 1.0; 2.0; |] [||]
  [ Quote 1.0; Get_var 0; Push; Binary Add; ] in
  Printf.eprintf "if u1 = 1 then 1 + u1 = %g\n" r
;;

essai2 ()
;;

let essai3 () =
  let params = [| 1.0; 2.0; |]
  and globals = [| 3.0; 1.0; 3.14; |] in
  let r = exec params globals
  [ Get_global 1; Push; Get_var 0; Binary Add; ] in
  let a = globals.(1) in
  let u1 = params.(0) in
  Printf.eprintf
    "if a = %g and u1 = %g then a + u1 = %g\n"
    a u1 r
;;

essai3 ()
;;
*)
