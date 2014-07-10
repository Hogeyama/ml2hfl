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

(* $Id: solve0.ml,v 1.2 2011-08-08 19:31:17 weis Exp $ *)

let rec remove_from_list d = function
  | [] -> []
  | d' :: l ->
    if d = d' then l else d' :: remove_from_list d l
;;

let remove_digit d board i j =
  let l = board.(i).(j) in
  let rl = remove_from_list d l in
  if rl = [] then raise Types.Backtrack else
  board.(i).(j) <- rl
;;

let suppress_digit_in_square d board i0 j0 =
(*prerr_endline (Printf.sprintf "suppress_digit: %i %i" i0 j0);*)
  for i = i0 to i0 + 2 do
    for j = j0 to j0 + 2 do
      let c = board.(i).(j) in
      match c with
      | [ _ ] -> ()
      | l -> remove_digit d board i j
    done
  done
;;

let sature_square board i0 j0 =
  Format.printf "sature_square: %i %i@." i0 j0;
  for i = i0 to i0 + 2 do
    for j = j0 to j0 + 2 do
      let c = board.(i).(j) in
      match c with
      | [d] ->
        suppress_digit_in_square d board i0 j0
      | _ -> ()
    done
  done
;;

let sature_squares board =
(*  Format.printf "sature_squares:@." i0 j0;*)
  for i = 0 to 2 do 
    for j = 0 to 2 do
      sature_square board (3 * i) (3 * j)
    done
  done
;;

(****************
let suppress_digit_in_square d (board, i0, j0) =
(*prerr_endline (Printf.sprintf "suppress_digit: %i %i" i0 j0);*)
  let f i j = function
    | [ _ ] -> ()
    | l -> remove_digit d board i0 j0 in
  Utils.iter_on_square f (board, i0, j0)
;;

let sature_square (board, i0, j0) =
  Format.printf "sature_square: %i %i@." i0 j0;
  let f i j = function
    | [d] -> suppress_digit_in_square d (board, i0, j0)
    | _ -> () in
  Utils.iter_on_square f (board, i0, j0)
;;

let sature_squares board = Utils.iter_on_squares sature_square
;;
******************)

let suppress_on_line board i0 d =
  for j = 0 to 8 do
    match board.(i0).(j) with
    | [_] -> ()
    | _ -> remove_digit d board i0 j
  done
;;

let suppress_on_row board j0 d =
  for i = 0 to 8 do
    match board.(i).(j0) with
    | [d] -> ()
    | _ -> remove_digit d board i j0
  done
;;

let rec sature_board board =
  for i = 0 to 8 do
    for j = 0 to 8 do
(*prerr_endline (Printf.sprintf "sature_board %i %i" i j);*)
      match board.(i).(j) with
      | [d] ->
(*prerr_endline (Printf.sprintf "suppress_on_line %i" i);*)
        suppress_on_line board i d;
(*prerr_endline (Printf.sprintf "suppress_on_row %i" j);*)
        suppress_on_row board j d
      | _ -> ()
    done;
  done;
  sature_squares board;
;;

let deduce board with_lists =
  let rec except (i0, j0) = function
    | [] -> []
    | (l, i, j as list) :: lists ->
      if i = i0 && j = j0 then lists else
      list :: except (i0, j0) lists in
(**
  let not_in_other_lists i j d =
    let digit_is_reducible =
      List.for_all
        (fun (l, _, _) -> not (List.mem d l))
        (except (i, j) with_lists) in
    if digit_is_reducible then begin board.(i).(j) <- [d]; true end
    else false in
*)
  let digit_not_in_other_lists i j d =
    List.for_all
      (fun (l, _, _) -> not (List.mem d l))
      (except (i, j) with_lists) &&
    (board.(i).(j) <- [d];
     true) in

  (** Find if we can reduce a list of possible digits at place i, j. *)
  let reducible (l, i, j) =
    List.exists (fun d -> digit_not_in_other_lists i j d) l in

  List.exists reducible with_lists
;;

let deduce_square board i0 j0 =
  let with_lists =
    let rec loop accu i j =
      if j > j0 + 2 then loop accu (i + 1) j0 else
      if i > i0 + 2 then accu else
      match board.(i).(j) with
      | [d] -> loop accu i (j + 1)
      | l -> loop ((l, i, j) :: accu) i (j + 1) in
    loop [] i0 j0 in

  deduce board with_lists
;;
(**********
  let rec loop i j =
    if j > j0 + 2 then loop (i + 1) j0 else
    if i > i0 + 2 then false else
    match board.(i).(j) with
    | [d] -> loop i (j + 1)
    | l -> deduce board l with_lists || loop i (j + 1) in
  loop i0 j0
;;
********)

let deduce_row board i0 =
  let with_lists =
    let rec loop accu j =
      if j > 8 then accu else
      match board.(i0).(j) with
      | [d] -> loop accu (j + 1)
      | l -> loop ((l, i0, j) :: accu) (j + 1) in
    loop [] 0 in

  deduce board with_lists
;;

let deduce_column board j0 =
  let with_lists =
    let rec loop accu i =
      if i > 8 then accu else
      match board.(i).(j0) with
      | [d] -> loop accu (i + 1)
      | l -> loop ((l, i, j0) :: accu) (i + 1) in
    loop [] 0 in

  deduce board with_lists
;;

let squares = [
  (0, 0); (0, 3); (0, 6);
  (3, 0); (3, 3); (3, 6);
  (6, 0); (6, 3); (6, 6);
];;

let rows = [
  0; 1; 2; 3; 4; 5; 6; 7; 8;
];;

let columns = rows
;;

let deduce_squares board =
  List.exists (fun (i, j) -> deduce_square board i j) squares
;;

let deduce_rows board =
  List.exists (fun i -> deduce_row board i) rows
;;

let deduce_columns board =
  List.exists (fun j -> deduce_column board j) columns
;;

let rec deduce_board board =
  sature_board board;
  (* For each square *)
  let found_something = deduce_squares board in
  if found_something then deduce_board board else
  (* For each row *)
  let found_something = deduce_rows board in
  if found_something then deduce_board board else
  (* For each column *)
  let found_something = deduce_columns board in
  if found_something then deduce_board board
;;
