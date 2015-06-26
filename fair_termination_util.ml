open Util
open Syntax
open Term_util
open Type
open Fair_termination_type


let print_fairness fm fairness =
  let pr fm (a,b) = Format.printf "(%s, %s)" a b in
  Format.fprintf fm "@[<hov 1>{%a}@]" (print_list pr ",@ ") fairness

let print_rank_fun xs fm {coeffs;const} =
  let xs' = List.map Option.some xs @ [None] in
  let pr fm (c,x) =
    match x with
    | Some x' -> Format.fprintf fm "%d*%a" c Id.print x'
    | None -> Format.fprintf fm "%d" c
  in
  Format.fprintf fm "%a" (print_list pr " + ") @@ List.combine (coeffs@[const]) xs'

let event_fun = "event"
let is_event_fun_var x = Id.name x = event_fun
