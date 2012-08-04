open Utilities

module CS = CEGAR_syntax

type base =
    Unit
  | Bool
  | Int

type t =
    Base of base * (CS.t -> CS.t list)
  | Fun of t * (CS.t -> t)
  | Pair of t * (CS.t -> t)
  | Inter of t list
  | Union of t list

let print_base fm = function
    Unit -> Format.pp_print_string fm "unit"
  | Bool -> Format.pp_print_string fm "bool"
  | Int -> Format.pp_print_string fm "int"

let rec occur x = function
    Base(_,ps) -> List.exists (fun p -> List.mem x (CS.get_fv p)) (ps (CS.Var (CS.new_id "")))
  | Fun(typ1,typ2)
  | Pair(typ1,typ2) -> occur x typ1 || occur x (typ2 (CS.Var (CS.new_id "")))
  | Inter typs
  | Union typs -> List.exists (occur x) typs

let rec print fm = function
    Base(base,ps) ->
      Format.printf "{v:%a | %a}"
        print_base base (print_list CEGAR_print.term ";" false) (ps (CS.Var (CS.new_id "")))
  | Fun(typ1, typ2) ->
      let x = CS.new_id "" in
      let typ2' = typ2 (CS.Var x) in
        if occur x typ2'
        then Format.fprintf fm "(@[%a:%a@ ->@ %a@])" CEGAR_print.var x print typ1 print typ2'
        else Format.fprintf fm "(@[%a@ ->@ %a@])" print typ1 print typ2'
  | Pair(typ1, typ2) ->
      let x = CS.new_id "" in
      let typ2' = typ2 (CS.Var x) in
        if occur x typ2'
        then Format.fprintf fm "(@[%a:%a@ *@ %a@])" CEGAR_print.var x print typ1 print typ2'
        else Format.fprintf fm "(@[%a@ *@ %a@])" print typ1 print typ2'
  | Inter typs -> print_list print " /\\@ " false fm typs
  | Union typs -> print_list print " \\/@ " false fm typs
