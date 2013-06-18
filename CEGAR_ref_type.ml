open Util

module CS = CEGAR_syntax

type base =
    Unit
  | Bool
  | Int

type t =
    Base of base * CS.var * CS.t
  | Fun of CS.var * t * t
  | Inter of t list

let print_base fm = function
    Unit -> Format.pp_print_string fm "unit"
  | Bool -> Format.pp_print_string fm "bool"
  | Int -> Format.pp_print_string fm "int"

let rec occur x = function
    Base(_,_,p) -> List.mem x (CS.get_fv p)
  | Fun(_, typ1,typ2) -> occur x typ1 || occur x typ2
  | Inter typs -> List.exists (occur x) typs

let rec print fm = function
    Base(base,x,CS.Const CS.True) ->
      Format.printf "%a" print_base base
  | Base(base,x,p) ->
      Format.printf "{%a:%a | %a}" CEGAR_print.var x print_base base CEGAR_print.term p
  | Fun(x, typ1, typ2) ->
      if occur x typ2
      then Format.fprintf fm "(@[%a:%a@ ->@ %a@])" CEGAR_print.var x print typ1 print typ2
      else Format.fprintf fm "(@[%a@ ->@ %a@])" print typ1 print typ2
  | Inter [] -> Format.fprintf fm "Top"
  | Inter [typ] -> print fm typ
  | Inter typs -> Format.fprintf fm "(@[%a@])" (print_list print " /\\@ ") typs


let rec decomp_fun n typ =
  match typ with
    Base _
  | Inter _ -> assert (n=0); [], typ
  | Fun(x,typ1,typ2) ->
      if n <= 0
      then [], typ
      else
        let typs,typ' = decomp_fun (n-1) typ2 in
          (x,typ1)::typs, typ'


let rec arg_num = function
    Base _ -> 0
  | Inter [] -> assert false
  | Inter (typ::_) -> arg_num typ
  | Fun(_,_,typ2) -> 1 + arg_num typ2
