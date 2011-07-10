
open Util
open CEGAR_syntax
open CEGAR_const

let print_var = Format.pp_print_string

let rec print_term fm = function
    Const c -> CEGAR_const.print fm c
  | Var x -> print_var fm x
  | App(t1,t2) -> Format.printf "(%a %a)" print_term t1 print_term t2

let rec print_fun_def fm (f,xs,t1,t2) =
  if t1 = Const True
  then Format.printf "%a -> %a@." (print_list print_var " " false) (f::xs) print_term t2
  else Format.printf "%a when %a -> %a@." (print_list print_var " " false) (f::xs) print_term t1 print_term t2

let rec print_prog fm (env,defs,s) =
  Format.fprintf fm "Main: %a@." print_var s;
  List.iter (print_fun_def fm) defs


