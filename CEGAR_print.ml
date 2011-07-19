
open Util
open CEGAR_syntax
open CEGAR_type
open CEGAR_const

let rec print_var = Format.pp_print_string

and print_var_typ env fm x = Format.fprintf fm "(%a:%a)" print_var x print_typ (List.assoc x env)

and print_typ_base fm = function
    TUnit -> Format.fprintf fm "unit"
  | TBool -> Format.fprintf fm "bool"
  | TInt -> Format.fprintf fm "int"
  | TBottom -> Format.fprintf fm "bot"
  | TTuple n -> Format.fprintf fm "tuple"

and print_typ fm = function
    TBase(b,ps) ->
      let x = new_id "x" in
      let preds = ps (Var x) in
        if List.mem x (rev_flatten_map get_fv preds)
        then Format.fprintf fm "%a:%a[%a]" print_var x print_typ_base b (print_list print_term ";" false) preds
        else
          if preds = []
          then Format.fprintf fm "%a" print_typ_base b
          else Format.fprintf fm "%a[%a]" print_typ_base b (print_list print_term ";" false) preds
  | TFun typ ->
      let x = new_id "x" in
      let typ1,typ2 = typ (Var x) in
        if occur_arg_pred x typ2
        then Format.fprintf fm "(%a:%a -> %a)" print_var x print_typ typ1 print_typ typ2
        else Format.fprintf fm "(%a -> %a)" print_typ typ1 print_typ typ2
  | TApp _ as typ ->
      let typ,typs = decomp_tapp typ in
        Format.fprintf fm "(%a)" (print_list print_typ " " false) (typ::typs)

and print_env fm env =
  List.iter (fun (f,typ) -> Format.fprintf fm "%a : %a@." print_var f print_typ typ) env

and print_const fm = function
    Fail -> Format.fprintf fm "fail"
  | Event s -> Format.fprintf fm "event(%s)" s
  | Label n -> Format.fprintf fm "label(%d)" n
  | Unit -> Format.fprintf fm "()"
  | True -> Format.fprintf fm "true"
  | False -> Format.fprintf fm "false"
  | RandBool -> Format.fprintf fm "rand_bool"
  | RandInt -> Format.fprintf fm "rand_int"
  | And -> Format.fprintf fm "&&"
  | Or -> Format.fprintf fm "||"
  | Not -> Format.fprintf fm "not"
  | Lt -> Format.fprintf fm "<"
  | Gt -> Format.fprintf fm ">"
  | Leq -> Format.fprintf fm "<="
  | Geq -> Format.fprintf fm ">="
  | Eq -> Format.fprintf fm "="
  | Int n -> Format.fprintf fm "%d" n
  | Add -> Format.fprintf fm "+"
  | Sub -> Format.fprintf fm "-"
  | Mul -> Format.fprintf fm "*"
  | Tuple n -> Format.fprintf fm "(%d)" n
  | Proj(_,i) -> Format.fprintf fm "#%d" i
  | If -> Format.fprintf fm "if"
  | Branch -> Format.fprintf fm "br"

and print_term fm = function
    Const c -> print_const fm c
  | Var x -> print_var fm x
  | App _ as t ->
      let t,ts = decomp_app t in
        Format.fprintf fm "(%a)" (print_list print_term " " false) (t::ts)
  | Let(x,t1,t2) ->
      Format.fprintf fm "(let %a = %a in %a)" print_var x print_term t1 print_term t2
  | Fun(x,t) ->
      Format.fprintf fm "(fun %a -> %a)" print_var x print_term t

and print_fun_def fm (f,xs,t1,t2) =
  if t1 = Const True
  then Format.fprintf fm "%a -> %a@." (print_list print_var " " false) (f::xs) print_term t2
  else Format.fprintf fm "%a when %a -> %a@." (print_list print_var " " false) (f::xs) print_term t1 print_term t2

and print_prog fm (env,defs,s) =
  Format.fprintf fm "Main: %a@." print_var s;
  List.iter (print_fun_def fm) defs

and print_prog_typ fm (env,defs,s) =
  Format.fprintf fm "Main: %a@." print_var s;
  List.iter (print_fun_def fm) defs;
  Format.fprintf fm "Types:\n%a@." print_env env;

and print_const_ML fm = function
    Fail -> Format.fprintf fm "assert false"
  | Event s -> Format.fprintf fm "event(%s)" s
  | Label n -> Format.fprintf fm "label(%d)" n
  | Unit -> Format.fprintf fm "()"
  | True -> Format.fprintf fm "true"
  | False -> Format.fprintf fm "false"
  | RandBool -> Format.fprintf fm "rand_bool"
  | And -> Format.fprintf fm "(&&)"
  | Or -> Format.fprintf fm "(||)"
  | Not -> Format.fprintf fm "(not)"
  | Lt -> Format.fprintf fm "(<)"
  | Gt -> Format.fprintf fm "(>)"
  | Leq -> Format.fprintf fm "(<=)"
  | Geq -> Format.fprintf fm "(>=)"
  | Eq -> Format.fprintf fm "(=)"
  | Int n -> Format.fprintf fm "%d" n
  | Add -> Format.fprintf fm "(+)"
  | Sub -> Format.fprintf fm "(-)"
  | Mul -> Format.fprintf fm "(*)"
  | Tuple 0 -> Format.fprintf fm "()"
  | Tuple 1 -> ()
  | Tuple n -> Format.fprintf fm "(%d)" n
  | Proj(_,0) -> ()
  | Proj(_,i) -> Format.fprintf fm "#%d" i
  | If -> Format.fprintf fm "if_term"
  | Branch -> Format.fprintf fm "br"

and print_term_ML fm = function
    Const c -> print_const_ML fm c
  | Var x -> print_var fm x
  | App _ as t ->
      let t,ts = decomp_app t in
        Format.fprintf fm "(%a)" (print_list print_term_ML " " false) (t::ts)
  | Let(x,t1,t2) -> Format.fprintf fm "(let %a = %a in %a)" print_var x print_term_ML t1 print_term_ML t2
  | Fun(x,t) -> Format.fprintf fm "(fun %a -> %a)" print_var x print_term_ML t

and print_fun_def_ML fm (f,xs,t1,t2) =
  if t1 = Const True
  then Format.fprintf fm "and %a = %a@." (print_list print_var " " false) (f::xs) print_term_ML t2
  else Format.fprintf fm "%a when %a = %a@." (print_list print_var " " false) (f::xs) print_term_ML t1 print_term_ML t2

and print_prog_ML fm (env,defs,s) =
  Format.fprintf fm "let rec if_term b x y = if b then x else y@.";
  Format.fprintf fm "and br x y = if true then x else y@.";
  List.iter (print_fun_def_ML fm) defs;
  Format.fprintf fm "Types:\n%a@." print_env env;



