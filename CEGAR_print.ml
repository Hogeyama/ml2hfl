
open Utilities
open CEGAR_syntax
open CEGAR_type

let counter = ref 0
let new_id x =
  let x' = x ^ "_" ^ string_of_int !counter in
    incr counter;
    x'

let rec occur_arg_pred x = function
    TBase(_,ps) -> List.mem x (rev_flatten_map get_fv (ps (Const Unit)))
  | TFun(typ1,typ2) ->
      let typ2 = typ2 (Const Unit) in
        occur_arg_pred x typ1 || occur_arg_pred x typ2
  | TAbs _ -> assert false
  | TApp(typ1,typ2) -> occur_arg_pred x typ1 || occur_arg_pred x typ2

let rec print_var = Format.pp_print_string

and print_var_typ env fm x = Format.fprintf fm "(%a:%a)" print_var x print_typ (List.assoc x env)

and print_typ_base fm = function
    TUnit -> Format.fprintf fm "unit"
  | TBool -> Format.fprintf fm "bool"
  | TInt -> Format.fprintf fm "int"
  | TTuple n -> Format.fprintf fm "tuple"
  | TList _ -> assert false

and print_typ_aux var fm = function
    TBase(b,ps) ->
      let x,occur = match var with None -> new_id "x", false | Some(x,occur) -> x, occur in
      let preds = ps (Var x) in
        if occur || List.mem x (rev_flatten_map get_fv preds) then Format.fprintf fm "%a:" print_var x;
        Format.fprintf fm "%a" print_typ_base b;
        if preds <> [] then Format.fprintf fm "[%a]" (print_list print_term ";" false) preds
  | TFun _ as typ ->
      let rec aux b fm = function
          TFun(typ1, typ2) ->
            let x = new_id "x" in
            let typ2 = typ2 (Var x) in
            let s1,s2 = if b then "(",")" else "","" in
            let var' = Some(x, occur_arg_pred x typ2) in
            let b' = match typ2 with TFun _ -> true | _ -> false in
              if b'
              then Format.fprintf fm "%s@[%a ->@ %a@]%s" s1 (print_typ_aux var') typ1 (aux false) typ2 s2
              else Format.fprintf fm "%s%a -> %a%s" s1 (print_typ_aux var') typ1 (aux false) typ2 s2
        | typ -> print_typ_aux None fm typ
      in
        aux true fm typ
  | TApp _ as typ ->
      let typ,typs = decomp_tapp typ in
        Format.fprintf fm "(%a)" (print_list (print_typ_aux None) " " false) (typ::typs)
  | TAbs _ -> assert false

and print_typ fm typ =
  counter := 1;
  print_typ_aux None fm typ

and print_env fm env =
  List.iter (fun (f,typ) -> Format.fprintf fm "%a : %a@." print_var f print_typ typ) env

and print_const fm = function
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
  | EqUnit -> Format.fprintf fm "="
  | EqInt -> Format.fprintf fm "="
  | EqBool -> Format.fprintf fm "<=>"
  | Int n -> Format.fprintf fm "%d" n
  | Add -> Format.fprintf fm "+"
  | Sub -> Format.fprintf fm "-"
  | Mul -> Format.fprintf fm "*"
  | Tuple n -> Format.fprintf fm "(%d)" n
  | Proj(_,i) -> Format.fprintf fm "#%d" i
  | If -> Format.fprintf fm "if"
  | Bottom -> Format.fprintf fm "_|_"
  | Temp _ -> assert false

and print_term fm = function
    Const c -> print_const fm c
  | Var x -> print_var fm x
  | App(App(App(Const If, Const RandBool), Const True), Const False) ->
      print_const fm RandBool
  | App(App(Const ((EqInt|EqBool|Lt|Gt|Leq|Geq|Add|Sub|Mul|Or|And) as op), t1), t2) ->
      Format.fprintf fm "(@[%a@ %a@ %a@])" print_term t1 print_const op print_term t2
  | App _ as t ->
      let t,ts = decomp_app t in
      let rec pr fm = function
          [] -> ()
        | t::ts -> Format.fprintf fm "@ %a%a" print_term t pr ts
      in
        Format.fprintf fm "(@[<hov 1>%a%a@])" print_term t pr ts
  | Let(x,t1,t2) ->
      let xs,t1 = decomp_fun t1 in
        Format.fprintf fm "(let %a %a@ =@ %a@ in@ %a)" print_var x (print_list print_var " " false) xs print_term t1 print_term t2
  | Fun _ as t ->
      let env,t' = decomp_annot_fun t in
      let pr fm (x,typ) =
        match typ with
            None -> print_var fm x
          | Some typ when !Flag.print_fun_arg_typ -> Format.fprintf fm "(%a:%a)" print_var x print_typ typ
          | Some typ -> print_var fm x
      in
        Format.fprintf fm "(@[fun %a@ ->@ %a@])" (print_list pr " " false) env print_term t'

and print_fun_def fm (f,xs,t1,es,t2) =
  let aux = fun s -> function (Event s) -> " {" ^ s ^ "} =>" | (Branch n) -> " l" ^ string_of_int n ^ " =>" in
  let s = List.fold_left aux "" es in
    if t1 = Const True
    then
      let ys,t2 = decomp_fun t2 in
        Format.fprintf fm "@[<hov 4>%a ->%s@ %a@." (print_list print_var " " false) (f::xs@ys) s print_term t2
    else Format.fprintf fm "@[<hov 4>%a when %a ->%s@ %a@." (print_list print_var " " false) (f::xs) print_term t1 s print_term t2

and print_prog fm (_,defs,s) =
  Format.fprintf fm "Main: %a@." print_var s;
  List.iter (print_fun_def fm) defs

and print_prog_typ fm (env,defs,s) =
  Format.fprintf fm "Main: %a@." print_var s;
  List.iter (print_fun_def fm) defs;
  Format.fprintf fm "Types:\n%a@." print_env env;




and print_const_ML fm = function
  | Unit -> Format.fprintf fm "()"
  | True -> Format.fprintf fm "true"
  | False -> Format.fprintf fm "false"
  | RandBool -> Format.fprintf fm "(Random.bool())"
  | RandInt -> Format.fprintf fm "rand_int()"
  | And -> Format.fprintf fm "(&&)"
  | Or -> Format.fprintf fm "(||)"
  | Not -> Format.fprintf fm "(not)"
  | Lt -> Format.fprintf fm "(<)"
  | Gt -> Format.fprintf fm "(>)"
  | Leq -> Format.fprintf fm "(<=)"
  | Geq -> Format.fprintf fm "(>=)"
  | EqBool -> Format.fprintf fm "(=)"
  | EqInt -> Format.fprintf fm "(=)"
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
  | Temp _ -> assert false
  | Bottom -> Format.fprintf fm "()"
  | EqUnit -> assert false

and print_term_ML fm = function
    Const c -> print_const_ML fm c
  | Var x -> print_var fm x
  | App(App(App(Const If, t1), t2), t3) ->
      Format.fprintf fm "(if %a then %a else %a)" print_term_ML t1 print_term_ML t2 print_term_ML t3
  | App(t1,t2) ->
      Format.fprintf fm "(%a %a)" print_term_ML t1 print_term_ML t2
  | Let(x,t1,t2) ->
      let xs,t1 = decomp_fun t1 in
        Format.fprintf fm "(let %a %a= %a in %a)" print_var x (print_list print_var " " true) xs print_term_ML t1 print_term_ML t2
  | Fun(x,_,t) ->
      Format.fprintf fm "(fun %a -> %a)" print_var x print_term_ML t

and print_fun_def_ML fm (f,xs,t1,_,t2) =
  if t1 = Const True
  then Format.fprintf fm "and %a = %a@." (print_list print_var " " false) (f::xs) print_term_ML t2
  else Format.fprintf fm "%a when %a = %a@." (print_list print_var " " false) (f::xs) print_term_ML t1 print_term_ML t2

and print_prog_ML fm (env,defs,s) =
  Format.fprintf fm "let rec f x = f x@.";
  List.iter (print_fun_def_ML fm) defs;
  if env <> [] then Format.fprintf fm "Types:\n%a@." print_env env



let rec print_base_typ_as_tree fm = function
    TUnit -> Format.fprintf fm "TUnit"
  | TInt -> Format.fprintf fm "TInt"
  | TBool -> Format.fprintf fm "TBool"
  | TList -> Format.fprintf fm "TList"
  | TTuple n -> Format.fprintf fm "(TTuple %d)" n

and print_typ_as_tree fm = function
    TBase(b,ps) ->
      let x = new_id "x" in
        Format.fprintf fm "(TBase(%a,fun %s->%a))" print_base_typ_as_tree b x (print_list_as_tree print_term_as_tree) (ps (Var x))
  | TAbs _ -> assert false
  | TApp _ -> assert false
  | TFun(typ1,typ2) ->
      let x = new_id "x" in
        Format.fprintf fm "(TFun(%a,fun %s->%a))" print_typ_as_tree typ1 x print_typ_as_tree (typ2 (Var x))

and print_var_as_tree fm x = Format.printf "\"%s\"" x

and print_const_as_tree fm = function
  | Unit -> Format.fprintf fm "Unit"
  | True -> Format.fprintf fm "True"
  | False -> Format.fprintf fm "False"
  | RandBool -> Format.fprintf fm "RandBool"
  | RandInt -> Format.fprintf fm "RandInt"
  | And -> Format.fprintf fm "And"
  | Or -> Format.fprintf fm "Or"
  | Not -> Format.fprintf fm "Not"
  | Lt -> Format.fprintf fm "Lt"
  | Gt -> Format.fprintf fm "Gt"
  | Leq -> Format.fprintf fm "Leq"
  | Geq -> Format.fprintf fm "Geq"
  | EqBool -> Format.fprintf fm "EqBool"
  | EqInt -> Format.fprintf fm "EqInt"
  | Int n -> Format.fprintf fm "(Int %d)" n
  | Add -> Format.fprintf fm "Add"
  | Sub -> Format.fprintf fm "Sub"
  | Mul -> Format.fprintf fm "Mult"
  | Tuple n -> assert false
  | Proj _ -> assert false
  | If -> Format.fprintf fm "If"
  | Temp _ -> assert false
  | Bottom -> Format.fprintf fm "Bottom"
  | EqUnit -> assert false

and print_term_as_tree fm = function
    Const c -> Format.fprintf fm "(Const %a)" print_const_as_tree c
  | Var x -> Format.fprintf fm "(Var %a)" print_var_as_tree x
  | App(t1,t2) -> Format.fprintf fm "(App(%a,%a))" print_term_as_tree t1 print_term_as_tree t2
  | Let _ -> assert false
  | Fun _ -> assert false

and print_event_as_tree fm = function
    Event s -> Format.fprintf fm "(Event \"%s\")" s
  | Branch n -> Format.fprintf fm "(Branch %d)" n

and print_list_as_tree (pr:Format.formatter -> 'a -> unit) fm xs = Format.fprintf fm "[%a]" (print_list pr ";" false) xs

and print_fun_def_as_tree fm (f,xs,t1,es,t2) =
  Format.fprintf fm "%a,%a,%a,%a,%a"
    print_var_as_tree f
    (fun fm xs -> Format.fprintf fm "[%a]" (print_list print_var_as_tree ";" false) xs) xs
(*(print_list_as_tree Format.pp_print_string) xs
*)
    print_term_as_tree t1
    (fun fm xs -> Format.fprintf fm "[%a]" (print_list print_event_as_tree ";" false) xs) es
    print_term_as_tree t2

and print_env_as_tree fm env =
  let aux fm (f,typ) = Format.printf "%a,%a" print_var_as_tree f print_typ_as_tree typ in
    Format.fprintf fm "[%a]" (print_list aux ";" false) env

and print_prog_as_tree fm (env,defs,s) =
  Format.fprintf fm "(%a,%a,%a)"
    print_env_as_tree env
    (fun fm xs -> Format.fprintf fm "[%a]" (print_list print_fun_def_as_tree ";" false) xs) defs
    print_var_as_tree s




let print_node fm = function
    BranchNode n -> Format.fprintf fm "#%d" n
  | EventNode s -> Format.fprintf fm "%s" s


(*
let print_node fm = function
    BranchNode n -> Format.fprintf fm "BranchNode %d" n
  | EventNode s -> Format.fprintf fm "EventNode %S" s
*)


let print_ce = print_list print_node "; " false




let fun_def = print_fun_def
let term = print_term
let var = print_var
let typ = print_typ
let ce = print_ce
let prog = print_prog
let prog_typ = print_prog_typ

