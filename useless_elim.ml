
open Utilities
open CEGAR_syntax
open CEGAR_type
open CEGAR_util

type typ = TVar of int | TTop | TBase | TFun of typ * typ


exception TypingError


let new_tvar () = TVar (Id.new_int ())




module G = Graph.Pack.Digraph

let calc_scc min n egdes =
  let g = G.create () in
  let vertices = Array.init n G.V.create in
  let () = List.iter (fun (x,y) -> G.add_edge g vertices.(x-min) vertices.(y-min)) egdes in
  let sccs = List.map (List.map (fun l -> min + G.V.label l)) (G.Components.scc_list g) in
    sccs







let rec decomp_tfun = function
    TFun(typ1,typ2) ->
      let typs,typr = decomp_tfun typ2 in
        typ1::typs, typr
  | typ -> [], typ

let rec print_typ fm = function
    TVar n -> Format.fprintf fm "a%d" n
  | TTop -> Format.fprintf fm "Top"
  | TBase -> Format.fprintf fm "Base"
  | TFun(typ1,typ2) -> Format.fprintf fm "(%a -> %a)" print_typ typ1 print_typ typ2





(*
let rec meat typ1 typ2 =
  match typ1,typ2 with
      TVar{contents=typ1}, typ2
    | typ1, TVar{contents=typ2} -> meat typ1 typ2
    | TUnit, typ
    | typ, TUnit -> typ
    | TBool, TBool -> TBool
    | TFun(typ11,typ12), TFun(typ21,typ22) -> TFun(meat typ11 typ21, meat typ12 typ22)
    | _ -> assert false
*)

let rec template = function
    CEGAR_type.TBase _ -> new_tvar ()
  | TAbs _ -> assert false
  | TApp _ -> assert false
  | CEGAR_type.TFun(typ1,typ2) -> TFun(template typ1, template (typ2 (Const Unit)))

let rec reduce_constraint typ1 typ2 =
  match typ1,typ2 with
      TFun(typ11,typ12), TFun(typ21,typ22) ->
        reduce_constraint typ11 typ21 @@
        reduce_constraint typ21 typ11 @@
        reduce_constraint typ12 typ22
    | TFun _, _ -> assert false
    | _, TFun _ -> assert false
    | typ1,typ2 -> [typ1,typ2]

let rec constraints_term env_orig env t typ =
  match t with
      Const (Unit|Int _|True|False|RandBool) -> [TBase, typ]
    | Const _ -> assert false
    | Var x -> reduce_constraint (List.assoc x env) typ
    | App(App(Const (And|Or|Lt|Gt|Leq|Geq|EqUnit|EqBool|EqInt|Add|Sub|Mul), t1), t2) ->
        (TBase,typ) :: constraints_term env_orig env t1 TBase @@
                             constraints_term env_orig env t2 TBase
    | App(App(App(Const If, t1), t2), t3) ->
        constraints_term env_orig env t1 TBase @@
        constraints_term env_orig env t2 typ @@
        constraints_term env_orig env t3 typ
    | App(t1,t2) ->
        let typ1,typ2 =
          match template (get_typ env_orig t1) with
              TFun(typ1,typ2) -> typ1,typ2
            | _ -> assert false
        in
          reduce_constraint typ2 typ @@
          constraints_term env_orig env t1 (TFun(typ1,typ2)) @@
          constraints_term env_orig env t2 typ1
    | Let(x,t1,t2) -> assert false
    | Fun(x,t) -> assert false

let new_env env x =
  let typ = template (List.assoc x env) in
    x, typ

let constraints_def env_orig env (f,xs,t1,_,t2)  =
  let f_typ_orig = List.assoc f env_orig in
  let env_orig' = get_arg_env f_typ_orig xs @@ env_orig in
  let f_typ = List.assoc f env in
  let env' = List.map (new_env env_orig') xs in
  let r_typ =
    let typs,typr = CEGAR_syntax.decomp_tfun f_typ_orig in
    let _,typs' = take2 typs (List.length xs) in
      template (List.fold_right (fun typ1 typ2 -> CEGAR_type.TFun(typ1,fun _ ->typ2)) typs' typr)
  in
  let fun_typ = List.fold_right (fun (_,typ1) typ2 -> TFun(typ1,typ2)) env' r_typ in
  let env'' = env' @@ env in
    reduce_constraint fun_typ f_typ @@
    constraints_term env_orig' env'' t1 TBase @@
    constraints_term env_orig' env'' t2 r_typ

let print_constraint (typ1,typ2) = Format.printf "%a <: %a@." print_typ typ1 print_typ typ2
let print_constraints constrs =
  Format.printf "Constraints:@.";
  List.iter print_constraint constrs;
  Format.printf "@."
(*
let rec subst x typ = function
    TVar y when x = y -> typ
  | TVar y -> TVar y
  | TTop -> TTop
  | TBase -> TBase
  | TFun(typ1,typ2) -> TFun(subst x typ typ1, subst x typ typ2)

let subst_constr x typ (typ1,typ2) = subst x typ typ1, subst x typ typ2

let rec reduce_constraints sol constrs =
  let rec aux flag sol constrs1 = function
      [] -> flag, sol, constrs1
    | (TVar x, TFun(typ21,typ22))::constrs2 ->
        let typ11 = typ21 in
        let typ12 = new_tvar () in
        let typ1 = TFun(typ11, typ12) in
        let sbst = List.map (subst_constr x typ1) in
        let constrs1' = sbst constrs1 in
        let constrs2' = (typ12,typ22) :: sbst constrs2 in
          aux true ((x,typ1)::sol) constrs1' constrs2'
    | (TFun(typ11,typ12), TFun(typ21,typ22))::constrs2 ->
        aux flag sol constrs1 ((typ21,typ11)::(typ12,typ22)::constrs2)
    | (TBase, TFun _)::_ -> raise TypingError
    | (TBase, TBase)::constrs2 -> aux flag sol constrs1 constrs2
    | (typ1,typ2)::constrs2 -> aux flag sol ((typ1,typ2)::constrs1) constrs2
  in
  let flag,sol',constrs' = aux false sol [] constrs in
    if flag
    then reduce_constraints sol' constrs'
    else sol', constrs'
let reduce_constraints constrs = reduce_constraints [] constrs
*)
(*
let rec solve_constraints sol constrs =
  let constrs1,constrs2 = List.partition (fun (typ1,typ2) -> typ2 = TBase) constrs in
    if constrs1 = []
    then
      let rec aux = function
          TVar x -> [x]
        | TTop -> []
        | TBase -> []
        | TFun(typ1,typ2) -> aux typ1 @@ aux typ2
      in
      let vars = uniq (rev_flatten_map (fun (typ1,typ2) -> aux typ1 @@ aux typ2) constrs2) in
        List.rev_map (fun x -> x, TTop) vars @@ sol
    else
      let constrs11,constrs12 = List.partition (function (TVar _, _) -> true | _ -> false) constrs1 in
      let () = if List.exists (fun (typ,_) -> typ <> TBase) constrs12 then raise TypingError in
      let constrs11' = uniq constrs11 in
      let sol' = List.map (function (TVar x, _) -> x, TBase | _ -> assert false) constrs11' in
      let constrs2' = List.fold_left (fun constrs (x,_) -> List.map (subst_constr x TBase) constrs) constrs2 sol' in
        solve_constraints (sol'@@sol) constrs2'
let solve_constraints constrs = solve_constraints [] constrs

let print_sol sol =
  Format.printf "Solution:@.";
  List.iter (fun (x,typ) -> Format.printf "%a := %a@." print_typ (TVar x) print_typ typ) sol;
  Format.printf "@."
*)
(*
let rec arity_constr_term env t typ =
  match t,typ with
      Const c, _ -> []
    | Var x, _ -> []
    | App(App(App(Const If, t1), t2), t3), _ ->
        arity_constr_term t2 TBase @@ arity_constr_term t3 TBase
    | App _ as t, TFun _ ->
        let t1,ts = decomp_app t in
        let f = match t1 with Var f -> f | _ -> assert false in
        let typs,typr = decomp_tfun (List.assoc f env) in
        let _,typs' = take2 (List.length ts) typs in
        let typ' = List.fold_right (fun typ1 typ2 -> TFun(typ1,typ2)) typs' typr in
        let typ'' = merge typ typ' in
    | App _ as t ->
        let t1,ts = decomp_app t in
        let f = match t1 with Var f -> f | _ -> assert false in
        let typs,_ = decomp_tfun (List.assoc f env) in
          List.fold_left2 (fun constr t typ -> arity_constr_term t typ @@ constr) [] ts typs
    | Let _ -> assert false
    | Fun _ -> assert false

let arity_constr_def env (f,xs,t1,es,t2) =
  let f_typ = List.assoc f env in
  let use = use_of_typ f_typ in
  let xs' = List.map (List.nth xs) use in
  let rec get_arg_env typ xs =
    match typ,xs with
        TFun(typ1,typ2), x::xs -> (x,typ1) :: get_arg_env typ2 xs
      | _ -> []
  in
  let env' = get_arg_env f_typ xs @@ env in
    f, xs', elim_term env' t1, es, elim_term env' t2
*)

let rec subst x y = function
    TVar z when x = z -> TVar y
  | TVar z -> TVar z
  | TTop -> TTop
  | TBase -> TBase
  | TFun(typ1,typ2) -> TFun(subst x y typ1, subst x y typ2)
let subst_constr x y (typ1,typ2) = subst x y typ1, subst x y typ2

let rec calc_eq constrs =
  let edges = rev_flatten_map (function (TVar x, TVar y) -> [x,y] | _ -> []) constrs in
  let max = List.fold_left (fun acc (x,y) -> max acc (max x y)) 0 edges in
  let min = List.fold_left (fun acc (x,y) -> min acc (min x y)) max edges in
  let sccs = calc_scc min (max-min+1) edges in
    List.map (fun scc -> List.hd scc, List.tl scc) sccs
    

let solve_constraints constrs env eqs =
  let hash = Hashtbl.create 0 in
  let rec solve = function
      TTop -> TTop
    | TBase -> TBase
    | TFun(typ1,typ2) -> TFun(solve typ1, solve typ2)
    | TVar x ->
        try
          Hashtbl.find hash x
        with Not_found ->
          try
            let _,xs = List.find (fun (y,_) -> x = y) eqs in
            let constrs' = List.filter (function (TVar y,_) -> List.mem y (x::xs) | _ -> false) constrs in
            let constrs'' = List.filter (function (_,TVar y) -> not (List.mem y (x::xs)) | _ -> true) constrs' in
            let ub = List.map (fun (_,typ) -> solve typ) constrs'' in
            let typ = if List.mem TBase ub then TBase else TTop in
              Hashtbl.add hash x typ; typ
          with Not_found ->
            let y,_ = List.find (fun (_,xs) -> List.mem x xs) eqs in
              solve (TVar y)
  in
    List.map (fun (f,typ) -> f, solve typ) env

let print_env _ env =
  Format.printf "Environment:@.";
  List.iter (fun (x,typ) -> Format.printf "%s: %a@." x print_typ typ) env;
  Format.printf "@."

let infer ((env,defs,main):prog) =
  let env' = List.map (fun (f,_) -> (new_env env) f) env in
  let constrs = (List.assoc main env', TBase) :: rev_flatten_map (constraints_def env env') defs in
  let eqs = calc_eq constrs in
    solve_constraints constrs env' eqs

let use_of_typ typ =
  let typs,_ = decomp_tfun typ in
  let rec aux i = function
      TBase -> [i]
    | TTop -> []
    | TFun(_,typ) -> aux i typ
    | TVar _ -> assert false
  in
    List.flatten (mapi aux typs)

let rec elim_term env = function
    Const c -> Const c
  | Var x -> Var x
  | App _ as t ->
      let t1,ts = decomp_app t in
      let ts' =
        match t1 with
            Const _ -> List.map (elim_term env) ts
          | Var x ->
              let n = List.length ts in
              let use = List.filter (fun i -> i < n) (use_of_typ (List.assoc x env)) in
              let ts' = List.map (List.nth ts) use in
                List.map (elim_term env) ts'
          | _ -> assert false
      in
        make_app t1 ts'
  | Let _ -> assert false
  | Fun _ -> assert false

let elim_def env (f,xs,t1,es,t2) =
  let f_typ = List.assoc f env in
    if snd (decomp_tfun f_typ) = TTop
    then []
    else
      let use = use_of_typ f_typ in
      let xs' = List.map (List.nth xs) use in
        let rec get_arg_env typ xs =
          match typ,xs with
              TFun(typ1,typ2), x::xs -> (x,typ1) :: get_arg_env typ2 xs
            | TVar _, _ -> assert false
            | _ -> []
        in
        let env' = get_arg_env f_typ xs @@ env in
          [f, xs', elim_term env' t1, es, elim_term env' t2]

(** call-by-name *)
let elim ((env,defs,main):prog) : prog =
  let env' = infer (env,defs,main) in
  let defs' = flatten_map (elim_def env') defs in
    Format.printf "BEFORE:@.%a@.@.%a@.AFTER:@.%a@." CEGAR_print.prog (env,defs,main) print_env env' CEGAR_print.prog (env,defs',main);
    Typing.infer ([],defs',main)


