
open Utilities
open CEGAR_syntax
open CEGAR_type

type typ = TVar of int | TTop | TBase | TFun of typ * typ


exception TypingError


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

let new_tvar () = TVar (Id.new_int ())

let rec constraints_term env t typ =
  match t with
      Const (Unit|Int _|True|False|RandBool) -> [TBase, typ]
    | Const _ -> assert false
    | Var x -> [List.assoc x env, typ]
    | App(App(Const (And|Or|Lt|Gt|Leq|Geq|EqUnit|EqBool|EqInt|Add|Sub|Mul), t1), t2) ->
        (TBase,typ) :: constraints_term env t1 TBase @@ constraints_term env t2 TBase
    | App(App(App(Const If, t1), t2), t3) ->
        constraints_term env t1 TBase @@
        constraints_term env t2 typ @@
        constraints_term env t3 typ
    | App(t1,t2) ->
        let typ1 = new_tvar () in
        let typ2 = new_tvar () in
          (typ2,typ) :: constraints_term env t1 (TFun(typ1,typ2))
                     @@ constraints_term env t2 typ1
    | Let(x,t1,t2) -> assert false
    | Fun(x,t) -> assert false

let new_env x =
  let typ = new_tvar () in
    x, typ

let constraints_def env (f,xs,t1,_,t2)  =
  let f_typ = List.assoc f env in
  let env' = List.map new_env xs in
  let r_typ = new_tvar () in
  let fun_typ = List.fold_right (fun (_,typ1) typ2 -> TFun(typ1,typ2)) env' r_typ in
  let env'' = env' @@ env in
    (fun_typ, f_typ) :: constraints_term env'' t1 TBase @@ constraints_term env'' t2 r_typ

let print_constraint (typ1,typ2) = Format.printf "%a <: %a@." print_typ typ1 print_typ typ2
let print_constraints constrs =
  Format.printf "Constraints:@.";
  List.iter print_constraint constrs;
  Format.printf "@."

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
    | (TVar x, (TFun(typ21,typ22) as typ2))::constrs2 ->
        let typ1 = TFun(new_tvar(), new_tvar()) in
        let sbst = List.map (subst_constr x typ1) in
        let constrs1' = sbst constrs1 in
        let constrs2' = (typ1,typ2) :: sbst constrs2 in
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
    else
      let check = function
          TFun _, _ -> false
        | _ -> true
      in
        sol', List.filter check constrs'
let reduce_constraints constrs = reduce_constraints [] constrs


let rec solve_constraints sol constrs =
  let constrs1,constrs2 = List.partition (fun (typ1,typ2) -> typ2 = TBase) constrs in
    if constrs1 = []
    then
      let aux = function
          TVar x -> [x]
        | TTop -> []
        | TBase -> []
        | TFun _ -> assert false
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


let infer ((env,defs,main):prog) =
  let env = List.map (fun (f,_) -> new_env f) env in
  let constrs = (List.assoc main env, TBase) :: rev_flatten_map (constraints_def env) defs in
  let sol,constrs' = reduce_constraints constrs in
  let sol' = solve_constraints constrs' in
  let sbst = List.fold_right (fun (x,typ) -> subst x typ) (sol'@@sol) in
    List.map (fun (f,typ) -> f, sbst typ) env

let use_of_typ typ =
  let typs,_ = decomp_tfun typ in
  let aux i = function
      TBase -> [i]
    | TTop -> []
    | TFun _ -> [i]
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

let rec get_arg_env typ xs =
  match typ,xs with
      TFun(typ1,typ2), x::xs -> (x,typ1) :: get_arg_env typ2 xs
    | _ -> []

let elim_def env (f,xs,t1,es,t2) =
  let f_typ = List.assoc f env in
  let use = use_of_typ f_typ in
  let xs' = List.map (List.nth xs) use in
  let env' = get_arg_env f_typ xs @@ env in
    f, xs', elim_term env' t1, es, elim_term env' t2

let print_env env =
  Format.printf "Environment:@.";
  List.iter (fun (x,typ) -> Format.printf "%s: %a@." x print_typ typ) env;
  Format.printf "@."

(** call-by-name *)
let elim ((env,defs,main):prog) : prog =
  let env' = infer (env,defs,main) in
  let defs' = List.map (elim_def env') defs in
    Format.printf "BEFORE:@.%a@." CEGAR_print.prog (env,defs,main);
    print_env env';
    Format.printf "@.AFTER:@.%a@." CEGAR_print.prog ([],defs',main);
    Typing.infer ([],defs',main)


