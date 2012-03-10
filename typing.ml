
open Utilities
open CEGAR_syntax
open CEGAR_type


exception CannotUnify
exception External

type typ =
    TUnit
  | TBool
  | TInt
  | TVar of typ option ref
  | TFun of typ * typ
  | TTuple of typ list

let rec print_typ fm = function
    TUnit -> Format.fprintf fm "unit"
  | TBool -> Format.fprintf fm "bool"
  | TInt -> Format.fprintf fm "int"
  | TVar{contents=Some typ} -> print_typ fm typ
  | TVar{contents=None} -> Format.fprintf fm "?"
  | TFun(typ1,typ2) -> Format.fprintf fm "(%a -> %a)" print_typ typ1 print_typ typ2
  | TTuple typs -> Format.fprintf fm "(%a)" (print_list print_typ " * " false) typs

let new_tvar () = TVar (ref None)

let rec occurs r = function
    TVar{contents = Some typ} -> occurs r typ
  | TFun(typ1,typ2) -> occurs r typ1 || occurs r typ2
  | TVar({contents = None} as r') -> r == r'
  | _ -> false

let rec unify typ1 typ2 =
  match typ1, typ2 with
      TVar{contents = Some typ1}, _ -> unify typ1 typ2
    | _, TVar{contents = Some typ2} -> unify typ1 typ2
    | TUnit, TUnit -> ()
    | TBool, TBool -> ()
    | TInt, TInt -> ()
    | TFun(typ11, typ12), TFun(typ21, typ22) ->
        unify typ11 typ21;
        unify typ12 typ22
    | TTuple [], typ
    | typ, TTuple [] -> unify TUnit typ
    | TTuple typs1, TTuple typs2 ->
        List.iter2 unify typs1 typs2
    | TVar r1, TVar r2 when r1 == r2 -> ()
    | TVar({contents = None} as r), typ
    | typ, TVar({contents = None} as r) ->
        assert (not (occurs r typ));
        r := Some typ
    | _ -> Format.printf "UNIFY: %a, %a@." print_typ typ1 print_typ typ2; assert false


let nil = fun _ -> []

let rec trans_typ = function
    TUnit -> TBase(CEGAR_type.TUnit,nil)
  | TBool -> TBase(CEGAR_type.TBool,nil)
  | TInt -> TBase(CEGAR_type.TInt,nil)
  | TVar{contents=None} -> TBase(CEGAR_type.TUnit,nil)
  | TVar{contents=Some typ} -> trans_typ typ
  | TFun(typ1,typ2) -> CEGAR_type.TFun(trans_typ typ1, fun _ -> trans_typ typ2)
  | TTuple typs -> make_tapp (TBase(CEGAR_type.TTuple (List.length typs),nil)) (List.map trans_typ typs)

let get_typ_const = function
  | Unit -> TUnit
  | True -> TBool
  | False -> TBool
  | RandBool -> TBool
  | RandInt ->
      let typ = new_tvar () in
        TFun(TFun(TInt,typ),typ)
  | EqUnit -> TFun(TUnit,TFun(TUnit,TBool))
  | EqInt -> TFun(TInt,TFun(TInt,TBool))
  | EqBool -> TFun(TBool,TFun(TBool,TBool))
  | And -> TFun(TBool,TFun(TBool,TBool))
  | Or -> TFun(TBool,TFun(TBool,TBool))
  | Not -> TFun(TBool,TBool)
  | Lt -> TFun(TInt,TFun(TInt,TBool))
  | Gt -> TFun(TInt,TFun(TInt,TBool))
  | Leq -> TFun(TInt,TFun(TInt,TBool))
  | Geq -> TFun(TInt,TFun(TInt,TBool))
  | Add -> TFun(TInt,TFun(TInt,TInt))
  | Sub -> TFun(TInt,TFun(TInt,TInt))
  | Mul -> TFun(TInt,TFun(TInt,TInt))
  | Int _ -> TInt
  | If ->
      let typ = new_tvar () in
        TFun(TBool,TFun(typ,TFun(typ,typ)))
  | Proj(n,i) ->
      let typs = Array.to_list (Array.init n (fun _ -> new_tvar())) in
        TFun(TTuple typs, List.nth typs i)
  | Tuple n ->
      let typs = Array.to_list (Array.init n (fun _ -> new_tvar())) in
        List.fold_right (fun typ1 typ2 -> TFun(typ1,typ2)) typs (TTuple typs)
  | Bottom -> new_tvar ()
  | Temp _ -> assert false
  | Label _ ->
      let typ = new_tvar () in
        TFun(typ,typ)

let rec infer_term env = function
    Const c -> get_typ_const c
  | Var x ->
      let typ =
        try
          List.assoc x env
        with
            Not_found when is_external x -> raise External
          | Not_found when is_parameter x -> TInt
          | Not_found -> Format.printf "VAR: %s@." x; assert false
      in
        typ
  | App(t1,t2) ->
      let typ1 = infer_term env t1 in
      let typ2 = infer_term env t2 in
      let typ = new_tvar () in
      let typ' = TFun(typ2,typ) in
        unify typ1 typ';
        typ
  | Let(x,t1,t2) ->
      let typ1 = infer_term env t1 in
      let env' = (x,typ1)::env in
      let typ2 = infer_term env' t2 in
        typ2
  | Fun(x,_,t) ->
      let typ_x = new_tvar() in
      let env' = (x,typ_x)::env in
      let typ1 = infer_term env' t in
        TFun(typ_x,typ1)

let infer_def env (f,xs,t1,_,t2) =
  if false then Format.printf "%a@." CEGAR_print.var f;
  let typs = List.map (fun _ -> new_tvar()) xs in
  let env' = List.map2 (fun x typ -> x,typ) xs typs @ env in
  let typ1 = infer_term env' t1 in
  let typ2 = infer_term env' t2 in
  let typ = try List.assoc f env with _ -> assert false in
  let typ' = List.fold_right (fun typ1 typ2 -> TFun(typ1,typ2)) typs typ2 in
    unify typ1 TBool;
    unify typ typ'


let infer (_,defs,main) =
  let env = List.map (fun (f,_,_,_,_) -> f, new_tvar ()) defs in
  let () = unify TUnit (List.assoc main env) in
  let () = List.iter (infer_def env) defs in
  let env' = List.map (fun (f,_) -> f, trans_typ (List.assoc f env)) env in
    env', defs, main


