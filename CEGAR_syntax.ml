
open Utilities
open CEGAR_type
open ExtString

type var = string


type const =
    Temp of string (* for temporary use *)
  | Unit
  | True
  | False
  | RandInt
  | RandBool
  | And
  | Or
  | Not
  | Lt
  | Gt
  | Leq
  | Geq
  | EqUnit
  | EqBool
  | EqInt
  | Int of int
  | Add
  | Sub
  | Mul
  | Tuple of int
  | Proj of int * int (* 0-origin *)
  | If (* for abstraction and model-checking *)
  | Bottom
  | Label of int



and t =
    Const of const
  | Var of var
  | App of t * t
  | Let of var * t * t (*** for temporary use ***)
  | Fun of var * typ option * t (*** for temporary use ***)


and event = Event of string | Branch of int

(*
and ce_node = BranchNode of int | EventNode of string
*)
and ce_node = int
and ce = ce_node list



and fun_def = var * var list * t * event list * t
and typ = t CEGAR_type.t
and env = (var * typ) list
and prog = {env:env; defs:fun_def list; main:var}


let new_id s = Id.to_string (Id.new_var s Type.TUnknown)
let decomp_id s =
  try
    let len = String.length s in
    let i = String.rindex s '_' in
      String.sub s 0 i, int_of_string (String.sub s (i+1) (len-i-1))
  with Failure "int_of_string" | Not_found -> s, 0
let add_name x s =
  let name,n = decomp_id x in
    name ^ s ^ "_" ^ if n <> 0 then string_of_int n else ""
let id_name x = fst (decomp_id x)
let rename_id s =
  let name = id_name s in
    name ^ "_" ^ string_of_int (Id.new_int ())





let make_app t ts = List.fold_left (fun t1 t2 -> App(t1, t2)) t ts
let make_fun xs t =
  let f = new_id "f" in
    [f, xs, Const True, t], f
let make_annot_fun x typ t = Fun(x, Some typ, t)
let make_fun_temp xs t = List.fold_right (fun x t -> Fun(x,None,t)) xs t
let make_if t1 t2 t3 =
  match t1 with
      Const True -> t2
    | Const False -> t3
    | _ -> make_app (Const If) [t1;t2;t3]
let make_int n = Const (Int n)
let make_br t1 t2 = make_if (Const RandBool) t1 t2
let make_and t1 t2 = make_app (Const And) [t1; t2]
let make_or t1 t2 = make_app (Const Or) [t1; t2]
let make_not t = App(Const Not, t)
let make_lt t1 t2 = make_app (Const Lt) [t1; t2]
let make_gt t1 t2 = make_app (Const Gt) [t1; t2]
let make_leq t1 t2 = make_app (Const Leq) [t1; t2]
let make_geq t1 t2 = make_app (Const Geq) [t1; t2]
let make_eq_int t1 t2 = make_app (Const EqInt) [t1; t2]
let make_eq_bool t1 t2 = make_app (Const EqBool) [t1; t2]
let make_add t1 t2 = make_app (Const Add) [t1; t2]
let make_sub t1 t2 = make_app (Const Sub) [t1; t2]
let make_mul t1 t2 = make_app (Const Mul) [t1; t2]
let make_label n t = make_app (Const (Label n)) [t]





let rec get_fv = function
    Const _ -> []
  | Var x -> [x]
  | App(t1, t2) -> get_fv t1 @@ get_fv t2
  | Let(x,t1,t2) -> get_fv t1 @@ diff (get_fv t2) [x]
  | Fun(x,_,t) -> diff (get_fv t) [x]
let get_fv t = uniq (get_fv t)


let rec get_typ_arity = function
    TFun(typ1,typ2) -> 1 + get_typ_arity (typ2 (Const Unit))
  | typ -> 0


let rec decomp_app = function
    App(t1,t2) ->
      let t,ts = decomp_app t1 in
        t, ts@[t2]
  | t -> t, []
let rec decomp_fun = function
    Fun(x,_,t) ->
      let xs,t = decomp_fun t in
        x::xs, t
  | t -> [], t
let rec decomp_annot_fun acc = function
    Fun(x, typ, t) -> decomp_annot_fun ((x,typ)::acc) t
  | t -> List.rev acc, t
let decomp_annot_fun t = decomp_annot_fun [] t
let rec decomp_tfun = function
    TFun(typ1,typ2) ->
      let typs,typ = decomp_tfun (typ2 (Const Unit)) in
        typ1::typs, typ
  | typ -> [], typ


let get_var_arity f env = get_typ_arity (List.assoc f env)

let rec is_CPS_value env = function
    Const Unit
  | Const True
  | Const False
  | Const (Int _)
  | Var _ -> true
  | App(App(Const And, t1), t2)
  | App(App(Const EqUnit, t1), t2)
  | App(App(Const EqInt, t1), t2)
  | App(App(Const EqBool, t1), t2)
  | App(App(Const Or, t1), t2)
  | App(App(Const Lt, t1), t2)
  | App(App(Const Gt, t1), t2)
  | App(App(Const Leq, t1), t2)
  | App(App(Const Geq, t1), t2)
  | App(App(Const Add, t1), t2)
  | App(App(Const Sub, t1), t2)
  | App(App(Const Mul, t1), t2) -> is_CPS_value env t1 && is_CPS_value env t2
  | App(Const Not, t) -> is_CPS_value env t
  | App _ as t ->
      let t1,ts = decomp_app t in
      let n = match t1 with Var f -> get_var_arity f env | _ -> 0 in
        n > List.length ts && List.for_all (is_CPS_value env) ts
  | Let _ -> assert false
  | Fun _ -> assert false
  | _ -> false
let is_CPS_def env (f,xs,cond,es,t) =
  let b1 = is_CPS_value env cond in
  let b2 =
    match t with
        Const _ -> true
      | Var _ -> true
      | App _ -> List.for_all (is_CPS_value env) (snd (decomp_app t))
      | Let _ -> assert false
      | Fun _ -> assert false
  in
    b1 && b2

let is_CPS {env=env;defs=defs} = List.for_all (is_CPS_def env) defs


let is_external x = String.contains x '.'
let is_parameter x = String.starts_with x Flag.extpar_header
