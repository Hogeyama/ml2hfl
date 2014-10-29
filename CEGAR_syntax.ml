
open Util
open CEGAR_type

type var = string

module VarSet =
  Set.Make(
    struct
      type t = var
      let compare = compare
    end)

type const =
  | Temp of string (* for temporary use *)
  | Unit
  | True
  | False
  | Char of char
  | String of string
  | Float of string
  | Int32 of int32
  | Int64 of int64
  | Nativeint of nativeint
  | RandInt of int option
  | RandBool
  | RandVal of string
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
  | CmpPoly of string * string
  | Int of int
  | Add
  | Sub
  | Mul
  | Tuple of int
  | Proj of int * int (* 0-origin *)
  | If (* for abstraction and model-checking *)
  | Bottom
  | Label of int
  | TreeConstr of int * string
  | CPS_result


and t =
  | Const of const
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


let prefix_randint = "#randint"
let make_randint_name n = Format.sprintf "%s_%d" prefix_randint n
let decomp_randint_name s =
  try
    let s1,s2 = String.split s "_" in
    assert (s1 = prefix_randint);
    int_of_string s2
  with _ -> raise (Invalid_argument "decomp_randint_name")
let is_randint_var s =
  try
    ignore @@ decomp_randint_name s;
    true
  with _ -> false


let _Const c = Const c
let _Var x = Var x
let _App t1 t2 = App(t1,t2)
let _Let x t1 t2 = Let(x,t1,t2)


let new_id name = Id.to_string (Id.new_var ~name Type.typ_unknown)
let decomp_id s =
  try
    let len = String.length s in
    let i = String.rindex s '_' in
    String.sub s 0 i, int_of_string (String.sub s (i+1) (len-i-1))
  with Failure "int_of_string" | Not_found -> s, 0
let add_name x s =
  let name,n = decomp_id x in
  if n <> 0
  then name ^ s ^ "_" ^ string_of_int n
  else name ^ s
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
  | Const True -> t2
  | Const False -> t3
  | _ -> make_app (Const If) [t1;t2;t3]
let make_int n = Const (Int n)
let make_br t1 t2 = make_if (Const RandBool) t1 t2
let make_and t1 t2 =
  match t1,t2 with
  | Const True, t
  | t, Const True -> t
  | _ -> make_app (Const And) [t1; t2]
let make_or t1 t2 =
  match t1,t2 with
  | Const False, t
  | t, Const False -> t
  | _ -> make_app (Const Or) [t1; t2]
let make_not t = App(Const Not, t)
let make_lt t1 t2 = make_app (Const Lt) [t1; t2]
let make_gt t1 t2 = make_app (Const Gt) [t1; t2]
let make_leq t1 t2 = make_app (Const Leq) [t1; t2]
let make_geq t1 t2 = make_app (Const Geq) [t1; t2]
let make_eq_int t1 t2 = make_app (Const EqInt) [t1; t2]
let make_eq_bool t1 t2 =
  match t1,t2 with
  | Const False, t
  | t, Const False -> make_not t
  | Const True, t
  | t, Const True -> t
  | _ -> make_app (Const EqBool) [t1; t2]
let make_add t1 t2 = make_app (Const Add) [t1; t2]
let make_sub t1 t2 = make_app (Const Sub) [t1; t2]
let make_mul t1 t2 = make_app (Const Mul) [t1; t2]
let make_label n t = make_app (Const (Label n)) [t]
let make_let bindings t =
  List.fold_right (fun (x,t) t' -> Let(x,t,t')) bindings t

let make_tree label ts = make_app (Const (TreeConstr(List.length ts, label))) ts

let rec add_bool_labels leaf = function
  | [] -> leaf
  | b::bs -> make_tree (if b then "true" else "false") [add_bool_labels leaf bs]

let make_br_exists n = function
  | [] -> assert false
  | [(bs, t)] -> make_tree ("r"^string_of_int n) [add_bool_labels t bs]
  | (bs, t)::xs ->
    let make_br_exists_aux t1 (bs2, t2) = make_tree "br_exists" [t1; (add_bool_labels t2 bs2)] in
    make_tree ("r"^string_of_int n) [List.fold_left make_br_exists_aux (add_bool_labels t bs) xs]

let make_br_exists n xs =
  Format.printf "#rand_int%d:@." n;
  List.iter (fun (bs,_) ->
    Format.printf "  %s@." (List.fold_left (fun s b -> if b then "0"^s else "1"^s) "" bs)
  ) xs;
  make_br_exists n xs



let rec get_fv = function
  | Const _ -> StringSet.empty
  | Var x -> StringSet.singleton x
  | App(t1, t2) -> StringSet.union (get_fv t1) (get_fv t2)
  | Let(x,t1,t2) -> StringSet.union (get_fv t1) (StringSet.remove x @@ get_fv t2)
  | Fun(x,_,t) -> StringSet.remove x (get_fv t)
let get_fv t = StringSet.elements @@ get_fv t


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




let is_parameter x = String.starts_with x Flag.extpar_header
let isEX_COEFFS id = Str.string_match (Str.regexp ".*COEFFICIENT.*") id 0


let get_ext_funs {env; defs} =
  env
  |> List.filter_out (fun (f,_) -> List.exists (fun (g,_,_,_,_) -> f = g) defs)
  |> List.map fst

let get_ext_fun_env prog =
  let ext_funs = get_ext_funs prog in
  List.map (fun f -> f, List.assoc f prog.env) ext_funs


let rec size t =
  match t with
  | Const _ -> 1
  | Var _ -> 1
  | App(t1, t2) -> 1 + size t1 + size t2
  | Let _ -> unsupported "CEGAR_syntax.size Let"
  | Fun(_,_,t) -> 1 + size t

let prog_size prog =
  List.fold_left (fun sum (f,xs,t1,e,t2) -> sum + List.length xs + size t1 + size t2) 0 prog.defs
