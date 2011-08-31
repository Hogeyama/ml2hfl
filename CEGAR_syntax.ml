
open Utilities
open CEGAR_type

let new_id s = Id.to_string (Id.new_var s Type.TUnknown)
let rename_id s =
  let len = String.length s in
  let n =
    try
      let i = String.rindex s '_' in
      let n = String.sub s (i+1) (len-i-1) in
      let _ = int_of_string n in
        i
    with _ -> len
  in
    String.sub s 0 n ^ "_" ^ string_of_int (Id.new_int ())
let decomp_id s =
  try
    let len = String.length s in
    let i = String.rindex s '_' in
      String.sub s 0 i, int_of_string (String.sub s (i+1) (len-i-1))
  with _ -> s, 0
let add_name x s =
  let name,n = decomp_id x in
    name ^ s ^ if n <> 0 then string_of_int n else ""


type var = string


type const =
    Event of string
  | Label of int (* for abstraction *)
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



type t =
    Const of const
  | Var of var
  | App of t * t
  | Let of var * t * t
  | Fun of var * t


type fun_def = var * var list * t * t
type typ = t CEGAR_type.t
type env = (var * typ) list
type prog = env * fun_def list * var




let make_app t ts = List.fold_left (fun t1 t2 -> App(t1, t2)) t ts
let make_fun xs t =
  let f = new_id "f" in
    [f, xs, Const True, t], f
let make_fun_temp xs t = List.fold_right (fun x t -> Fun(x,t)) xs t
let make_if t1 t2 t3 =
  match t1 with
      Const True -> t2
    | Const False -> t3
    | _ -> make_app (Const If) [t1;t2;t3]
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
let make_loop () =
  let f = new_id "loop" in
    [f, ["u"], Const True, App(Var f, Const Unit)], App(Var f, Const Unit)

let typ_unit = TBase(TUnit, fun _ -> [])
let typ_bool = TBase(TBool, fun x -> [x])
let typ_int = TBase(TInt, fun _ -> [])
let make_tfun typ1 typ2 = TFun(fun _ -> typ1, typ2)


let rec get_fv = function
    Const _ -> []
  | Var x -> [x]
  | App(t1, t2) -> get_fv t1 @@@ get_fv t2
  | Let(x,t1,t2) -> diff (get_fv t1) [x] @@@ get_fv t2
  | Fun(x,t) -> diff (get_fv t) [x]



let rec decomp_app = function
    App(t1,t2) ->
      let t,ts = decomp_app t1 in
        t, ts@[t2]
  | t -> t, []
let rec decomp_fun = function
    Fun(x,t) ->
      let xs,t = decomp_fun t in
        x::xs, t
  | t -> [], t
