
open Util
open CEGAR_type
open CEGAR_const

let new_id s = Id.to_string (Id.new_var s Type.TUnknown)

type var = string

type t =
    Const of CEGAR_const.t
  | Var of var
  | App of t * t

type fun_def = var * var list * t * t

type prog = (var * t CEGAR_type.t ) list * fun_def list * var


let make_fun xs t =
  let f = new_id "f" in
    [f, xs, Const True, t], f
let make_if t1 t2 t3 =
  let f = new_id "f" in
  let x = new_id "b" in
  let def1 = f, [x], Var x, t2 in
  let def2 = f, [x], App(Const Not, Var x), t3 in
    [def1;def2], App(Var f, t1)
let make_temp_if t1 t2 t3 = App(App(App(Const If,t1),t2),t3)
let make_not t = App(Const Not, t)
let make_and t1 t2 = App(App(Const And, t1), t2)
let make_or t1 t2 = App(App(Const Or, t1), t2)
let make_eq t1 t2 = App(App(Const Eq, t1), t2)
let loop_var = "loop"
let loop_def = loop_var, ["u"], Const True, App(Var loop_var, Const Unit)
let loop_term = App(Var loop_var, Const Unit)




let rec make_arg_let t =
  let desc =
    match t.Syntax.desc with
        Syntax.Unit -> Syntax.Unit
      | Syntax.True -> Syntax.True
      | Syntax.False -> Syntax.False
      | Syntax.Unknown -> assert false
      | Syntax.Int n -> Syntax.Int n
      | Syntax.Var x -> Syntax.Var x
      | Syntax.App(t, ts) ->
          let f = Id.new_var "f" (t.Syntax.typ) in
          let xts = List.map (fun t -> Id.new_var "x" (t.Syntax.typ), t) ts in
          let t' = {Syntax.desc=Syntax.App(Syntax.make_var f, List.map (fun (x,_) -> Syntax.make_var x) xts); Syntax.typ=Type.TUnknown} in
            (List.fold_left (fun t2 (x,t1) -> {Syntax.desc=Syntax.Let(Flag.Nonrecursive,x,[],t1,t2);Syntax.typ=t2.Syntax.typ}) t' ((f,t)::xts)).Syntax.desc
      | Syntax.If(t1, t2, t3) ->
          let t1' = make_arg_let t1 in
          let t2' = make_arg_let t2 in
          let t3' = make_arg_let t3 in
            Syntax.If(t1',t2',t3')
      | Syntax.Branch(t1, t2) -> assert false
      | Syntax.Let(flag,f,xs,t1,t2) -> 
          let t1' = make_arg_let t1 in
          let t2' = make_arg_let t2 in
            Syntax.Let(flag,f,xs,t1',t2')
      | Syntax.BinOp(op, t1, t2) ->
          let t1' = make_arg_let t1 in
          let t2' = make_arg_let t2 in
            Syntax.BinOp(op, t1', t2')
      | Syntax.Not t -> Syntax.Not (make_arg_let t)
      | Syntax.Fail -> Syntax.Fail
      | Syntax.Fun(x,t) -> assert false
      | Syntax.Event s -> assert false
  in
    {Syntax.desc=desc; Syntax.typ=t.Syntax.typ}




let trans_var x = Id.to_string x

let rec trans_typ = function
    Type.TUnit -> TBase(TUnit, fun _ -> [])
  | Type.TBool -> TBase(TBool, fun x -> [x])
  | Type.TAbsBool -> assert false
  | Type.TInt _ -> TBase(TInt, fun _ -> [])
  | Type.TRInt _  -> assert false
  | Type.TVar _  -> assert false
  | Type.TFun(x,typ) -> TFun(fun _ -> trans_typ (Id.typ x), trans_typ typ)
  | Type.TList _ -> assert false
  | Type.TConstr _ -> assert false
  | Type.TVariant _ -> assert false
  | Type.TRecord _ -> assert false
  | Type.TUnknown -> assert false
  | Type.TBottom -> assert false

let rec trans_binop = function
    Syntax.Eq -> Const Eq
  | Syntax.Lt -> Const Lt
  | Syntax.Gt -> Const Gt
  | Syntax.Leq -> Const Leq
  | Syntax.Geq -> Const Geq
  | Syntax.And -> Const And
  | Syntax.Or -> Const Or
  | Syntax.Add -> Const Add
  | Syntax.Sub -> Const Sub
  | Syntax.Mult -> Const Mul

let rec trans_term t =
  match t.Syntax.desc with
    Syntax.Unit -> [], Const Unit
  | Syntax.True -> [], Const True
  | Syntax.False -> [], Const False
  | Syntax.Unknown -> assert false
  | Syntax.Int n -> [], Const (Int n)
  | Syntax.Var x -> [], Var (Id.to_string x)
  | Syntax.App(t, ts) ->
      let defs,t' = trans_term t in
      let defss,ts' = List.split (List.map trans_term ts) in
        defs @ (List.flatten defss), List.fold_left (fun t1 t2 -> App(t1,t2)) t' ts'
  | Syntax.If(t1, t2, t3) ->
      let defs1,t1' = trans_term t1 in
      let defs2,t2' = trans_term t2 in
      let defs3,t3' = trans_term t3 in
      let defs,t = make_if t1' t2' t3' in
        defs1@defs2@defs3@defs, t
  | Syntax.Let _ -> assert false
  | Syntax.BinOp(op, t1, t2) ->
      let defs1,t1' = trans_term t1 in
      let defs2,t2' = trans_term t2 in
        defs1@defs2, App((App(trans_binop op, t1'), t2'))
  | Syntax.Not t ->
      let defs,t' = trans_term t in
        defs, App(Const Not, t')
  | Syntax.Fail -> [], Const Fail
  | Syntax.Fun _
  | Syntax.Event _ -> assert false

let trans_prog t =
  let t' = make_arg_let t in
  let defs,_ = Syntax.lift t' in
  let main,_ = Util.last defs in
  let aux (f,(xs,t)) =
    let defs,t' = trans_term t in
      (trans_var f, List.map trans_var xs, Const True, t')::defs
  in
  let env = List.map (fun (f,_) -> trans_var f, trans_typ (Id.typ f)) defs in
  let defs' = Util.rev_map_flatten aux defs in
    env, defs', trans_var main



let rec get_fv = function
    Const _ -> []
  | Var x -> [x]
  | App(t1, t2) -> get_fv t1 @@@ get_fv t2



let rec get_const_typ = function
    Fail -> assert false
  | Event _ -> TBase(TUnit, fun _ -> [])
  | Label _ -> assert false
  | Unit _ -> TBase(TUnit, fun _ -> [])
  | True _ -> TBase(TBool, fun x -> [x])
  | False _ -> TBase(TBool, fun x -> [make_not x])
  | Unknown _ -> TBase(TBool, fun _ -> [])
  | And -> assert false
  | Or -> assert false
  | Not -> assert false
  | Lt -> assert false
  | Gt -> assert false
  | Leq -> assert false
  | Geq -> assert false
  | Eq -> assert false
  | Int n -> TBase(TInt, fun x -> [make_eq x (Const (Int n))])
  | Add -> assert false
  | Sub -> assert false
  | Mul -> assert false
  | Tuple _ -> assert false
  | Proj _ -> assert false
  | If _ -> assert false


let rec get_typ env = function
    Const c -> get_const_typ c
  | Var x -> List.assoc x env
  | App(t1,t2) ->
      match get_typ env t1 with
          TFun typ -> snd (typ t1)
        | _ -> assert false

            

    
let rec decomp_app = function
    App(t1,t2) ->
      let t,ts = decomp_app t1 in
        t, t2::ts
  | t -> t, []



let rec subst x t = function
    Const c -> Const c
  | Var y when x = y -> t
  | Var y -> Var y
  | App(t1,t2) -> App(subst x t t1, subst x t t2)
