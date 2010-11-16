
open Util
open Format

type typ =
    TUnit
  | TBool
  | TAbsBool
  | TInt of t list
  | TRInt of t
  | TVar of typ option ref
  | TFun of (ident*typ) * typ
  | TUnknown

and ident = {id:int; origin:string; typ:typ}
and label = Read | Write | Close
and binop = Eq | Lt | Gt | Leq | Geq | And | Or | Add | Sub | Mult

and t =
    Unit
  | True
  | False
  | Unknown
  | Int of int
  | NInt of ident
  | Var of ident
  | Fun of ident * t
  | App of t * t list
  | If of t * t * t * t
  | Branch of t * t
  | Let of ident * ident list * t * t
  | Letrec of ident * ident list * t * t
  | BinOp of binop * t * t
  | Not of t
  | Fail
  | Label of bool * t
  | Event of string

type syntax = ML | TRecS | CVC3 | CSIsat

type node = BrNode | LabNode of bool | FailNode


exception Feasible of t
exception Infeasible


type literal = Cond of t | Pred of (ident * int * ident * t list)






let dummy_var = {id=0; origin=""; typ=TInt[]}
let abst_var = {id=0; origin="v"; typ=TInt[]}


let counter = ref 1

let new_int () = incr counter; !counter
let get_counter () = !counter
let set_counter n = counter := n


let new_var s = {id= -1; origin=s; typ=TUnknown}
let new_var' s = {id=new_int (); origin=s; typ=TUnknown}
let new_var_id x = {x with id=new_int ()}

let rec trans = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Int n -> Int n
  | NInt x -> NInt x
  | Var x -> Var x
  | Fun(x, t) ->
      let rec aux xs = function
          Fun(x,t) ->
            aux (x::xs) t
        | t -> List.rev xs, t
      in
      let xs', t = aux [] (Fun(x,t)) in
      let f = new_var' "f" in
      let t' = trans t in
        Let(f, xs', t', Var f)
  | App(t, ts) ->
      let t' = trans t in
      let ts' = List.map trans ts in
        App(t', ts')
  | If(t1, t2, t3, t4) ->
      let t1' = trans t1 in
      let t2' = trans t2 in
      let t3' = trans t3 in
      let t4' = trans t4 in
        If(t1', t2', t3', t4')
  | Branch(t1, t2) ->
      let t1' = trans t1 in
      let t2' = trans t2 in
        Branch(t1', t2')
  | Let(f, xs, t1, t2) ->
      let t1' = trans t1 in
      let t2' = trans t2 in
        Let(f, xs, t1', t2')
  | Letrec(f, xs, t1, t2) ->
      let t1' = trans t1 in
      let t2' = trans t2 in
        Letrec(f, xs, t1', t2')
  | BinOp(op, t1, t2) ->
      let t1' = trans t1 in
      let t2' = trans t2 in
        BinOp(op, t1', t2')
  | Not(t) ->
      let t' = trans t in
        Not(t')
  | Fail -> Fail
  | Unknown -> Unknown
  | Label(b,t) ->
      let t' = trans t in
        Label(b,t')
  | Event s -> Event s









(* [x |-> t], [t/x] *)
let rec subst x t = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt y ->
      if x.id = y.id then t else NInt y
  | Var y ->
      if x.id = y.id then t else Var y
  | Fun(y, t1) ->
      let t1' = if x.id = y.id then t1 else subst x t t1 in
        Fun(y, t1')
  | App(t1, ts) ->
      let t1' = subst x t t1 in
      let ts' = List.map (subst x t) ts in
        begin
          match t1' with
              App(t, ts) -> App(t, ts@ts')
            | _ -> App(t1', ts')
        end
  | If(t1, t2, t3, t4) ->
      let t1' = subst x t t1 in
      let t2' = subst x t t2 in
      let t3' = subst x t t3 in
      let t4' = subst x t t4 in
        If(t1', t2', t3', t4')
  | Branch(t1, t2) ->
      let t1' = subst x t t1 in
      let t2' = subst x t t2 in
        Branch(t1', t2')
  | Let(f, xs, t1, t2) ->
      let t1' = subst x t t1 in
      let t2' = if f.id = x.id then t2 else subst x t t2 in
        Let(f, xs, t1', t2')
  | Letrec(f, xs, t1, t2) ->
      let t1' = if f.id = x.id then t1 else subst x t t1 in
      let t2' = if f.id = x.id then t2 else subst x t t2 in
        Letrec(f, xs, t1', t2')
  | BinOp(op, t1, t2) ->
      let t1' = subst x t t1 in
      let t2' = subst x t t2 in
        BinOp(op, t1', t2')
  | Not t1 ->
      let t1' = subst x t t1 in
        Not t1'
  | Fail -> Fail
  | Label(b, t1) ->
      let t1' = subst x t t1 in
        Label(b, t1')

let rec subst_int n t = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int m -> if n = m then t else BinOp(Add, t, Int (m - n))
  | NInt y -> NInt y
  | Var y -> Var y
  | Fun(y, t1) -> Fun(y, subst_int n t t1)
  | App(t1, ts) ->
      let t1' = subst_int n t t1 in
      let ts' = List.map (subst_int n t) ts in
        begin
          match t1' with
              App(t, ts) -> App(t, ts@ts')
            | _ -> App(t1', ts')
        end
  | If(t1, t2, t3, t4) ->
      let t1' = subst_int n t t1 in
      let t2' = subst_int n t t2 in
      let t3' = subst_int n t t3 in
      let t4' = subst_int n t t4 in
        If(t1', t2', t3', t4')
  | Branch(t1, t2) ->
      let t1' = subst_int n t t1 in
      let t2' = subst_int n t t2 in
        Branch(t1', t2')
  | Let(f, xs, t1, t2) ->
      let t1' = subst_int n t t1 in
      let t2' = subst_int n t t2 in
        Let(f, xs, t1', t2')
  | Letrec(f, xs, t1, t2) ->
      let t1' = subst_int n t t1 in
      let t2' = subst_int n t t2 in
        Letrec(f, xs, t1', t2')
  | BinOp(Mult, t1, t2) -> (* non-linear expressions not supported *)
      BinOp(Mult, t1, t2)
  | BinOp(op, t1, t2) ->
      let t1' = subst_int n t t1 in
      let t2' = subst_int n t t2 in
        BinOp(op, t1', t2')
  | Not t1 ->
      let t1' = subst_int n t t1 in
        Not t1'
  | Fail -> Fail
  | Label(b, t1) ->
      let t1' = subst_int n t t1 in
        Label(b, t1')

let subst_term sub term =
  let ids, terms = List.split sub in
  List.fold_right2 subst ids terms term

let rec subst_type x t = function
    TUnit -> TUnit
  | TAbsBool -> TAbsBool
  | TBool -> TBool
  | TInt ts -> TInt (List.map (subst x t) ts)
  | TRInt t' -> TRInt (subst x t t')
  | TVar _ -> assert false
  | TFun((y,typ1),typ2) ->
      let typ1' = subst_type x t typ1 in
      let typ2' = subst_type x t typ2 in
      let y' = {y with typ = typ1'} in
        TFun((y',typ1'),typ2')
  | TUnknown -> TUnknown

let rec subst_orig x t = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt y ->
      if x.origin = y.origin then t else NInt y
  | Var y ->
      if x.origin = y.origin then t else Var y
  | Fun(y, t1) ->
      let t1' = if x.origin = y.origin then t1 else subst_orig x t t1 in
        Fun(y, t1')
  | App(t1, ts) ->
      let t1' = subst_orig x t t1 in
      let ts' = List.map (subst_orig x t) ts in
        begin
          match t1' with
              App(t, ts) -> App(t, ts@ts')
            | _ -> App(t1', ts')
        end
  | If(t1, t2, t3, t4) ->
      let t1' = subst_orig x t t1 in
      let t2' = subst_orig x t t2 in
      let t3' = subst_orig x t t3 in
      let t4' = subst_orig x t t4 in
        If(t1', t2', t3', t4')
  | Branch(t1, t2) ->
      let t1' = subst_orig x t t1 in
      let t2' = subst_orig x t t2 in
        Branch(t1', t2')
  | Let(f, xs, t1, t2) ->
      let t1' = subst_orig x t t1 in
      let t2' = if f.origin = x.origin then t2 else subst_orig x t t2 in
        Let(f, xs, t1', t2')
  | Letrec(f, xs, t1, t2) ->
      let t1' = if f.origin = x.origin then t1 else subst_orig x t t1 in
      let t2' = if f.origin = x.origin then t2 else subst_orig x t t2 in
        Letrec(f, xs, t1', t2')
  | BinOp(op, t1, t2) ->
      let t1' = subst_orig x t t1 in
      let t2' = subst_orig x t t2 in
        BinOp(op, t1', t2')
  | Not t1 ->
      let t1' = subst_orig x t t1 in
        Not t1'
  | Fail -> Fail
  | Label(b, t1) ->
      let t1' = subst_orig x t t1 in
        Label(b, t1')

let rec subst_type_orig x t = function
    TUnit -> TUnit
  | TAbsBool -> TAbsBool
  | TBool -> TBool
  | TInt ts -> TInt (List.map (subst_orig x t) ts)
  | TRInt t' -> TRInt (subst_orig x t t')
  | TVar _ -> assert false
  | TFun((y,typ1),typ2) ->
      let typ1' = subst_type_orig x t typ1 in
      let typ2' = subst_type_orig x t typ2 in
      let y' = {y with typ = typ1'} in
        TFun((y',typ1'),typ2')
  | TUnknown -> TUnknown

let rec fff = function
    TUnit -> TUnit
  | TAbsBool -> TAbsBool
  | TBool -> TBool
  | TInt ts -> TInt ts
  | TRInt t -> TRInt t
  | TVar _ -> assert false
  | TFun((x,typ1),typ2) ->
      let x' = new_var_id x in
      TFun(({x' with typ = typ1}, typ1), subst_type_orig x (Var x') typ2)
  | TUnknown -> TUnknown




let rec get_nint = function
    Unit -> []
  | True -> []
  | False -> []
  | Unknown -> []
  | Int n -> []
  | NInt x -> [x]
  | Var x -> []
  | App(t, ts) -> get_nint t @@@ (rev_map_flatten get_nint ts)
  | If(t1, t2, t3, t4) -> get_nint t1 @@@ get_nint t2 @@@ get_nint t3 @@@ get_nint t4
  | Branch(t1, t2) -> get_nint t1 @@@ get_nint t2
  | Let(f, xs, t1, t2) ->
      let fv_t1 = diff (get_nint t1) xs in
      let fv_t2 = diff (get_nint t2) [f] in
        fv_t1 @@@ fv_t2
  | Letrec(f, xs, t1, t2) ->
      let fv_t1 = diff (get_nint t1) (f::xs) in
      let fv_t2 = diff (get_nint t2) [f] in
        fv_t1 @@@ fv_t2
  | BinOp(op, t1, t2) -> get_nint t1 @@@ get_nint t2
  | Not t -> get_nint t
  | Fail -> []
  | Fun(x,t) -> diff (get_nint t) [x]
  | Label(_,t) -> get_nint t

let rec get_int = function
    Unit -> []
  | True -> []
  | False -> []
  | Unknown -> []
  | Int n -> [n]
  | NInt x -> []
  | Var x -> []
  | App(t, ts) -> get_int t @@@ (rev_map_flatten get_int ts)
  | If(t1, t2, t3, t4) -> get_int t1 @@@ get_int t2 @@@ get_int t3 @@@ get_int t4
  | Branch(t1, t2) -> get_int t1 @@@ get_int t2
  | Let(_, _, t1, t2) ->
      (get_int t1) @@@ (get_int t2)
  | Letrec(_, _, t1, t2) ->
      (get_int t1) @@@ (get_int t2)
  | BinOp(Mult, t1, t2) -> [] (* non-linear expressions not supported *)
  | BinOp(_, t1, t2) -> get_int t1 @@@ get_int t2
  | Not t -> get_int t
  | Fail -> []
  | Fun(_,t) -> get_int t
  | Label(_,t) -> get_int t

let rec get_fv = function
    Unit -> []
  | True -> []
  | False -> []
  | Unknown -> []
  | Int n -> []
  | NInt x -> []
  | Var x -> [x]
  | App(t, ts) -> get_fv t @@@ (rev_map_flatten get_fv ts)
  | If(t1, t2, t3, t4) -> get_fv t1 @@@ get_fv t2 @@@ get_fv t3 @@@ get_fv t4
  | Branch(t1, t2) -> get_fv t1 @@@ get_fv t2
  | Let(f, xs, t1, t2) ->
      let fv_t1 = diff (get_fv t1) xs in
      let fv_t2 = diff (get_fv t2) [f] in
        fv_t1 @@@ fv_t2
  | Letrec(f, xs, t1, t2) ->
      let fv_t1 = diff (get_fv t1) (f::xs) in
      let fv_t2 = diff (get_fv t2) [f] in
        fv_t1 @@@ fv_t2
  | BinOp(op, t1, t2) -> get_fv t1 @@@ get_fv t2
  | Not t -> get_fv t
  | Fail -> []
  | Fun(x,t) -> diff (get_fv t) [x]
  | Label(_,t) -> get_fv t


let rec get_fv2 = function
    Unit -> []
  | True -> []
  | False -> []
  | Unknown -> []
  | Int n -> []
  | NInt x -> [x]
  | Var x -> [x]
  | App(t, ts) -> get_fv2 t @@@ (rev_map_flatten get_fv2 ts)
  | If(t1, t2, t3, t4) -> get_fv2 t1 @@@ get_fv2 t2 @@@ get_fv2 t3 @@@ get_fv2 t4
  | Branch(t1, t2) -> get_fv2 t1 @@@ get_fv2 t2
  | Let(f, xs, t1, t2) ->
      let fv_t1 = diff (get_fv2 t1) xs in
      let fv_t2 = diff (get_fv2 t2) [f] in
        fv_t1 @@@ fv_t2
  | Letrec(f, xs, t1, t2) ->
      let fv_t1 = diff (get_fv2 t1) (f::xs) in
      let fv_t2 = diff (get_fv2 t2) [f] in
        fv_t1 @@@ fv_t2
  | BinOp(op, t1, t2) -> get_fv2 t1 @@@ get_fv2 t2
  | Not t -> get_fv2 t
  | Fail -> []
  | Fun(x,t) -> diff (get_fv2 t) [x]
  | Label(_,t) -> get_fv2 t






let rec get_args = function
    TFun((x,typ1),typ2) -> x :: get_args typ2
  | _ -> []

let rec get_argvars = function
    TFun((x,typ1),typ2) -> x :: get_argvars typ1 @ get_argvars typ2
  | _ -> []

let app2app t ts =
  match t,ts with
      _,[] -> t
    | Var x,_ -> App(Var x, ts)
    | App(t', ts'),_ -> App(t', ts'@ts)
    | _ -> App(t, ts)






(*let rec safe = function
    Unit
  | True
  | False
  | Unknown
  | Int _
  | Var _
  | Fail -> true
  | NInt _ -> false
  | App(t, ts) -> safe t && List.for_all safe ts
  | If(t1, t2, t3, t4) -> safe t1 && safe t2 && safe t3 && safe t4
  | Branch(t1, t2) -> safe t1 && safe t2
  | Let(_, _, t1, t2) -> safe t1 && safe t2
  | Letrec(_, _, t1, t2) -> safe t1 && safe t2
  | BinOp(_, t1, t2) -> safe t1 && safe t2
  | Not t -> safe t
  | Fun(_, t) -> safe t
  | Label(_, t) -> safe t*)

let rec eval = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | Var x -> Var x
  | App(Fun(x, t), t'::ts) ->
      (match t' with
        NInt _ ->
          App(Fun(x, eval t), List.map eval (t'::ts))
      | _ ->
          eval (App(subst_term [x, t'] t, ts)))
  | App(t, []) -> eval t
  | App(t, ts) ->
      App(eval t, List.map eval ts)
  | If(True, t2, t3, t4) ->
      eval t2
  | If(False, t2, t3, t4) ->
      eval t3
  | If(t1, t2, t3, t4) ->
      If(eval t1, eval t2, eval t3, eval t4)
  | Branch(t1, t2) ->
      Branch(eval t1, eval t2)
  | Let(f, xs, t1, t2) -> (*assume that evaluation of t1 does not fail*)
      if (*safe t1*)true then
        let t1' = List.fold_right (fun x t -> Fun(x, t)) xs (eval t1) in
        eval (subst_term [f, t1'] t2)
      else
        Let(f, xs, eval t1, eval t2)
  | Letrec(f, xs, t1, t2) ->
      (*if not (List.mem f (get_fv t1)) then
        let t1' = List.fold_right (fun x t -> Fun(x, t)) xs (eval t1) in
        eval (subst_term [f, t1'] t2)
      else*)
        Letrec(f, xs, eval t1, eval t2)
  | BinOp(Add, Int 0, t) ->
      eval t
  | BinOp(Mult, Int 1, t) ->
      eval t
  | BinOp(Sub, t1, t2) ->
      eval (BinOp(Add, eval t1, eval (BinOp(Mult, Int(-1), t2))))
  | BinOp(Mult, Int n, BinOp(Mult, Int m, t)) ->
      eval (BinOp(Mult, Int(n * m), t))
  | BinOp(op, t1, t2) ->
      BinOp(op, eval t1, eval t2)
  | Not t ->
      Not(eval t)
  | Fail ->
      Fail
  | Fun(x,App(t,ts)) ->
      let t' = eval t in
      let ts' = List.map eval ts in
      if ts' <> [] then
        let l, r = Utilities.list_last_and_rest ts' in
        if l = Var x && List.for_all (fun t -> not (List.mem x (get_fv t))) (t'::r) then
          eval (App(t', r))
        else
          Fun(x,App(t', ts'))
      else
        Fun(x,App(t', ts'))
  | Fun(x,t) ->
      Fun(x, eval t)
  | Label(b,t) ->
      Label(b, eval t)


(*let rec eta = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | Var x -> Var x
  | App(t, ts) ->
      App(eta t, List.map eta ts)
  | If(t1, t2, t3, t4) ->
      If(eta t1, eta t2, eta t3, eta t4)
  | Branch(t1, t2) ->
      Branch(eta t1, eta t2)
  | Let(f, xs, t1, t2) ->
      Let(f, xs, eta t1, eta t2)
  | Letrec(f, xs, t1, t2) ->
      Letrec(f, xs, eta t1, eta t2)
  | BinOp(op, t1, t2) ->
      BinOp(op, eta t1, eta t2)
  | Not t ->
      Not(eta t)
  | Fail ->
      Fail
  | Fun(x,t) ->
      Fun(x, eta t)
  | Label(b,t) ->
      Label(b, eta t)*)



let rec simplify = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | Var x -> Var x
  | App(t, []) ->
      simplify t
  | App(App(t, ts1), ts2) ->
      simplify (App(t, ts1@ts2))
  | App(t, ts) ->
      let t' = simplify t in
      let ts' = List.map simplify ts in
        App(t', ts')
  | If(t1, t2, t3, t4) ->
      let t1' = simplify t1 in
      let t2' = simplify t2 in
      let t3' = simplify t3 in
      let t4' = simplify t4 in
        If(t1', t2', t3', t4')
  | Branch(t1, t2) ->
      let t1' = simplify t1 in
      let t2' = simplify t2 in
        Branch(t1', t2')
  | Let(f, xs, Let(g, ys, t1, Var h), t2) when g = h ->
      let t1' = simplify t1 in
      let t2' = simplify t2 in
        Let(f, xs@ys, t1', t2')
  | Letrec(f, xs, Let(g, ys, t1, Var h), t2) when g = h ->
      let t1' = simplify t1 in
      let t2' = simplify t2 in
        Letrec(f, xs@ys, t1', t2')
  | Let(f, xs, t1, t2) ->
      let t1' = simplify t1 in
      let t2' = simplify t2 in
        Let(f, xs, t1', t2')
  | Letrec(f, xs, t1, t2) ->
      let t1' = simplify t1 in
      let t2' = simplify t2 in
        Letrec(f, xs, t1', t2')
  | BinOp(op, t1, t2) ->
      let t1' = simplify t1 in
      let t2' = simplify t2 in
        BinOp(op, t1', t2')
  | Not t ->
      let t' = simplify t in
        Not t'
  | Fail -> Fail
  | Fun(x,t) ->
      let t' = simplify t in
        Fun(x, t')
  | Label(b,t) ->
      let t' = simplify t in
        Label(b, t')

let imply t1 t2 = BinOp(Or, Not t1, t2)
let and_list ts = match ts with
  [] -> True
| [t] -> t
| t::ts -> List.fold_left (fun t1 t2 -> BinOp(And, t1, t2)) t ts

let rec lift_aux xs = function
    Unit -> [], Unit
  | True -> [], True
  | False -> [], False
  | Unknown -> [], Unknown
  | Int n -> [], Int n
  | NInt x -> [], NInt x
  | Var x -> [], Var x
  | Fun(x,t) -> (*assert false*) (*assume that t has the type unit*)
      let rec aux xs = function
          Fun(x,t) ->
            aux (x::xs) t
        | t -> List.rev xs, t
      in
      let xs', t = aux [] (Fun(x,t)) in
      let f = new_var' "f" in
      let typ = List.fold_right (fun x typ -> TFun((x,x.typ),typ)) xs' TUnit in
      let f = {f with typ = typ} in
      let t' = Let(f, xs', t, Var f) in
        lift_aux xs t'
  | App(t, ts) ->
      let defs,t' = lift_aux xs t in
      let defss,ts' = List.split (List.map (lift_aux xs) ts) in
        defs @ (List.flatten defss), App(t', ts')
  | If(t1,t2,t3,t4) ->
      let defs1,t1' = lift_aux xs t1 in
      let defs2,t2' = lift_aux xs t2 in
      let defs3,t3' = lift_aux xs t3 in
      let defs4,t4' = lift_aux xs t4 in
        defs1 @ defs2 @ defs3 @ defs4, If(t1',t2',t3',t4')
  | Branch(t1,t2) ->
      let defs1,t1' = lift_aux xs t1 in
      let defs2,t2' = lift_aux xs t2 in
        defs1 @ defs2, Branch(t1',t2')
  | Let(f,ys,t1,t2) ->
      let fv = inter xs (diff (get_fv(*get_fv2*) t1) ys) in
      let ys' = fv @ ys in
      let typ = List.fold_right (fun x typ -> TFun((x,x.typ),typ)) ys' TUnit in
      let f' = {f with typ=typ} in
      let f'' = List.fold_left (fun t x -> app2app t [Var x]) (Var f') fv in
      let defs1,t1' = lift_aux ys' t1 in
      let defs2,t2' = lift_aux xs (subst f f'' t2) in
        defs1 @ [(f',(ys',t1'))] @ defs2, t2'
  | Letrec(f,ys,t1,t2) ->
      let fv = inter xs (diff (get_fv(*get_fv2*) t1) (f::ys)) in
      let ys' = fv @ ys in
      let typ = List.fold_right (fun x typ -> TFun((x,x.typ),typ)) ys TUnit in
      let f' = {f with typ=typ} in
      let f'' = List.fold_left (fun t x -> app2app t [Var x]) (Var f') fv in
      let defs1,t1' = lift_aux ys' (subst f f'' t1) in
      let defs2,t2' = lift_aux xs (subst f f'' t2) in
        defs1 @ [(f',(ys',t1'))] @ defs2, t2'
  | BinOp(op,t1,t2) ->
      let defs1,t1' = lift_aux xs t1 in
      let defs2,t2' = lift_aux xs t2 in
        defs1 @ defs2, BinOp(op,t1',t2')
  | Not t ->
      let defs,t' = lift_aux xs t in
        defs, Not t'
  | Fail -> [], Fail
  | Label(b,t) ->
      let defs,t' = lift_aux xs t in
        defs, Label(b,t')
let lift t = lift_aux [](*(get_fv2 t)*) t

let rec lift2_aux xs = function
    Unit -> [], Unit
  | True -> [], True
  | False -> [], False
  | Unknown -> [], Unknown
  | Int n -> [], Int n
  | NInt x -> [], NInt x
  | Var x -> [], Var x
  | Fun(x,t) -> (*assert false*) (*assume that t has the type unit*)
      let rec aux xs = function
          Fun(x,t) ->
            aux (x::xs) t
        | t -> List.rev xs, t
      in
      let xs', t = aux [] (Fun(x,t)) in
      let f = new_var' "f" in
      let typ = List.fold_right (fun x typ -> TFun((x,x.typ),typ)) xs' TUnit in
      let f = {f with typ = typ} in
      let t' = Let(f, xs', t, Var f) in
        lift2_aux xs t'
  | App(t, ts) ->
      let defs,t' = lift2_aux xs t in
      let defss,ts' = List.split (List.map (lift2_aux xs) ts) in
        defs @ (List.flatten defss), App(t', ts')
  | If(t1,t2,t3,t4) ->
      let defs1,t1' = lift2_aux xs t1 in
      let defs2,t2' = lift2_aux xs t2 in
      let defs3,t3' = lift2_aux xs t3 in
      let defs4,t4' = lift2_aux xs t4 in
        defs1 @ defs2 @ defs3 @ defs4, If(t1',t2',t3',t4')
  | Branch(t1,t2) ->
      let defs1,t1' = lift2_aux xs t1 in
      let defs2,t2' = lift2_aux xs t2 in
        defs1 @ defs2, Branch(t1',t2')
  | Let(f,ys,t1,t2) ->
      let fv = xs in
      let ys' = fv @ ys in
      let typ = List.fold_right (fun x typ -> TFun((x,x.typ),typ)) ys' TUnit in
      let f' = {(new_var_id f) with typ=typ} in
      let f'' = List.fold_left (fun t x -> app2app t [Var x]) (Var f') fv in
      let defs1,t1' = lift2_aux ys' t1 in
      let defs2,t2' = lift2_aux xs (subst f f'' t2) in
        defs1 @ [(f',(ys',t1'))] @ defs2, t2'
  | Letrec(f,ys,t1,t2) ->
      let fv = xs in
      let ys' = fv @ ys in
      let typ = List.fold_right (fun x typ -> TFun((x,x.typ),typ)) ys' TUnit in
      let f' = {(new_var_id f) with typ=typ} in
      let f'' = List.fold_left (fun t x -> app2app t [Var x]) (Var f') fv in
      let defs1,t1' = lift2_aux ys' (subst f f'' t1) in
      let defs2,t2' = lift2_aux xs (subst f f'' t2) in
        defs1 @ [(f',(ys',t1'))] @ defs2, t2'
  | BinOp(op,t1,t2) ->
      let defs1,t1' = lift2_aux xs t1 in
      let defs2,t2' = lift2_aux xs t2 in
        defs1 @ defs2, BinOp(op,t1',t2')
  | Not t ->
      let defs,t' = lift2_aux xs t in
        defs, Not t'
  | Fail -> [], Fail
  | Label(b,t) ->
      let defs,t' = lift2_aux xs t in
        defs, Label(b,t')
let lift2 t = lift2_aux [](*(get_fv2 t)*) t

let rec canonize = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | Var x -> Var x
  | App(t, ts) ->
      let t' = canonize t in
      let ts' = List.map canonize ts in
        App(t', ts')
  | If(t1, t2, t3, t4) ->
      let t1' = canonize t1 in
      let t2' = canonize t2 in
      let t3' = canonize t3 in
      let t4' = canonize t4 in
        If(t1', t2', t3', t4')
  | Branch(t1, t2) ->
      let t1' = canonize t1 in
      let t2' = canonize t2 in
        Branch(t1', t2')
  | Let(f, xs, t1, t2) ->
      let t1' = canonize t1 in
      let t2' = canonize t2 in
        Let(f, xs, t1', t2')
  | Letrec(f, xs, t1, t2) ->
      let t1' = canonize t1 in
      let t2' = canonize t2 in
        Letrec(f, xs, t1', t2')
  | BinOp(Eq, Not t1, t2)
  | BinOp(Eq, t1, Not t2) ->
      let t1' = canonize t1 in
      let t2' = canonize t2 in
      BinOp(And, BinOp(Or, t1, t2), BinOp(Or, Not t1', Not t2'))
  | BinOp(Eq, BinOp((Eq|Lt|Gt|Leq|Geq|And|Or) as bop, t1, t2), t3) ->
      let t1' = canonize t1 in
      let t2' = canonize t2 in
      let t3' = canonize t3 in
      BinOp(And, BinOp(Or, Not t3', BinOp(bop, t1', t2')), BinOp(Or, t3', Not (BinOp(bop, t1', t2'))))
  | BinOp(Eq, t3, BinOp((Eq|Lt|Gt|Leq|Geq|And|Or) as bop, t1, t2)) ->
      let t1' = canonize t1 in
      let t2' = canonize t2 in
      let t3' = canonize t3 in
      BinOp(And, BinOp(Or, Not t3', BinOp(bop, t1', t2')), BinOp(Or, t3', Not (BinOp(bop, t1', t2'))))
  | BinOp(op, t1, t2) ->
      let t1' = canonize t1 in
      let t2' = canonize t2 in
        BinOp(op, t1', t2')
  | Not t ->
      let t' = canonize t in
        Not t'
  | Fail -> Fail
  | Fun(x,t) ->
      let t' = canonize t in
        Fun(x, t')
  | Label(b,t) ->
      let t' = canonize t in
        Label(b, t')















let part_eval t =
  let is_apply xs = function
  Var x -> xs = [x]
    | App(t, ts) ->
      let rec aux xs ts =
        match xs,ts with
          [], [] -> true
        | x::xs',t::ts' -> Var x = t && aux xs' ts'
        | _ -> false
      in
        aux xs (t::ts)
    | _ -> false
  in
  let is_alias xs = function
  Var x ->
    if xs = []
    then Some x
    else None
    | App(Var f, ts) ->
      let rec aux xs ts =
        match xs,ts with
          [], [] -> true
        | x::xs',t::ts' -> Var x = t && aux xs' ts'
        | _ -> false
      in
        if aux xs ts
        then Some f
        else None
    | _ -> None
  in
  let rec aux apply = function
  Unit -> Unit
    | True -> True
    | False -> False
    | Int n -> Int n
    | NInt x -> NInt x
    | Var x ->
      if List.mem_assoc x apply
      then
        let xs, t1 = List.assoc x apply in
          Let(x, xs, t1, Var x)
      else Var x
    | Fun(x, t) ->
      let t' = aux apply t in
        Fun(x, t')
    | App(Var f, ts) ->
      if List.mem_assoc f apply
      then
        match ts with
          [] ->
            let xs, t1 = List.assoc f apply in
              Let(f, xs, t1, Var f)
        | [t] -> t
        | t::ts' -> App(t, ts')
      else
        let ts' = List.map (aux apply) ts in
          App(Var f, ts')
    | App(Fun(x,t), ts) ->
      if is_apply [x] t
      then
        match ts with
          [] -> Fun(x,t)
        | [t] -> t
        | t::ts' -> App(t, ts')
      else
        begin
          match ts with
            [True|False] ->
              aux apply (subst x (List.hd ts) t)
          | _ ->
            let t' = aux apply t in
            let ts' = List.map (aux apply) ts in
              App(Fun(x,t'), ts')
        end
    | App(t, ts) ->
      let t' = aux apply t in
      let ts' = List.map (aux apply) ts in
        App(t', ts')
    | If(True, t2, _, _) ->
      let t2' = aux apply t2 in
        t2'
    | If(False, _, t3, _) ->
      let t3' = aux apply t3 in
        t3'
    | If(Unknown, _, _, t4) ->
      let t4' = aux apply t4 in
        t4'
    | If(Not t1, t2, t3, t4) ->
      let t1' = aux apply t1 in
      let t2' = aux apply t2 in
      let t3' = aux apply t3 in
      let t4' = aux apply t4 in
        If(t1', t3', t2', t4')
    | If(t1, t2, t3, t4) ->
      if t2 = t3
      then t2
      else
        let t1' = aux apply t1 in
        let t2' = aux apply t2 in
        let t3' = aux apply t3 in
        let t4' = aux apply t4 in
          If(t1', t2', t3', t4')
    | Branch(t1, t2) ->
      let t1' = aux apply t1 in
      let t2' = aux apply t2 in
        Branch(t1', t2')
    | Let(f, xs, t1, t2) ->
      if is_apply xs t1
      then
        let t2' = aux apply t2 in
          aux ((f,(xs,t1))::apply) t2'
      else
        begin
          match is_alias xs t1 with
            None ->  let t1' = aux apply t1 in
                     let t2' = aux apply t2 in
                       Let(f, xs, t1', t2')
          | Some x ->
            let t2' = aux apply t2 in
              subst f (Var x) t2'
        end
    | Letrec(f, xs, t1, t2) ->
      if is_apply xs t1
      then
        let t2' = aux apply t2 in
          aux ((f,(xs,t1))::apply) t2'
      else
        begin
          match is_alias xs t1 with
            Some x when not (List.mem f (get_fv t1)) ->
              let t2' = aux apply t2 in
                subst f (Var x) t2'
          | _ ->
            let t1' = aux apply t1 in
            let t2' = aux apply t2 in
              Letrec(f, xs, t1', t2')
        end
    | BinOp(op, t1, t2) ->
      let t1' = aux apply t1 in
      let t2' = aux apply t2 in
        BinOp(op, t1', t2')
    | Not t ->
      let t' = aux apply t in
        Not t'
    | Fail -> Fail
    | Unknown -> Unknown
    | Label(b, t) ->
      let t' = aux apply t in
        Label(b, t')
  in
  let t' = aux [] t in
  let t'' = simplify t' in
    t''







let part_eval2 t =
  let is_alias xs = function
      Var x ->
        if xs = []
        then Some x
        else None
    | App(Var f, ts) ->
        let rec aux xs ts =
          match xs,ts with
              [], [] -> true
            | x::xs',(Var y)::ts' -> x.id = y.id && aux xs' ts'
            | _ -> false
        in
          if aux xs ts
          then Some f
          else None
    | _ -> None
  in
  let rec aux = function
      Unit -> Unit
    | True -> True
    | False -> False
    | Int n -> Int n
    | NInt x -> NInt x
    | Var x -> Var x
    | Fun(x, t) ->
        let t' = aux t in
          Fun(x, t')
    | App(t, ts) ->
        let t' = aux t in
        let ts' = List.map (aux) ts in
          App(t', ts')
    | If(t1, t2, t3, t4) ->
        let t1' = aux t1 in
        let t2' = aux t2 in
        let t3' = aux t3 in
        let t4' = aux t4 in
          If(t1', t2', t3', t4')
    | Branch(t1, t2) ->
        let t1' = aux t1 in
        let t2' = aux t2 in
          Branch(t1', t2')
    | Let(f, xs, t1, t2) ->
        begin
          match is_alias xs t1 with
              None ->
                let t1' = aux t1 in
                let t2' = aux t2 in
                  Let(f, xs, t1', t2')
            | Some x ->
                aux (subst f (Var x) t2)
        end
    | Letrec(f, xs, t1, t2) ->
        begin
          match is_alias xs t1 with
              None ->
                let t1' = aux t1 in
                let t2' = aux t2 in
                  Letrec(f, xs, t1', t2')
            | Some x ->
                aux (subst f (Var x) t2)
        end
    | BinOp(op, t1, t2) ->
        let t1' = aux t1 in
        let t2' = aux t2 in
          BinOp(op, t1', t2')
    | Not t ->
        let t' = aux t in
          Not t'
    | Fail -> Fail
    | Unknown -> Unknown
    | Label(b, t) ->
        let t' = aux t in
          Label(b, t')
  in
  let t' = aux t in
  let t'' = simplify t' in
    t''






let rec expand t = function
    TFun((x,typ1),typ2) ->
      expand (app2app t [Var x]) typ2
  | _ -> t














let add_string_to_var str x = {x with origin = x.origin ^ str}
let rec add_string str = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt (add_string_to_var str x)
  | Var x -> Var (add_string_to_var str x)
  | App(t, ts) ->
      let t' = add_string str t in
      let ts' = List.map (add_string str) ts in
        App(t', ts')
  | If(t1, t2, t3, t4) ->
      let t1' = add_string str t1 in
      let t2' = add_string str t2 in
      let t3' = add_string str t3 in
      let t4' = add_string str t4 in
        If(t1', t2', t3', t4')
  | Branch(t1, t2) ->
      let t1' = add_string str t1 in
      let t2' = add_string str t2 in
        Branch(t1', t2')
  | Let(f, xs, t1, t2) ->
      let f' = add_string_to_var str f in
      let xs' = List.map (add_string_to_var str) xs in
      let t1' = add_string str t1 in
      let t2' = add_string str t2 in
        Let(f', xs', t1', t2')
  | Letrec(f, xs, t1, t2) ->
      let f' = add_string_to_var str f in
      let xs' = List.map (add_string_to_var str) xs in
      let t1' = add_string str t1 in
      let t2' = add_string str t2 in
        Letrec(f', xs', t1', t2')
  | BinOp(op, t1, t2) ->
      let t1' = add_string str t1 in
      let t2' = add_string str t2 in
        BinOp(op, t1', t2')
  | Not t ->
      let t' = add_string str t in
        Not t'
  | Fail -> Fail
  | Fun(x,t) ->
      let x' = add_string_to_var str x in
      let t' = add_string str t in
        Fun(x', t')
  | Label(b,t) ->
      let t' = add_string str t in
        Label(b, t')







let rec remove_unused = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | Var x -> Var x
  | Fun _ -> assert false
  | App(t, ts) ->
      let t' = remove_unused t in
      let ts' = List.map remove_unused ts in
        App(t', ts')
  | If(t1, t2, t3, t4) ->
      let t1' = remove_unused t1 in
      let t2' = remove_unused t2 in
      let t3' = remove_unused t3 in
      let t4' = remove_unused t4 in
        If(t1', t2', t3', t4')
  | Branch(t1, t2) ->
      let t1' = remove_unused t1 in
      let t2' = remove_unused t2 in
        Branch(t1', t2')
  | Let(f, xs, t1, t2) ->
      if List.mem f (get_fv t2)
      then
        let t1' = remove_unused t1 in
        let t2' = remove_unused t2 in
          Let(f, xs, t1', t2')
      else
        remove_unused t2
  | Letrec(f, xs, t1, t2) ->
      if List.mem f (get_fv t2)
      then
        let t1' = remove_unused t1 in
        let t2' = remove_unused t2 in
          Letrec(f, xs, t1', t2')
      else
        remove_unused t2
  | BinOp(op, t1, t2) ->
      let t1' = remove_unused t1 in
      let t2' = remove_unused t2 in
        BinOp(op, t1', t2')
  | Not t ->
      let t' = remove_unused t in
        Not t'
  | Fail -> Fail
  | Label _ -> assert false








let rec occurs x = function
      TUnit
    | TBool
    | TAbsBool
    | TUnknown
    | TVar _ -> false
    | TInt ts -> List.exists (fun y -> x.id = y.id) (List.concat (List.map get_fv ts))
    | TRInt t -> List.exists (fun y -> x.id = y.id) (get_fv t)
    | TFun((_,t1),t2) -> occurs x t1 || occurs x t2

let rec print_typ syntax fm = function
    TUnit -> fprintf fm "unit"
  | TAbsBool -> fprintf fm "abool"
  | TBool -> fprintf fm "bool"
  | TInt ps ->
      if ps = []
      then fprintf fm "int"
      else fprintf fm "int[%a]" print_preds ps
  | TRInt p ->
      fprintf fm "{ %a | %a }" print_id abst_var print_preds [p]
  | TVar _ -> fprintf fm "!!!"
  | TFun((x,typ1), typ2) ->
      assert (not Flag.check_fun_arg_typ || x.typ = typ1);
      if (match x.typ with TInt _ -> true | TRInt _ -> true | _ -> false) (*&& occurs x typ2*)
      then fprintf fm "(%a -> %a)" print_id_typ x (print_typ syntax) typ2
      else fprintf fm "(%a -> %a)" (print_typ syntax) typ1 (print_typ syntax) typ2
  | TUnknown -> fprintf fm "???"

and print_preds fm = function
    [] -> ()
  | [x] -> print_term ML 0 false fm x
  | x1::x2::xs -> fprintf fm "%a;%a" (print_term ML 0 false) x1 print_preds (x2::xs)

and print_id fm x =
  if x.id >= 0
  then fprintf fm "%s_%n" x.origin x.id
  else pp_print_string fm x.origin

and print_ids fm = function
    [] -> ()
  | x::xs -> fprintf fm "%a %a" print_id x print_ids xs

and print_id_typ fm x =
  match x.typ with
      TVar _ | TUnknown -> fprintf fm "%a" print_id x
    | _ -> fprintf fm "(%a:%a)" print_id x (print_typ ML) x.typ

and print_ids_typ fm = function
    [] -> ()
  | x::xs -> fprintf fm "%a %a" print_id_typ x print_ids_typ xs




(* priority (low -> high)
   1 : If, Let, Letrec
   2 : Fun
   3 : Or
   4 : And
   5 : Eq, Lt, Gt, Leq, Geq
   6 : Add, Sub
   7 : App
*)

and paren pri p = if pri < p then "","" else "(",")"

and print_binop syntax t1 t2 fm op =
  match syntax with
      ML | TRecS ->
        begin
          match op with
              Eq -> fprintf fm "="
            | Lt -> fprintf fm "<"
            | Gt -> fprintf fm ">"
            | Leq -> fprintf fm "<="
            | Geq -> fprintf fm ">="
            | And -> fprintf fm "&&"
            | Or -> fprintf fm "||"
            | Add -> fprintf fm "+"
            | Sub -> fprintf fm "-"
            | Mult -> fprintf fm "*"
        end
    | CVC3 ->
        begin
          match op with
              Eq ->
                begin
                  match t1,t2 with
                      (True|False),_
                    | _,(True|False)
                    | Var{typ=TBool},_
                    | _,Var{typ=TBool} -> fprintf fm "<=>"
                    | _ -> fprintf fm "="
                end
            | Lt -> fprintf fm "<"
            | Gt -> fprintf fm ">"
            | Leq -> fprintf fm "<="
            | Geq -> fprintf fm ">="
            | And -> fprintf fm "AND"
            | Or -> fprintf fm "OR"
            | Add -> fprintf fm "+"
            | Sub -> fprintf fm "-"
            | Mult -> fprintf fm "*"
        end
    | CSIsat ->
        begin
          match op with
              Eq -> fprintf fm "="
            | Lt -> fprintf fm "<"
            | Gt -> fprintf fm ">"
            | Leq -> fprintf fm "<="
            | Geq -> fprintf fm ">="
            | And -> fprintf fm "&"
            | Or -> fprintf fm "|"
            | Add -> fprintf fm "+"
            | Sub -> fprintf fm "-"
            | Mult -> fprintf fm "*"
        end

and print_termlist syntax pri typ fm = List.iter (fun bd -> fprintf fm "@;%a" (print_term syntax pri typ) bd)
and print_term syntax pri typ fm = function
    Unit -> fprintf fm "unit"
  | True ->
      begin
        match syntax with
            TRecS -> fprintf fm "0"
          | CVC3 -> fprintf fm "TRUE"
          | _ -> fprintf fm "true"
      end
  | False ->
      begin
        match syntax with
            TRecS -> fprintf fm "1"
          | CVC3 -> fprintf fm "FALSE"
          | _ -> fprintf fm "false"
      end
  | Unknown ->
      begin
        match syntax with
            TRecS -> fprintf fm "2"
          | _ -> fprintf fm "*"
      end
  | Int n -> fprintf fm "%d" n
  | NInt x ->
      begin
        match syntax with
            ML -> fprintf fm "?%a?" print_id x
          | _ -> print_id fm x
      end
  | Var x ->
      begin
        match syntax with
            TRecS -> print_id fm x
          | CVC3 -> print_id fm x
          | _ -> print_id fm x
      end
  | Fun(x, t) ->
      let p = 2 in
      let s1,s2 = paren pri p in
        fprintf fm "%sfun %a -> %a%s" s1 print_id x (print_term syntax p typ) t s2
  | App(t, ts) ->
      let p = 7 in
      let s1,s2 = paren pri p in
        fprintf fm "%s%a%a%s" s1 (print_term syntax p typ) t (print_termlist syntax p typ) ts s2
  | If(t1, t2, t3, t4) ->
      begin
        match syntax with
            TRecS ->
              let p = 7 in
              let s1,s2 = paren pri (p+1) in
                if !Flag.use_unknown
                then fprintf fm "(%s_case 3 %a %a %a %a%s)" s1 (print_term syntax p typ) t1 (print_term syntax p typ) t2 (print_term syntax p typ) t3 (print_term syntax p typ) t4 s2
                else fprintf fm "%s_case 2 %a %a %a%s" s1 (print_term syntax p typ) t1 (print_term syntax p typ) t2 (print_term syntax p typ) t3 s2
          | _ ->
              let p = 1 in
              let s1,s2 = paren pri (p+1) in
                fprintf fm "%s@[@[if %a@]@;then @[%a@]@;else @[%a@]@]%s" s1 (print_term syntax p typ) t1 (print_term syntax p typ) t2 (print_term syntax p typ) t3 s2
      end
  | Branch(t1, t2) ->
      let p = 7 in
      let s1,s2 = paren pri p in
        fprintf fm "%sbr %a %a%s" s1 (print_term syntax p typ) t1 (print_term syntax p typ) t2 s2
  | Let(f, xs, t1, t2)
  | Letrec(f, xs, t1, t2) as t ->
      let s_rec = match t with Let _ -> "" | _ -> " rec" in
      let p = 1 in
      let s1,s2 = paren pri (p+1) in
      let p_ids fm () =
        if typ
        then fprintf fm "%a %a" print_id f print_ids_typ xs
        else fprintf fm "%a" print_ids (f::xs)
      in
        begin
          match t2 with
              Let _
            | Letrec _ -> fprintf fm "%s@[<v>@[<hov 2>let%s %a= @,%a@]@;in@;%a@]%s" s1 s_rec p_ids () (print_term syntax p typ) t1 (print_term syntax p typ) t2 s2
            | _ -> fprintf fm "%s@[<v>@[<hov 2>let%s %a= @,%a @]@;@[<v 2>in@;@]@[<hov>%a@]@]%s" s1 s_rec p_ids () (print_term syntax p typ) t1 (print_term syntax p typ) t2 s2
        end
  | BinOp(Mult, t1, t2) when (syntax = CVC3 && (match t1 with Int(_) -> false | _ -> (match t2 with Int(_) -> false | _ -> true))) ->
      (let t = BinOp(Mult, t1, t2) in
       Format.printf "Nonlinear expression not supported: %a@." (print_term ML 0 true) t; assert false)
  | BinOp(op, t1, t2) ->
      let is_bool = function
          Var x -> x.typ = TBool
        | BinOp(Or, _, _) -> true
        | BinOp(And, _, _) -> true
        | _ -> false
      in
        begin
          if op = Eq && syntax = CVC3 && (is_bool t1 || is_bool t2)
          then
            let p = 5 in
            let s1,s2 = paren pri p in
              fprintf fm "%s%a <=> %a%s" s1 (print_term syntax p typ) t1 (print_term syntax p typ) t2 s2
          else
            let p = match op with Add|Sub|Mult -> 6 | And -> 4 | Or -> 3 | _ -> 5 in
            let s1,s2 = paren pri p in
              fprintf fm "%s%a %a %a%s" s1 (print_term syntax p typ) t1 (print_binop syntax t1 t2) op (print_term syntax p typ) t2 s2
        end
  | Not t ->
      let p = 6 in
      let s1,s2 = paren pri p in
      let nt = match syntax with CVC3 -> "NOT" | _ -> "not" in
        fprintf fm "%s%s %a%s" s1 nt (print_term syntax p typ) t s2
  | Fail -> fprintf fm "fail"
  | Label(true, t) ->
      let p = 7 in
      let s1,s2 = paren pri p in
      let s =
        match syntax with
            TRecS -> "then"
          | _ -> "l_then"
      in
        fprintf fm "%s%s %a%s" s1 s (print_term syntax p typ) t s2
  | Label(false, t) ->
      let p = 7 in
      let s1,s2 = paren pri p in
      let s =
        match syntax with
            TRecS -> "else"
          | _ -> "l_else"
      in
        fprintf fm "%s%s %a%s" s1 s (print_term syntax p typ) t s2
  | Event s ->
      fprintf fm "{%s}" s


let string_of_ident x =
  print_id str_formatter x;
  flush_str_formatter ()
let string_of_term syntax t =
  print_term syntax 0 false str_formatter t;
  flush_str_formatter ()





let print_hors fm (funs, spec) =
  let print_fundef (f, xs, t) =
    fprintf fm "%a %a-> %a.\n" print_id f print_ids xs (print_term TRecS 0 false) t
  in
    fprintf fm "%%BEGING\n";
    List.iter print_fundef funs;
    fprintf fm "%%ENDG\n\n";
    fprintf fm "%%BEGINA\n";
    Automata.print_buchi fm spec;
    fprintf fm "%%ENDA\n\n"



let print_term_fm syntax typ fm t =
  print_term syntax 0 typ fm t;
  printf "@?"

and print_term syntax typ t =
  print_term syntax 0 typ std_formatter t;
  printf "@?"

and print_term_fm_break syntax typ fm t =
  print_term syntax 0 typ fm t;
  printf "@."

and print_term_break syntax typ t =
  print_term syntax 0 typ std_formatter t;
  printf "@."






let print_constr fm = function
    Cond t -> print_term_fm ML false fm t
  | Pred(f,n,x,ts) -> Format.printf "P_{%a_%d}^%a(%a)" print_id f n print_id x (print_termlist ML 0 false) ts
let print_constr_list fm = List.iter (fun c -> Format.fprintf fm "@;[%a]" print_constr c)



let pp_print_typ = print_typ ML
let pp_print_term = print_term_fm ML false



let rec eta_expand = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Int n -> Int n
  | NInt x -> NInt x
  | Var x -> Var x
  | Fun(x, t) -> assert false
      (*let f = new_var' "f" in
        eta_expand (Let(f, [x], t, Var f))*)
  | App(t, ts) ->
      let t' = eta_expand t in
      let ts' = List.map eta_expand ts in
        App(t', ts')
  | If(t1, t2, t3, t4) ->
      let t1' = eta_expand t1 in
      let t2' = eta_expand t2 in
      let t3' = eta_expand t3 in
      let t4' = eta_expand t4 in
        If(t1', t2', t3', t4')
  | Let(f, xs, t1, t2) ->
      let t1' = eta_expand t1 in
      let t2' = eta_expand t2 in

    let n = (List.length (get_args f.typ)) - (List.length xs) in
    let ds = tabulate n (fun _ -> new_var' "d") in
    let xs' = List.map2 (fun x x' -> {x with typ = x'.typ}) (xs @ ds) (get_args f.typ) in

        Let(f, xs', App(t1', List.map (fun d -> Var(d)) ds), t2')
  | Letrec(f, xs, t1, t2) ->
      let t1' = eta_expand t1 in
      let t2' = eta_expand t2 in

    let n = (List.length (get_args f.typ)) - (List.length xs) in
    let ds = tabulate n (fun _ -> new_var' "d") in
    let xs' = List.map2 (fun x x' -> {x with typ = x'.typ}) (xs @ ds) (get_args f.typ) in

        Letrec(f, xs', App(t1', List.map (fun d -> Var(d)) ds), t2')
  | BinOp(op, t1, t2) ->
      let t1' = eta_expand t1 in
      let t2' = eta_expand t2 in
        BinOp(op, t1', t2')
  | Fail -> Fail
  | Unknown -> Unknown
  | _ -> assert false


let rec get_trace ce env trace t =
  match t,ce with
      Var x, _ -> get_trace ce env trace (App(Var x, []))
    | App(Fail, _), [] -> List.rev trace
    | App(Var x, ts), _ ->
        let b,t' = List.assoc x env in
        let xs = get_args x.typ in
        let xs' = List.map (fun x -> {(new_var' x.origin) with typ = x.typ}) xs in
        let t'' = List.fold_right2 subst xs (List.map (fun x -> Var x) xs') t' in
        let trace' = if b then (Var x)::trace else trace in
        let env' =
          let aux env x t =
            match x.typ with
                TFun _ -> (x, (false,expand t x.typ))::env
              | _ -> env
          in
            List.fold_left2 aux env xs' ts
        in
          get_trace ce env' trace' t''
    | Let(f, xs, t1, t2), _ ->
        let f' = {(new_var' f.origin) with typ = f.typ} in
        let t2' = subst f (Var f') t2 in
        let env' = (f',(true,t1))::env in
          get_trace ce env' trace t2'
    | Letrec(f, xs, t1, t2), _ ->
        let f' = {(new_var' f.origin) with typ = f.typ} in
        let t1' = subst f (Var f') t1 in
        let t2' = subst f (Var f') t2 in
        let env' = (f',(true,t1'))::env in
          get_trace ce env' trace t2'
    | If(t1, t2, _, _), true::ce' ->
        let trace' = True::trace in
          get_trace ce' env trace' t2
    | If(t1, _, t3, _), false::ce' ->
        let trace' = False::trace in
          get_trace ce' env trace' t3
    | _ -> assert false

let get_trace ce t = get_trace ce [] [] t



let rec normalize = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | Var x -> Var x
  | Fun(x, t) -> Fun(x, t)
  | App(If(t1,t2,t3,t4), ts) ->
      let t1' = normalize t1 in
      let t2' = normalize (app2app t2 ts) in
      let t3' = normalize (app2app t3 ts) in
      let t4' = normalize (app2app t4 ts) in
        If(t1', t2', t3', t4')
  | App(Branch(t1,t2), ts) ->
      let t1' = normalize (app2app t1 ts) in
      let t2' = normalize (app2app t2 ts) in
        Branch(t1', t2')
  | App(t, ts) ->
      let t' = normalize t in
      let ts' = List.map normalize ts in
        App(t', ts')
  | If(t1, t2, t3, t4) ->
      let t1' = normalize t1 in
      let t2' = normalize t2 in
      let t3' = normalize t3 in
      let t4' = normalize t4 in
        If(t1', t2', t3', t4')
  | Branch(t1, t2) ->
      let t1' = normalize t1 in
      let t2' = normalize t2 in
        Branch(t1', t2')
  | Let(f, xs, t1, t2) ->
      let t1' = normalize t1 in
      let t2' = normalize t2 in
        Let(f, xs, t1', t2')
  | Letrec(f, xs, t1, t2) ->
      let t1' = normalize t1 in
      let t2' = normalize t2 in
        Letrec(f, xs, t1', t2')
  | BinOp(op, t1, t2) ->
      let t1' = normalize t1 in
      let t2' = normalize t2 in
        BinOp(op, t1', t2')
  | Not t ->
      let t' = normalize t in
        Not t'
  | Fail -> Fail
  | Label(b, t) ->
      let t' = normalize t in
        Label(b, t')




let rec copy_pred_aux t1 t2 =
  match t1,t2 with
      Let(f1,xs1,t11,t12),Let(f2,xs2,t21,t22) ->
        assert (f1.id = f2.id); t2
    | Letrec(f1,xs1,t11,t12),Letrec(f2,xs2,t21,t22) ->
        assert (f1.id = f2.id); t2
    | _ -> t2

let copy_pred = copy_pred_aux



let rec normalize_bool_exp = function
    True -> True
  | False -> False
  | Unknown -> Unknown
  | Var x -> Var x
  | BinOp(Or|And as op, t1, t2) ->
      let t1' = normalize_bool_exp t1 in
      let t2' = normalize_bool_exp t2 in
        BinOp(op, t1', t2')
  | BinOp(Eq, (True|False), _)
  | BinOp(Eq, _, (True|False)) as t -> t
  | BinOp(Eq|Lt|Gt|Leq|Geq as op, t1, t2) ->
      let neg xs = List.map (fun (x,n) -> x,-n) xs in
      let rec decomp = function
          Int n -> [None, n]
        | Var x -> [Some (Var x), 1]
        | NInt x -> [Some (NInt x), 1]
        | BinOp(Add, t1, t2) ->
            decomp t1 @@ decomp t2
        | BinOp(Sub, t1, t2) ->
            decomp t1 @@ neg (decomp t2)
        | BinOp(Mult, t1, t2) ->
            let xns1 = decomp t1 in
            let xns2 = decomp t2 in
            let reduce xns = List.fold_left (fun acc (_,n) -> acc+n) 0 xns in
            let aux (x,_) = x <> None in
              begin
                match List.exists aux xns1, List.exists aux xns2 with
                    true, true ->
                      Format.printf "Nonlinear expression not supported: %a@." pp_print_term (BinOp(op,t1,t2));
                      assert false
                  | false, true ->
                      let k = reduce xns1 in
                        List.rev_map (fun (x,n) -> x,n*k) xns2
                  | true, false ->
                      let k = reduce xns2 in
                        List.rev_map (fun (x,n) -> x,n*k) xns1
                  | false, false ->
                      [None, reduce xns1 + reduce xns2]
              end
        | _ -> assert false
      in
      let xns1 = decomp t1 in
      let xns2 = decomp t2 in
      let compare (x1,_) (x2,_) =
        let aux = function
            None -> max_int
          | Some (Var x) -> x.id
          | Some (NInt n) -> n.id
          | _ -> assert false
        in
          compare (aux x1) (aux x2)
      in
      let xns = List.sort compare (xns1 @@ (neg xns2)) in
      let rec aux = function
          [] -> []
        | (x,n)::xns ->
            let xns1,xns2 = List.partition (fun (y,_) -> x=y) xns in
            let n' = List.fold_left (fun acc (_,n) -> acc+n) 0 ((x,n)::xns1) in
              (x,n') :: aux xns2
      in
      let xns' = aux xns in
      let xns'' = List.filter (fun (x,n) -> n<>0) xns' in
      let op',t1',t2' =
        match xns'' with
                [] -> assert false
          | (x,n)::xns ->
            let aux = function
                None,n -> Int n
              | Some x,n -> if n=1 then x else BinOp(Mult, Int n, x)
            in
            let t1,xns',op' =
              if n<0
              then
                let op' =
                  match op with
                          Eq -> Eq
                    | Lt -> Gt
                    | Gt -> Lt
                    | Leq -> Geq
                    | Geq -> Leq
                    | _ -> assert false
                in
                  aux (x,-n), xns, op'
              else
                aux (x,n), neg xns, op
            in
            let ts = List.map aux xns' in
            let t2 =
              match ts with
                      [] -> Int 0
                | t::ts' ->
                  List.fold_left (fun t2 t -> BinOp(Add, t2, t)) t ts'
            in
              op', t1, t2
      in
      let rec simplify = function
          BinOp(Add, t1, BinOp(Mult, Int n, t2)) when n < 0 ->
            let t1' = simplify t1 in
              BinOp(Sub, t1', BinOp(Mult, Int (-n), t2))
        | BinOp(Add, t1, Int n) when n < 0 ->
            let t1' = simplify t1 in
              BinOp(Sub, t1', Int (-n))
        | BinOp(Add, t1, t2) ->
            let t1' = simplify t1 in
              BinOp(Add, t1', t2)
        | t -> t
      in
        BinOp(op', t1', simplify t2')
  | Not t -> Not (normalize_bool_exp t)
  | Unit
  | Int _
  | NInt _
  | Fun _
  | App _
  | If _
  | Branch _
  | Let _
  | Letrec _
  | BinOp((Add|Sub|Mult), _, _)
  | Fail
  | Label _ -> assert false


