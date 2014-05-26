open Util
open CEGAR_syntax
open CEGAR_type

module S = Syntax
module U = Term_util

let const_of_bool b = if b then True else False



let apply_body_def f (g,xs,t1,e,t2) = g, xs, t1, e, f t2



let rec subst x t = function
    Const c -> Const c
  | Var y when x = y -> t
  | Var y -> Var y
  | App(t1,t2) -> App(subst x t t1, subst x t t2)
  | Let(y,t1,t2) when x = y -> Let(y, subst x t t1, t2)
  | Let(y,t1,t2) -> Let(y, subst x t t1, subst x t t2)
  | Fun(y,typ,t1) when x = y -> Fun(y, typ, t1)
  | Fun(y,typ,t1) -> Fun(y, typ, subst x t t1)

let rec subst_map map = function
    Const c -> Const c
  | Var x when List.mem_assoc x map -> List.assoc x map
  | Var x -> Var x
  | App(t1,t2) -> App(subst_map map t1, subst_map map t2)
  | Let(x,t1,t2) ->
      let map' = List.filter (fun (y,_) -> x <> y) map in
        Let(x, subst_map map' t1, subst_map map' t2)
  | Fun(x, typ,t1) ->
      let map' = List.filter (fun (y,_) -> x <> y) map in
        Fun(x, typ, subst_map map' t1)

let subst_def x t (f,xs,t1,t2) =
  f, xs, subst x t t1, subst x t t2

let rec subst_typ x t = function
    TBase(b,ps) ->
      (** ASSUME: y does not contain x **)
      let ps' y = List.map (subst x t) (ps y) in
        TBase(b, ps')
  | TFun(typ1,typ2) -> TFun(subst_typ x t typ1, fun y -> subst_typ x t (typ2 y))
  | _ -> assert false

let rec subst_typ_map map = function
    TBase(b,ps) ->
      (** ASSUME: y does not contain x **)
      let ps' y = List.map (subst_map map) (ps y) in
        TBase(b, ps')
  | TFun(typ1,typ2) -> TFun(subst_typ_map map typ1, fun y -> subst_typ_map map (typ2 y))
  | _ -> assert false



let rec arg_num = function
    TBase _ -> 0
  | TFun(_,typ) -> 1 + arg_num (typ (Const Unit))
  | _ -> assert false




let rec pop_main {env=env;defs=defs;main=main} =
  let compare_fun f g = compare (g = main, f) (f = main, g) in
  let compare_def (f,_,_,_,_) (g,_,_,_,_) = compare_fun f g in
  let compare_env (f,_) (g,_) = compare_fun f g in
  let env' = List.sort ~cmp:compare_env env in
  let defs' = List.sort ~cmp:compare_def defs in
  {env=env'; defs=defs'; main=main}




let rec get_arg_env typ xs =
  match typ,xs with
      TFun(typ1,typ2), x::xs ->
        let typ2 = typ2 (Var x) in
          (x,typ1) :: get_arg_env typ2 xs
    | _ -> []




let rec put_into_if_term = function
    Const c -> Const c
  | Var x -> Var x
  | App _ as t ->
      let t',ts = decomp_app t in
        if t' = Const If
        then
          match ts with
              t1::t2::t3::ts' ->
                let t1' = put_into_if_term t1 in
                let t2' = put_into_if_term t2 in
                let t3' = put_into_if_term t3 in
                let ts'' = List.map put_into_if_term ts' in
                make_if t1' (put_into_if_term (make_app t2' ts'')) (put_into_if_term (make_app t3' ts''))
            | _ -> Format.printf "%a@." CEGAR_print.term t; assert false
        else
          let ts' = List.map put_into_if_term ts in
            make_app t' ts'
  | Fun(x,typ,t) -> Fun(x, typ, put_into_if_term t)
  | Let(x,t1,t2) -> Let(x, put_into_if_term t1, put_into_if_term t2)
let put_into_if prog =
  {prog with defs=List.map (apply_body_def put_into_if_term) prog.defs}





let eta_expand_def env (f,xs,t1,e,t2) =
  let d = arg_num (List.assoc f env) - List.length xs in
  let ys = Array.to_list (Array.init d (fun _ -> new_id "x")) in
  let t2' = List.fold_left (fun t x -> App(t, Var x)) t2 ys in
    f, xs@ys, t1, e, t2' (* put_into_term t2' *)

let eta_expand prog =
  {prog with defs = List.map (eta_expand_def prog.env) prog.defs}


let rec make_arg_let t =
  let desc =
    match t.S.desc with
        S.Const c -> S.Const c
      | S.Unknown -> assert false
      | S.Var x -> S.Var x
      | S.App(t, ts) ->
          let f = Id.new_var "f__" (t.S.typ) in
          let xts = List.map (fun t -> Id.new_var "x" (t.S.typ), t) ts in
          let t' =
            {S.desc=S.App(U.make_var f, List.map (fun (x,_) -> U.make_var x) xts);
             S.typ=Type.typ_unknown}
          in
            (List.fold_left (fun t2 (x,t1) -> U.make_let [x,[],t1] t2) t' ((f,t)::xts)).S.desc
      | S.If(t1, t2, t3) ->
          let t1' = make_arg_let t1 in
          let t2' = make_arg_let t2 in
          let t3' = make_arg_let t3 in
            S.If(t1',t2',t3')
      | S.Branch(t1, t2) -> assert false
      | S.Let(flag,bindings,t2) ->
          let bindings' = List.map (fun (f,xs,t) -> f, xs, make_arg_let t) bindings in
          let t2' = make_arg_let t2 in
            S.Let(flag,bindings',t2')
      | S.BinOp(op, t1, t2) ->
          let t1' = make_arg_let t1 in
          let t2' = make_arg_let t2 in
            S.BinOp(op, t1', t2')
      | S.Not t -> S.Not (make_arg_let t)
      | S.Fun(x,t) -> assert false
      | S.Event _ -> assert false
      | _ -> assert false
  in
    {S.desc=desc; S.typ=t.S.typ}




exception TypeBottom

let nil = fun _ -> []

let rec get_const_typ = function
  | Unit -> typ_unit
  | True -> typ_bool()
  | False -> typ_bool()
  | Char _ -> typ_abst "char"
  | String _ -> typ_abst "string"
  | Float _ -> typ_abst "float"
  | Int32 _ -> typ_abst "int32"
  | Int64 _ -> typ_abst "int64"
  | Nativeint _ -> typ_abst "nativeint"
  | RandInt -> TFun(TFun(TBase(TInt,nil), fun x -> typ_unit), fun x -> typ_unit)
  | RandBool -> TBase(TBool,nil)
  | RandVal s -> TBase(TAbst s,nil)
  | And -> TFun(typ_bool(), fun x -> TFun(typ_bool(), fun y -> typ_bool()))
  | Or -> TFun(typ_bool(), fun x -> TFun(typ_bool(), fun y -> typ_bool()))
  | Not -> TFun(TBase(TInt,nil), fun x -> typ_bool())
  | Lt -> TFun(TBase(TInt,nil), fun x -> TFun(TBase(TInt,nil), fun y -> typ_bool()))
  | Gt -> TFun(TBase(TInt,nil), fun x -> TFun(TBase(TInt,nil), fun y -> typ_bool()))
  | Leq -> TFun(TBase(TInt,nil), fun x -> TFun(TBase(TInt,nil), fun y -> typ_bool()))
  | Geq -> TFun(TBase(TInt,nil), fun x -> TFun(TBase(TInt,nil), fun y -> typ_bool()))
  | EqUnit -> TFun(TBase(TUnit,nil), fun x -> TFun(TBase(TUnit,nil), fun y -> typ_bool()))
  | EqBool -> TFun(TBase(TBool,nil), fun x -> TFun(TBase(TBool,nil), fun y -> typ_bool()))
  | EqInt -> TFun(TBase(TInt,nil), fun x -> TFun(TBase(TInt,nil), fun y -> typ_bool()))
  | CmpPoly(typ,_) -> TFun(TBase(TAbst typ,nil), fun x -> TFun(TBase(TAbst typ,nil), fun y -> typ_bool()))
  | Int n -> TBase(TInt, fun x -> [make_eq_int x (Const (Int n))])
  | Add -> TFun(TBase(TInt,nil), fun x ->
                TFun(TBase(TInt,nil), fun y ->
                     TBase(TInt,fun r -> [make_eq_int r (make_add x y)])))
  | Sub -> TFun(TBase(TInt,nil), fun x ->
                TFun(TBase(TInt,nil), fun y ->
                     TBase(TInt,fun r -> [make_eq_int r (make_sub x y)])))
  | Mul -> TFun(TBase(TInt,nil), fun x ->
                TFun(TBase(TInt,nil), fun y ->
                     TBase(TInt,fun r -> [make_eq_int r (make_mul x y)])))
  | Tuple _ -> assert false
  | Proj _ -> assert false
  | If -> assert false
  | Bottom -> raise TypeBottom
  | Label _ -> assert false
  | Temp _ -> assert false
  | CPS_result -> typ_result



let rec get_typ env = function
    Const c -> get_const_typ c
  | Var x -> List.assoc x env
  | App(Const (Label _), t) ->
      get_typ env t
  | App(Const RandInt, t) ->
      let typ2 = match get_typ env t with TFun(_,typ) -> typ (Var "") | _ -> assert false in
        typ2
  | App(App(App(Const If, _), t1), t2) ->
      begin
        try
          get_typ env t1
        with TypeBottom -> get_typ env t2
      end
  | App(t1,t2) ->
      let typ2 = match get_typ env t1 with TFun(_,typ) -> typ t2 | _ -> assert false in
        typ2
  | Let(x,t1,t2) ->
      let typ = get_typ env t1 in
      let env' = (x,typ)::env in
        get_typ env' t2
  | Fun(x,_,t) -> assert false
(*
      let typ1 = List.assoc x env in
      let typ2 = get_typ env t in
        TFun(typ1, fun _ -> typ2)
*)




let rec get_arg_num = function
    TFun(_,typ) -> 1 + get_arg_num (typ (Const Unit))
  | typ -> 0


let map_defs f defs =
  let aux (g,xs,t1,t2) =
    let defs1,t1' = f t1 in
    let defs2,t2' = f t2 in
      (g,xs,t1',t2')::defs1@@@defs2
  in
    rev_map_flatten aux defs










let rec has_bottom = function
    Var _ -> false
  | Const Bottom -> true
  | Const _ -> false
  | App(t1, t2) -> has_bottom t1 || has_bottom t2
  | _ -> assert false




let rec normalize_bool_term ?(imply = fun _ _ -> false) = function
    Const c -> Const c
  | Var x -> Var x
  | App(Const Not, App(Const Not, t1)) -> normalize_bool_term ~imply t1
  | App(Const Not, App(App(Const (Lt|Gt|Leq|Geq as op), t1), t2)) ->
      let op' =
        match op with
            Lt -> Geq
          | Gt -> Leq
          | Leq -> Gt
          | Geq -> Lt
          | _ -> assert false
      in
        normalize_bool_term ~imply (App(App(Const op', t1), t2))
  | App(App(Const EqBool, Const True), t) -> normalize_bool_term ~imply t
  | App(App(Const And, _), _) as t ->
      let rec decomp = function
          App(App(Const And, t1), t2) -> decomp t1 @@@ decomp t2
        | t -> [normalize_bool_term ~imply t]
      in
      let rec aux ts1 = function
          [] -> List.rev ts1
        | t::ts2 ->
            if imply (ts1@@@ts2) t
            then aux ts1 ts2
            else aux (t::ts1) ts2
      in
      let ts' = aux [] (decomp t) in
      begin
        match ts' with
            [] -> Const True
          | t'::ts'' -> List.fold_left make_and t' ts''
      end
  | App(App(Const Or, _), _) as t ->
      let rec decomp = function
          App(App(Const Or, t1), t2) -> decomp t1 @@@ decomp t2
        | t -> [normalize_bool_term ~imply t]
      in
      let rec aux ts1 = function
          [] -> ts1
        | t::ts2 ->
            if imply (ts1@@@ts2) (make_not t)
            then aux ts1 ts2
            else aux (t::ts1) ts2
      in
      let ts' = aux [] (decomp t) in
      begin
        match ts' with
            [] -> Const False
          | t'::ts'' -> List.fold_left make_or t' ts''
      end
  | App(App(Const (EqInt|Lt|Gt|Leq|Geq) as op, t1), t2) ->
      let neg xs = List.map (fun (x,n) -> x,-n) xs in
      let rec decomp = function
          Const (Int n) -> [None, n]
        | Var x -> [Some (Var x), 1]
        | App(App(Const Add, t1), t2) ->
            decomp t1 @@@ decomp t2
        | App(App(Const Sub, t1), t2) ->
            decomp t1 @@@ neg (decomp t2)
        | App(App(Const Mul, t1), t2) ->
            let xns1 = decomp t1 in
            let xns2 = decomp t2 in
            let reduce xns = List.fold_left (fun acc (_,n) -> acc+n) 0 xns in
            let not_const xns = List.exists (fun (x,_) -> x <> None) xns in
              begin
                match not_const xns1, not_const xns2 with
                    true, true ->
                      Format.printf "Nonlinear expression not supported: %a@."
                        CEGAR_print.term (make_app op [t1;t2]);
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
            None -> true, ""
          | Some (Var x) -> false, x
          | _ -> assert false
        in
          compare (aux x1) (aux x2)
      in
      let xns = List.sort (xns1 @@@ (neg xns2)) in
      let d = List.fold_left (fun d (_,n) -> gcd d (abs n)) 0 xns in
      let xns' = List.map (fun (x,n) -> assert (n mod d = 0); x, n/d) xns in
      let rec aux = function
          [] -> []
        | (x,n)::xns ->
            let xns1,xns2 = List.partition (fun (y,_) -> x=y) xns in
            let n' = List.fold_left (fun acc (_,n) -> acc+n) 0 ((x,n)::xns1) in
              (x,n') :: aux xns2
      in
      let xns'' = aux xns' in
      let xns''' = List.filter (fun (_,n) -> n<>0) xns'' in
      let x,n = List.hd xns''' in
      let xns = List.tl xns''' in
      let op',t1',t2' =
        let aux = function
            None, n -> Const (Int n)
          | Some x, 1 -> x
          | Some x, n -> make_mul (make_int n) x
        in
        let t1,xns',op' =
          if n<0
          then
            let op' =
              match op with
                  Const EqInt -> Const EqInt
                | Const Lt -> Const Gt
                | Const Gt -> Const Lt
                | Const Leq -> Const Geq
                | Const Geq -> Const Leq
                | _ -> assert false
            in
              aux (x,-n), xns, op'
          else
            aux (x,n), neg xns, op
        in
        let ts = List.map aux xns' in
        let make_add_sub t1 t2 =
          match t2 with
              Const (Int n) when n < 0 -> make_sub t1 (make_int (-n))
            | App(App(Const Mul, Const (Int n)), t2') when n < 0 -> make_sub t1 (make_mul (make_int (-n)) t2')
            | _ -> make_add t1 t2
        in
        let t2 =
          match ts with
              [] -> Const (Int 0)
            | t::ts' -> List.fold_left make_add_sub t ts'
        in
          op', t1, t2
      in
        make_app op' [t1'; t2']
  | App(t1, t2) -> App(normalize_bool_term ~imply t1, normalize_bool_term ~imply t2)
  | Let _ -> assert false
  | Fun _ -> assert false





let assoc_fun_def defs f =
  let make_fun xs t =
    let xs' = List.map rename_id xs in
    let map = List.map2 (fun x x' -> x, Var x') xs xs' in
      List.fold_right (fun x t -> Fun(x,None,t)) xs' (subst_map map t)
  in
  let defs' = List.filter (fun (g,_,_,_,_) -> f = g) defs in
    match defs' with
        [_,xs,Const True,_,t] -> make_fun xs t
      | [_] -> raise (Fatal "Not implemented: CEGAR_abst_CPS.assoc_fun_def")
      | [_,xs1,t11,_,t12; _,xs2,t21,_,t22] when make_not t11 = t21 ->
          assert (xs1 = xs2);
          make_fun xs1 (make_if t11 t12 t22)
      | [_,xs1,t11,_,t12; _,xs2,t21,_,t22] when t11 = make_not t21 ->
          assert (xs1 = xs2);
          make_fun xs1 (make_if t21 t22 t12)
      | _ -> assert false

let get_nonrec defs main orig_fun_list force =
  let check (f,xs,t1,e,t2) =
    let defs' = List.filter (fun (g,_,_,_,_) -> f = g) defs in
    let used = List.filter (fun (_,_,t1,_,t2) -> List.mem f (get_fv t1 @@@ get_fv t2)) defs in
      List.for_all (fun (_,_,_,e,_) -> e = []) defs' &&
        f <> main &&
        (List.for_all (fun (_,xs,t1,e,t2) -> subset (get_fv t1 @@@ get_fv t2) xs) defs' ||
         (1 >= List.length (uniq (List.map (fun (f,_,_,_,_) -> f) used)) || List.mem f force) &&
         2 >= List.length defs')
  in
  let defs' = List.filter check defs in
  let nonrec = List.map (fun (f,xs,_,_,t) -> f, assoc_fun_def defs f) defs' in
    if !Flag.expand_nonrec_init
    then nonrec
    else
      let orig_fun_list' = diff orig_fun_list force in
        List.filter (fun (f,_) -> not (List.mem f orig_fun_list')) nonrec


let print_prog_typ' orig_fun_list force fm {env=env;defs=defs;main=main} =
  let nonrec = get_nonrec defs main orig_fun_list force in
  let env' = List.filter (fun (f,_) -> not (List.mem_assoc f nonrec)) env in
    CEGAR_print.prog_typ fm {env=env';defs=defs;main=main}
