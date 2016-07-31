open Util
open CEGAR_syntax
open CEGAR_type

type ext_path_part = Positive | Negative | Do_not_Care
type ext_path = ext_path_part list

exception TypeBottom

module S = Syntax
module U = Term_util

let const_of_bool b = if b then True else False



let map_body_def f (g,xs,t1,e,t2) = g, xs, t1, e, f t2
let map_def_prog f prog = {prog with defs = List.map f prog.defs}
let map_body_prog f prog = map_def_prog (map_body_def f) prog



let rec subst x t = function
  | Const c -> Const c
  | Var y when x = y -> t
  | Var y -> Var y
  | App(t1,t2) -> App(subst x t t1, subst x t t2)
  | Let(y,t1,t2) when x = y -> Let(y, subst x t t1, t2)
  | Let(y,t1,t2) -> Let(y, subst x t t1, subst x t t2)
  | Fun(y,typ,t1) when x = y -> Fun(y, typ, t1)
  | Fun(y,typ,t1) -> Fun(y, typ, subst x t t1)

let subst_var x y t = subst x (Var y) t

let rec subst_map map = function
  | Const c -> Const c
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
  | TBase(b,ps) ->
      (** ASSUME: y does not contain x **)
      let ps' y = List.map (subst x t) (ps y) in
      TBase(b, ps')
  | TFun(typ1,typ2) -> TFun(subst_typ x t typ1, subst_typ x t -| typ2)
  | TApp(typ1, typ2) -> TApp(subst_typ x t typ1, subst_typ x t typ2)
  | _ -> assert false

let rec subst_typ_map map = function
  | TBase(b,ps) ->
      (** ASSUME: y does not contain an element in Dom(map) **)
      let ps' y = List.map (subst_map map) (ps y) in
      TBase(b, ps')
  | TFun(typ1,typ2) -> TFun(subst_typ_map map typ1, fun y -> subst_typ_map map (typ2 y))
  | _ -> assert false



let rec arg_num = function
  | TBase _ -> 0
  | TFun(_,typ) -> 1 + arg_num (typ (Const Unit))
  | TApp _ -> 0
  | _ -> assert false




let rec pop_main {env;defs;main;info} =
  let compare_fun f g = compare (g = main, f) (f = main, g) in
  let compare_def (f,_,_,_,_) (g,_,_,_,_) = compare_fun f g in
  let compare_env (f,_) (g,_) = compare_fun f g in
  let env' = List.sort ~cmp:compare_env env in
  let defs' = List.sort ~cmp:compare_def defs in
  {env=env'; defs=defs'; main; info}




let rec get_arg_env typ xs =
  match typ,xs with
  | TFun(typ1,typ2), x::xs ->
      let typ2 = typ2 @@ Var x in
      (x,typ1) :: get_arg_env typ2 xs
  | _ -> []




let rec put_into_if_term = function
  | Const c -> Const c
  | Var x -> Var x
  | App _ as t ->
      let t',ts = decomp_app t in
      if t' = Const If
      then
        match ts with
        | t1::t2::t3::ts' ->
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
let put_into_if prog = map_body_prog put_into_if_term prog





let eta_expand_def env (f,xs,t1,e,t2) =
  let d = arg_num (List.assoc f env) - List.length xs in
  let ys = List.init d (fun _ -> new_id "x") in
  let t2' = List.fold_left (fun t x -> App(t, Var x)) t2 ys in
  f, xs@ys, t1, e, t2' (* put_into_term t2' *)

let eta_expand prog =
  {prog with defs = List.map (eta_expand_def prog.env) prog.defs}


let rec make_arg_let t =
  let desc =
    match t.S.desc with
    | S.Const c -> S.Const c
    | S.Var x -> S.Var x
    | S.App(t, ts) ->
        let f = Id.new_var ~name:"f__" (t.S.typ) in
        let xts = List.map (fun t -> Id.new_var (t.S.typ), t) ts in
        let t' =
          {S.desc = S.App(U.make_var f, List.map (fun (x,_) -> U.make_var x) xts);
           S.typ = Type.typ_unknown;
           S.attr = []}
        in
        (List.fold_left (fun t2 (x,t1) -> U.make_let [x,[],t1] t2) t' ((f,t)::xts)).S.desc
    | S.If(t1, t2, t3) ->
        let t1' = make_arg_let t1 in
        let t2' = make_arg_let t2 in
        let t3' = make_arg_let t3 in
        S.If(t1',t2',t3')
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
  {t with S.desc}



let assoc_renv n env =
  try
    snd @@ List.find (fun (s,_) -> Some n = decomp_randint_name s) env
  with Not_found -> assert false

let decomp_rand_typ ?(xs=None) typ =
  let xs = Option.map (fun ys -> ys @ [Const Unit]) xs in
  match decomp_tfun ~xs typ with
  | typs, typ' when is_typ_result typ' ->
      let typs',typ'' = List.decomp_snoc typs in
      let preds =
        match typ'' with
        | TFun(TBase (TInt, preds), typ''') when is_typ_result @@ typ''' (Const Unit) -> preds
        | _ -> assert false
      in
      typs', preds
  | _ -> assert false

let mem_assoc_renv n env =
  try
    ignore @@ assoc_renv n env;
    true
  with Not_found -> false

let rec col_rand_ids t =
  match t with
  | Const (Rand(TInt, Some n)) -> [n]
  | Const c -> []
  | Var x -> []
  | App(t1,t2) -> col_rand_ids t1 @ col_rand_ids t2
  | Let _ -> unsupported "col_rand_ids"
  | Fun _ -> unsupported "col_rand_ids"

let get_renv_aux prog i =
  let f,xs,_,_,_ = List.find (fun (_,_,_,_,t) -> List.mem i @@ col_rand_ids t) prog.defs in
  let typs,_ = decomp_tfun (List.assoc f prog.env) in
  let env = List.combine xs typs in
  List.filter (is_base -| snd) env

let get_renv prog =
  let rand_ids = List.filter_map (decomp_randint_name -| fst) prog.env in
  List.map (Pair.add_right @@ get_renv_aux prog) rand_ids

let make_renv n prog =
  let rand_ids = List.init n succ in
  let env = List.map (Pair.add_right @@ get_renv_aux prog) rand_ids in
  let make xtyps =
    let typ0 = TFun(TFun(typ_int, fun _ -> typ_result), fun _ -> typ_result) in
    List.fold_right (fun (x,typ1) typ2 -> TFun(typ1, subst_typ x -$- typ2)) xtyps typ0
  in
  List.map (Pair.map make_randint_name make) env





let nil = fun _ -> []

let get_const_typ env = function
  | Unit -> typ_unit
  | True -> typ_bool()
  | False -> typ_bool()
  | Char _ -> typ_abst "char"
  | String _ -> typ_abst "string"
  | Float _ -> typ_abst "float"
  | Int32 _ -> typ_abst "int32"
  | Int64 _ -> typ_abst "int64"
  | Nativeint _ -> typ_abst "nativeint"
  | Rand(TInt, None) -> TFun(TFun(TBase(TInt,nil), fun _ -> typ_unit), fun _ -> typ_unit)
  | Rand(TInt, Some n) -> assoc_renv n env
  | Rand(TBool, _) -> TBase(TBool,nil)
  | Rand _ -> assert false
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
  | TreeConstr(n,_) ->
      let typ = typ_unit in
      let typs = List.make n typ in
      List.fold_right (fun typ1 typ2 -> TFun(typ1, fun _ -> typ2)) typs typ



let rec get_typ env = function
  | Const c -> get_const_typ env c
  | Var x -> List.assoc x env
  | App(Const (Label _), t) -> get_typ env t
  | App(Const (Rand(TInt, _)), t) ->
      begin
        match get_typ env t with
        | TFun(_,typ) -> typ (Var "")
        | _ -> assert false
      end
  | App(App(App(Const If, _), t1), t2) ->
      begin
        try
          get_typ env t1
        with TypeBottom -> get_typ env t2
      end
  | App(t1,t2) ->
      begin
        match get_typ env t1 with
        | TFun(_,typ) -> typ t2
        | _ -> assert false
      end
  | Let(x,t1,t2) ->
      let typ = get_typ env t1 in
      let env' = (x,typ)::env in
      get_typ env' t2
  | Fun(x,Some typ,t) ->
      let typ' = get_typ ((x,typ)::env) t in
      TFun(typ, fun y -> subst_typ x y typ')
  | Fun(x,_,t) -> assert false
(*
      let typ1 = List.assoc x env in
      let typ2 = get_typ env t in
      TFun(typ1, fun _ -> typ2)
*)




let rec get_arg_num = function
  | TFun(_,typ) -> 1 + get_arg_num (typ (Const Unit))
  | typ -> 0



let rec has_bottom = function
  | Var _ -> false
  | Const Bottom -> true
  | Const _ -> false
  | App(t1, t2) -> has_bottom t1 || has_bottom t2
  | _ -> assert false


let rec normalize_bool_term ?(imply = fun env t -> List.mem t env) t =
  match t with
  | Const c -> Const c
  | Var x -> Var x
  | App(Const Not, App(App(Const (Lt|Gt|Leq|Geq as op), t1), t2)) ->
      let op' =
        match op with
        | Lt -> Geq
        | Gt -> Leq
        | Leq -> Gt
        | Geq -> Lt
        | _ -> assert false
      in
      normalize_bool_term ~imply (App(App(Const op', t1), t2))
  | App(Const Not, t1) ->
      begin
        match normalize_bool_term ~imply t1 with
        | App(Const Not, t1') -> t1'
        | t1' -> App(Const Not, t1')
      end
  | App(App(Const EqBool, Const True), t)
  | App(App(Const EqBool, t), Const True) -> normalize_bool_term ~imply t
  | App(App(Const EqBool, Const False), t)
  | App(App(Const EqBool, t), Const False) -> make_not @@ normalize_bool_term ~imply t
  | App(App(Const And, _), _) as t ->
      let rec decomp = function
        | App(App(Const And, t1), t2) -> decomp t1 @@@ decomp t2
        | t -> [normalize_bool_term ~imply t]
      in
      let rec aux ts1 = function
        | [] -> List.rev ts1
        | t::ts2 ->
            if imply (ts1@@@ts2) t
            then aux ts1 ts2
            else aux (t::ts1) ts2
      in
      let ts' = aux [] (decomp t) in
      begin
        match ts' with
        | [] -> Const True
        | [App(Const Not, t1); t2] when t1 = t2 -> Const False
        | [t1; App(Const Not, t2)] when t1 = t2 -> Const False
        | t'::ts'' -> List.fold_left make_and t' ts''
      end
  | App(App(Const Or, _), _) as t ->
      let rec decomp = function
        | App(App(Const Or, t1), t2) -> decomp t1 @@@ decomp t2
        | t -> [normalize_bool_term ~imply t]
      in
      let rec aux ts1 = function
        | [] -> ts1
        | t::ts2 ->
            if imply (ts1@@@ts2) (make_not t)
            then aux ts1 ts2
            else aux (t::ts1) ts2
      in
      let ts' = aux [] (decomp t) in
      begin
        match ts' with
        | [] -> Const False
        | t'::ts'' -> List.fold_left make_or t' ts''
      end
  | App(App(Const (EqInt|Lt|Gt|Leq|Geq) as op, t1), t2) ->
      let neg xs = List.map (fun (x,n) -> x,-n) xs in
      let rec decomp = function
        | Const (Int n) -> [None, n]
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
              | true, true ->
                  raise NonLinear
              | false, true ->
                  let k = reduce xns1 in
                  List.rev_map (fun (x,n) -> x,n*k) xns2
              | true, false ->
                  let k = reduce xns2 in
                  List.rev_map (fun (x,n) -> x,n*k) xns1
              | false, false ->
                  [None, reduce xns1 + reduce xns2]
            end
        | _ ->
            unsupported @@ Format.asprintf "CEGAR_util.normalize_bool_term: %a" CEGAR_print.term @@ make_app op [t1;t2]
      in
      let xns1 = decomp t1 in
      let xns2 = decomp t2 in
      let xns =
        let compare (x1,_) (x2,_) =
          let aux = function
            | None -> true, ""
            | Some (Var x) -> false, x
            | _ -> assert false
          in
          compare (aux x2) (aux x1)
        in
        List.sort ~cmp:compare (xns1 @@@ neg xns2)
      in
      let d = List.fold_left (fun d (_,n) -> Math.gcd d (abs n)) 0 xns in
      if d = 0
      then Const True
      else
        let xns' = List.map (fun (x,n) -> assert (n mod d = 0); x, n/d) xns in
        let xns'' =
          let rec aux = function
            | [] -> []
            | (x,n)::xns ->
                let xns1,xns2 = List.partition (fun (y,_) -> x=y) xns in
                let n' = List.fold_left (fun acc (_,n) -> acc+n) 0 ((x,n)::xns1) in
                (x,n') :: aux xns2
          in
          aux xns'
        in
        let xns''' = List.filter (fun (_,n) -> n<>0) xns'' in
        if xns''' = []
        then
          match op with
          | Const (EqInt | Leq | Geq) -> Const True
          | Const (Lt | Gt) -> Const False
          | _ -> assert false
        else
          let x,n = List.hd xns''' in
          let xns = List.rev @@ List.tl xns''' in
          let op',t1',t2' =
            let aux = function
              | None, n -> Const (Int n)
              | Some x, 1 -> x
              | Some x, n -> make_mul (make_int n) x
            in
            let t1,xns',op' =
              if n<0
              then
                let op' =
                  match op with
                  | Const EqInt -> Const EqInt
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
            let t2 =
              match ts with
              | [] -> Const (Int 0)
              | t::ts' ->
                  let make_add_sub t1 t2 =
                    match t2 with
                    | Const (Int n) when n < 0 -> make_sub t1 (make_int (-n))
                    | App(App(Const Mul, Const (Int n)), t2') when n < 0 -> make_sub t1 (make_mul (make_int (-n)) t2')
                    | _ -> make_add t1 t2
                  in
                  List.fold_left make_add_sub t ts'
            in
            op', t1, t2
          in
          make_app op' [t1'; t2']
  | App(t1, t2) -> App(normalize_bool_term ~imply t1, normalize_bool_term ~imply t2)
  | Let _ -> assert false
  | Fun(x,typ,t) -> Fun(x, typ, normalize_bool_term ~imply t)
let normalize_bool_term ?(imply = fun env t -> List.mem t env) t =
  try
    normalize_bool_term ~imply t
  with NonLinear -> t




let assoc_fun_def defs f =
  let make_fun xs t =
    let xs' = List.map rename_id xs in
    let map = List.map2 (fun x x' -> x, Var x') xs xs' in
    List.fold_right (fun x t -> Fun(x,None,t)) xs' (subst_map map t)
  in
  let defs' = List.filter (fun (g,_,_,_,_) -> f = g) defs in
  match defs' with
  | [_,xs,Const True,_,t] -> make_fun xs t
  | [_] -> raise (Fatal "Not implemented: CEGAR_abst_CPS.assoc_fun_def")
  | [_,xs1,t11,_,t12; _,xs2,t21,_,t22] when make_not t11 = t21 ->
      assert (xs1 = xs2);
      make_fun xs1 (make_if t11 t12 t22)
  | [_,xs1,t11,_,t12; _,xs2,t21,_,t22] when t11 = make_not t21 ->
      assert (xs1 = xs2);
      make_fun xs1 (make_if t21 t22 t12)
  | _ -> Format.eprintf "LENGTH[%s]: %d@." f @@ List.length defs'; assert false

let get_nonrec defs main orig_fun_list force =
  let check (f,xs,t1,e,t2) =
    let defs' = List.filter (fun (g,_,_,_,_) -> f = g) defs in
    let used = List.count (fun (_,_,_,_,t2) -> List.mem f @@ get_fv t2) defs in
    List.for_all (fun (_,_,t1,e,_) -> e = []) defs' &&
    f <> main &&
    (List.for_all (fun (_,xs,_,_,t2) -> List.Set.subset (get_fv t2) xs) defs' ||
     (1 >= used || List.mem f force) &&
     2 >= List.length defs')
  in
  let defs' = List.filter check defs in
  let non_rec = List.rev_map (fun (f,_,_,_,_) -> f, assoc_fun_def defs f) defs' in
  if !Flag.expand_nonrec_init
  then non_rec
  else
    let orig_fun_list' = List.Set.diff orig_fun_list force in
    List.filter_out (fun (f,_) -> List.mem f orig_fun_list') non_rec


let print_prog_typ' orig_fun_list force fm {env;defs;main;info} =
  let non_rec = get_nonrec defs main orig_fun_list force in
  let env' = List.filter_out (fun (f,_) -> List.mem_assoc f non_rec) env in
  CEGAR_print.prog_typ fm {env=env';defs;main;info}



let eval_step_by_step  prog =
  let assoc_fun_def defs f =
    let make_fun xs t =
      let xs' = List.map rename_id xs in
      let map = List.map2 (fun x x' -> x, Var x') xs xs' in
      List.fold_right (fun x t -> Fun(x,None,t)) xs' (subst_map map t)
    in
    let defs' = List.filter (fun (g,_,_,_,_) -> f = g) defs in
    match defs' with
    | [_,xs,Const True,_,t] -> make_fun xs t
    | [_] -> raise (Fatal "Not implemented: CEGAR_abst_CPS.assoc_fun_def")
    | [_,xs1,t11,_,t12; _,xs2,t21,_,t22] when make_not t11 = t21 ->
        assert (xs1 = xs2);
        make_fun xs1 (make_if t11 t12 t22)
    | [_,xs1,t11,_,t12; _,xs2,t21,_,t22] when t11 = make_not t21 ->
        assert (xs1 = xs2);
        make_fun xs1 (make_if t21 t22 t12)
    | [_,xs1,Const True,_,t12; _,xs2,Const True,_,t22] -> make_fun xs1 @@ make_br t22 t12
    | _ -> Format.printf "LENGTH[%s]: %d@." f @@ List.length defs'; assert false
  in
  let counter = ref 0 in
  let rec get_tf l_then l_else =
    Format.printf "[%d] t/f?" !counter;
    if Option.is_some l_then then Format.printf " (t -> %d?, " @@ Option.get l_then;
    if Option.is_some l_else then Format.printf "f -> %d?)" @@ Option.get l_else;
    Format.printf ": @?";
    match read_line () with
    | "t" -> Format.printf "t@."; true
    | "f" -> Format.printf "f@."; false
    | _ -> get_tf l_then l_else
  in
  let rec eval_cond t t_then t_else =
    match t with
    | Const True -> true
    | Const False -> false
    | Const (Rand(TBool,_)) ->
        let aux = function App(Const (Label n), _) -> Some n | _ -> None in
        incr counter; get_tf (aux t_then) (aux t_else)
    | App(App(App(Const If, t1), t2), t3) ->
        if eval_cond t1 t_then t_else then eval_cond t2 t_then t_else else eval_cond t3 t_then t_else
    | _ -> assert false
  in
  let rec eval t =
    if true then Color.printf Color.Red "EVAL:@.";
    if true then Format.printf "%a@." CEGAR_print.term t;
(*    ignore @@ read_line ();*)
    match decomp_app t with
    | Const If, [t1;t2;t3] ->
        if eval_cond t1 t2 t3
        then eval t2
        else eval t3
    | Const (Label n), [t] ->
        Color.printf Color.Green "Label %d@." n;
        if false then ignore @@ read_line ();
        eval t
    | Fun(x,_,t1), t2::ts ->
        if true then Color.printf Color.Blue "[%s |-> %a]@." x CEGAR_print.term t2;
        eval @@ make_app (subst x t2 t1) ts
    | Var f, ts ->
        let subst' x t1 t2 =
          if true then Color.printf Color.Blue "[%s |-> %a]@." x CEGAR_print.term t1;
          subst x t1 t2
        in
        let xs,t' = decomp_fun @@ assoc_fun_def prog.defs f in
        eval @@ List.fold_right2 subst' xs ts t'
    | _ ->
        Color.printf Color.Blue "%a@." CEGAR_print.term t;
        assert false
  in
  let t_main = assoc_fun_def prog.defs prog.main in
  eval t_main


let initialize_env prog = {prog with env=[]}


let rec has_no_effect t =
  match t with
  | Const c -> true
  | Var y -> true
  | App(t1,t2) -> false
  | Let(y,t1,t2) -> has_no_effect t1 && has_no_effect t2
  | Fun(y,typ,t1) -> true




let assign_id_to_rand prog =
  let count = ref 0 in
  let rec aux t =
    match t with
    | Const (Rand(TInt, None)) -> Const (Rand(TInt, None))
    | Const (Rand(TInt, Some _)) -> incr count; Const (Rand(TInt, Some !count))
    | Const c -> Const c
    | Var x -> Var x
    | App(t1,t2) -> App(aux t1, aux t2)
    | Let(x,t1,t2) -> Let(x, aux t1, aux t2)
    | Fun(x,typ,t) -> Fun(x, typ, aux t)
  in
  let prog' = map_body_prog aux prog in
  let env = make_renv !count prog' @ prog.env in
  {prog' with env}


let make_map_randint_to_preds prog =
  let env' = List.filter (is_randint_var -| fst) prog.env in
  let aux (f,typ) =
    let i = Option.get @@ decomp_randint_name f in
    let _,xs,_,_,_ = List.find (fun (_,_,_,_,t) -> List.mem i @@ col_rand_ids t) prog.defs in
    let _,preds = decomp_rand_typ ~xs:(Some (List.map _Var xs)) typ in
    i, preds
  in
  List.map aux env'

let conv_path ext_ce =
  let aux = List.map (List.map (fun b -> if b then Positive else Negative)) in
  List.map (fun (n,bs) -> (n, aux bs)) ext_ce

let rec arrange_ext_preds_sequence = function
  | [] -> []
  | (r,bs)::ext ->
      let (rs, rest) = List.partition (fun (f, _) -> f=r) ext in
      (r,bs::List.map snd rs) :: arrange_ext_preds_sequence rest

let is_same_branching (_,_,ce1,_) (_,_,ce2,_) = ce1 = ce2

let rec group_by_same_branching = function
  | [] -> []
  | x::xs -> let (gr, rest) = List.partition (is_same_branching x) xs in (x :: gr) :: group_by_same_branching rest

(*
r1: [Positive, Negative, Positive], r2: [Positive, Negative]
r1: [Positive, Negative, Negative], r2: [Positive, Negative]
->
(r1, 0, Positive), (r1, 1, Negative), (r1, 2, Positive), (r2, 0, Positive), (r2, 1, Negative)
(r1, 0, Positive), (r1, 1, Negative), (r1, 2, Negative), (r2, 0, Positive), (r2, 1, Negative)

*)
let remove_meaningless_pred path1 path2 =
  let bs1 = List.concat @@ List.concat_map
    (fun (n1, bss1) ->
      List.mapi (fun i bs -> List.mapi (fun j b -> (n1, i, j, b)) bs) bss1) path1 in
  let bs2 = List.concat @@ List.concat_map
    (fun (n2, bss2) ->
      List.mapi (fun i bs -> List.mapi (fun j b -> (n2, i, j, b)) bs) bss2) path2 in
  let aux acc (n', i', j', b1) (_, _, _, b2) =
    if b1 <> b2 then
      let modify = List.map (fun ((n, bss) as seq) ->
	if n=n' then (n, List.mapi (fun i bs -> if i=i' then List.mapi (fun j b -> if j=j' then Do_not_Care else b) bs else bs) bss)
	else seq)
      in match acc with
	| None -> Some(modify)
	| Some(modify') -> Some(modify |- modify')
    else
      acc
  in
  match List.fold_left2 aux None bs1 bs2 with
    | None -> None
    | Some(modify) -> Some(modify path1)

let rec found_and_merge_paths (_,_,_,path) = function
  | [] -> None
  | ((path',orig_ce,ce,ext_path) as p)::ps ->
    match remove_meaningless_pred path ext_path with
      | None ->
	begin
	  match found_and_merge_paths p ps with
	    | None -> None
	    | Some(merged_path, rest) -> Some(merged_path, p::rest)
	end
      | Some(merged) -> Some((path', orig_ce, ce, merged), ps)

let rec merge_similar_paths_aux = function
  | [] -> []
  | p::ps ->
    match found_and_merge_paths p ps with
      | None -> p :: merge_similar_paths_aux ps
      | Some(merged_path, rest) -> merged_path :: merge_similar_paths_aux rest

let rec merge_similar_paths l =
  let l' = merge_similar_paths_aux l in
  if List.length l = List.length l' then l else merge_similar_paths l'

let inlined_functions orig_fun_list force {defs;main} =
  List.unique @@ List.map fst @@ get_nonrec defs main orig_fun_list force
