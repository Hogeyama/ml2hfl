
open Syntax
open Type
open Utilities



let make_let' f xs t1 t2 =
  match xs,t1.desc with
      [r], App({desc=Var _} as k, [{desc=Var r'}]) when r = r' -> subst f k t2
    | _ -> make_let [f, xs, t1] t2




let rec trans_exc_typ = function
    TUnit -> TUnit
  | TBool -> TBool
  | TAbsBool -> TAbsBool
  | TInt -> TInt
  | TRInt p -> TRInt p
  | TVar _ -> assert false
  | TFun(x,typ) ->
      let typ1 = trans_exc_typ (Id.typ x) in
      let typ2 = trans_exc_typ typ in
      let y = Id.new_var "x" typ2 in
      let k = Id.new_var "k" (TFun(y,TUnit)) in
      let e = Id.new_var "e" !typ_excep in
      let h = Id.new_var "h" (TFun(e,TUnit)) in
        TFun(Id.set_typ x typ1, TFun(k,TFun(h,TUnit)))
  | TList typ -> TList (trans_exc_typ typ)
  | TConstr(s,b) -> TConstr(s,b)
  | TPred(typ,ps) -> TPred(trans_exc_typ typ, ps)
  | TPair(typ1,typ2) -> TPair(trans_exc_typ typ1, trans_exc_typ typ2)

let trans_exc_var x = Id.set_typ x (trans_exc_typ (Id.typ x))

let rec trans_exc ct ce t =
  match t.desc with
      Unit
    | True
    | False
    | Int _
    | NInt _ -> ct t
    | RandInt false ->
        let r = Id.new_var "r" TInt in
        let k = Id.new_var "k" (TFun(r,TUnit)) in
          make_let [k, [r], ct (make_var r)] (make_app (make_var k) [randint_term])
    | RandInt _ -> assert false
    | Var x -> ct (make_var (trans_exc_var x))
    | Fun(x, t) ->
(*
        let f = Id.new_var "f" t.typ in
          trans_exc ct ce (make_let f [x] t (make_var f))
*)
        let x' = trans_exc_var x in
        let r = Id.new_var "r" (trans_exc_typ t.typ) in
        let k = Id.new_var "k" (TFun(r,TUnit)) in
        let e = Id.new_var "e" !typ_excep in
        let h = Id.new_var "h" (TFun(e,TUnit)) in
        let ct' y = make_app (make_var k) [y] in
        let ce' y = make_app (make_var h) [y] in
          ct (make_fun x' (make_fun k (make_fun h (trans_exc ct' ce' t))))
    | App(_, []) -> assert false
    | App(t1, [t2]) ->
        let r = Id.new_var "r" (trans_exc_typ t.typ) in
        let k = Id.new_var "k" (TFun(r,TUnit)) in
        let e = Id.new_var "e" !typ_excep in
        let h = Id.new_var "h" (TFun(e,TUnit)) in
        let ct' x = trans_exc (fun y -> make_app x [y; make_var  k; make_var h]) ce t2 in
        let t2' = trans_exc ct' ce t1 in
          make_let [k, [r], ct (make_var r)] (make_let [h, [e], ce (make_var e)] t2')
    | App(t1, t2::ts) ->
        let x,typ = match t1.typ with TFun(x,typ) -> x,typ | _ -> assert false in
          trans_exc ct ce {desc=App({desc=App(t1,[t2]);typ=typ}, ts); typ=t.typ}
    | If(t1, t2, t3) ->
        let x = Id.new_var "x" t.typ in
        let k = Id.new_var "k" (TFun(x,TUnit)) in
        let ct' y = make_app (make_var k) [y] in
        let t2' = trans_exc ct' ce t2 in
        let t3' = trans_exc ct' ce t3 in
        let ct'' y = make_let [k, [x], ct (make_var x)] (make_if y t2' t3') in
          trans_exc ct'' ce t1
(*
    | Let(flag, bindings, t2) ->
        let aux (f,xs,t1) t2 =
          match flag,xs with
              Flag.Nonrecursive, [] ->
                let ct' t = subst x t (trans_exc ct ce t2) in
                  trans_exc ct' ce t1
            | Flag.Recursive, [] -> assert false
            | _, [x] ->
                let x' = trans_exc_var x in
                let r = Id.new_var "r" (trans_exc_typ t1.typ) in
                let k = Id.new_var "k" (TFun(r,TUnit)) in
                let e = Id.new_var "e" !typ_excep in
                let h = Id.new_var "h" (TFun(e,TUnit)) in
                let f' = trans_exc_var f in
                let ct' y = make_app (make_var k) [y] in
                let ce' y = make_app (make_var h) [y] in
                let t1' = trans_exc ct' ce' t1 in
                let t2' = trans_exc ct ce t2 in
                  make_let_f flag f' [x';k;h] t1' t2'
            | _, x::xs ->
                let typ = match Id.typ f with TFun(_,typ) -> typ | _ -> assert false in
                let g = Id.new_var (Id.name f) typ in
                let t1' = make_let g xs t1 (make_var g) in
                  trans_exc ct ce (make_let_f flag [f, [x], t1'] t2)
*)
    | Let(Flag.Nonrecursive, [x, [], t1], t2) ->
        let ct' t = subst x t (trans_exc ct ce t2) in
          trans_exc ct' ce t1
    | Let(Flag.Recursive, [f, [], t1], t2) -> assert false
    | Let(flag, [f, [x], t1], t2) ->
        let x' = trans_exc_var x in
        let r = Id.new_var "r" (trans_exc_typ t1.typ) in
        let k = Id.new_var "k" (TFun(r,TUnit)) in
        let e = Id.new_var "e" !typ_excep in
        let h = Id.new_var "h" (TFun(e,TUnit)) in
        let f' = trans_exc_var f in
        let ct' y = make_app (make_var k) [y] in
        let ce' y = make_app (make_var h) [y] in
        let t1' = trans_exc ct' ce' t1 in
        let t2' = trans_exc ct ce t2 in
          make_let_f flag [f', [x';k;h], t1'] t2'
    | Let(flag, [f, x::xs, t1], t2) ->
        let typ = match Id.typ f with TFun(_,typ) -> typ | _ -> assert false in
        let g = Id.new_var (Id.name f) typ in
        let t1' = make_let [g, xs, t1] (make_var g) in
          trans_exc ct ce (make_let_f flag [f, [x], t1'] t2)
    | BinOp(op, t1, t2) ->
        let ct1 t1' t2' = ct {desc=BinOp(op, t1', t2'); typ=t.typ} in
        let ct2 y1 = trans_exc (fun y2 -> ct1 y1 y2) ce t2 in
          trans_exc ct2 ce t1
    | Not t ->
        let ct' t1 = ct (make_not t1) in
          trans_exc ct' ce t
    | Unknown -> ct {desc=Unknown;typ=t.typ}
    | Event(s,false) -> ct (make_event_cps s)
    | Nil -> ct (make_nil (t.typ))
    | Cons(t1, t2) ->
        let ct1 t1' t2' = ct {desc=Cons(t1', t2'); typ=t.typ} in
        let ct2 y1 = trans_exc (fun y2 -> ct1 y1 y2) ce t2 in
          trans_exc ct2 ce t1
    | Constr(cstr,[]) -> ct {desc=Constr(cstr,[]);typ=t.typ}
    | Constr(cstr,t1::ts) ->
        let x = Id.new_var "x" t.typ in
        let k = Id.new_var "k" (TFun(x,TUnit)) in
        let aux t1 t2 =
          match t2.desc with
              App({desc=Var k}, [{desc=Constr(c,ts)}]) ->
                make_app (make_var k) [{desc=Constr(c,t1::ts);typ=t.typ}]
            | _ -> assert false
        in
        let ct1 x = make_app (make_var k) [{desc=Constr(cstr,[x]);typ=t.typ}] in
        let ct' = List.fold_right (fun t ct -> fun x -> trans_exc (fun y -> ct (aux x y)) ce t) ts ct1 in
          make_let [k, [x], ct (make_var x)] (trans_exc ct' ce t1)
    | Match(t1,pats) ->
        let x = Id.new_var "x" t.typ in
        let k = Id.new_var "k" (TFun(x,TUnit)) in
        let ct' y = make_app (make_var k) [y] in
        let aux (pat,c,t) =
          assert (c = true_term);
          pat, c, trans_exc ct' ce t
        in
        let pats' = List.map aux pats in
        let ct'' y = make_let [k, [x], ct (make_var x)] {desc=Match(y,pats');typ=TUnit} in
          trans_exc ct'' ce t1
    | Raise t -> trans_exc ce ce t
    | TryWith(t1,{desc=Fun(x,t2)}) ->
        let ce' e = subst x e (trans_exc ct ce t2) in
          trans_exc ct ce' t1
    | Bottom -> make_bottom TUnit
    | Pair(t1,t2) ->
        let ct1 t1' t2' = ct (make_pair t1' t2') in
        let ct2 y1 = trans_exc (fun y2 -> ct1 y1 y2) ce t2 in
          trans_exc ct2 ce t1
    | Fst t ->
        let ct' t1 = ct (make_fst t1) in
          trans_exc ct' ce t
    | Snd t ->
        let ct' t1 = ct (make_snd t1) in
          trans_exc ct' ce t
    | _ -> Format.printf "%a@." pp_print_term t; assert false


type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree

let root x =
  match x with
      Leaf t -> t
    | Node _ -> assert false
let rec flatten = function
    Leaf f -> [f]
  | Node(lhs,rhs) -> flatten lhs @ flatten rhs

let rec map f path = function
    Leaf t -> Leaf (f path t)
  | Node(t1,t2) -> Node(map f (path@[1]) t1, map f (path@[2]) t2)
let map f t = map f [] t

let rec fold f_node f_leaf = function
    Leaf typ -> f_leaf typ
  | Node(t1,t2) -> f_node (fold f_node f_leaf t1) (fold f_node f_leaf t2)

let rec proj path t =
  match path,t with
      [],_ -> t
    | 1::path',Node(t',_) -> proj path' t'
    | 2::path',Node(_,t') -> proj path' t'
    | _ -> assert false


let rec remove_pair_typ = function
    TUnit -> Leaf TUnit
  | TBool -> Leaf TBool
  | TAbsBool -> Leaf TAbsBool
  | TInt -> Leaf TInt
  | TRInt p -> Leaf (TRInt p)
  | TVar _ -> assert false
  | TFun _ as typ ->
      let xs,typ' = decomp_tfun typ in
      let aux x = flatten (remove_pair_var x) in
      let xs' = List.flatten (List.map aux xs) in
        Leaf (List.fold_right (fun x typ -> TFun(x,typ)) xs' typ')
  | TPair(typ1,typ2) -> Node(remove_pair_typ typ1, remove_pair_typ typ2)
  | TList typ -> Leaf (TList (root (remove_pair_typ typ)))
  | TConstr(s,b) -> Leaf (TConstr(s,b))
  | TPred(TPair(typ1,typ2),ps) ->
      let ps' = List.map remove_pair ps in
        remove_pair_typ (TPair(TPred(typ1,ps'),typ2))
  | TPred(typ,ps) ->
      let ps' = List.map remove_pair ps in
      let typ' =
        match remove_pair_typ typ with
            Leaf typ -> typ
          | Node _ -> raise (Fatal "Not implemented CPS.remove_pair_typ(TPred)")
      in
        Leaf (TPred(typ', ps'))

and remove_pair_var x =
  let to_string path = List.fold_left (fun acc i -> acc ^ string_of_int i) "" path in
  let aux path typ = Id.set_typ (Id.add_name x (to_string path)) typ in
    map aux (remove_pair_typ (Id.typ x))

and remove_pair_aux t typ_opt =
  let typ = match typ_opt with None -> t.typ | Some typ -> typ in
  let typs = remove_pair_typ typ in
    match t.desc with
        Unit
      | True
      | False
      | Int _
      | NInt _
      | RandInt _
      | Event _ -> Leaf t
      | Bottom -> map (fun _ -> make_bottom) typs
      | Var x -> map (fun _ x -> make_var x) (remove_pair_var x)
      | Fun(x, t) ->
          let xs = flatten (remove_pair_var x) in
          let t' = root (remove_pair_aux t None) in
            Leaf (List.fold_right make_fun xs t')
      | App(t1, ts) ->
          let typs = get_argtyps t1.typ in
          let typs' = take typs (List.length ts) in
          let t' = root (remove_pair_aux t1 None) in
          let ts' = List.flatten (List.map2 (fun t typ -> flatten (remove_pair_aux t (Some typ))) ts typs') in
            Leaf (make_app t' ts')
      | If(t1, t2, t3) ->
          let t1' = root (remove_pair_aux t1 None) in
          let t2' = root (remove_pair_aux t2 None) in
          let t3' = root (remove_pair_aux t3 None) in
            Leaf (make_if t1' t2' t3')
      | Branch(t1, t2) ->
          let t1' = root (remove_pair_aux t1 None) in
          let t2' = root (remove_pair_aux t2 None) in
            Leaf {desc=Branch(t1',t2'); typ=t1'.typ}
      | Let(flag, [f, xs, t1], t2) ->
          let f' = root (remove_pair_var f) in
          let xs' = List.flatten (List.map (fun x -> flatten (remove_pair_var x)) xs) in
          let t1' = root (remove_pair_aux t1 None) in
          let t2' = root (remove_pair_aux t2 None) in
            Leaf (make_let_f flag [f', xs', t1'] t2')
      | Let _ -> assert false
      | BinOp(op, t1, t2) ->
          let t1' = root (remove_pair_aux t1 None) in
          let t2' = root (remove_pair_aux t2 None) in
            Leaf {desc=BinOp(op, t1', t2'); typ=root typs}
      | Not t1 ->
          let t1' = root (remove_pair_aux t1 None) in
            Leaf (make_not t1')
      | Record fields -> assert false
      | Proj(i,s,f,t1) -> assert false
      | SetField(n,i,s,f,t1,t2) -> assert false
      | Nil -> assert false
      | Cons(t1,t2) -> assert false
      | Constr(s,ts) -> assert false
      | Match(t1,pats) -> assert false
      | TryWith(t1,t2) -> assert false
      | Pair(t1,t2) -> Node(remove_pair_aux t1 None, remove_pair_aux t2 None)
      | Fst {desc=Var x} when x = abst_var -> Leaf (make_var x) (* for predicates *)
      | Fst t ->
          let t' =
            match remove_pair_aux t None with
                Leaf _ -> Format.printf "%a@." pp_print_term t;assert false
              | Node(t',_) -> t'
          in
            t'
      | Snd {desc=Var x} when x = abst_var -> Leaf (make_var x) (* for predicates *)
      | Snd t ->
          let t' =
            match remove_pair_aux t None with
                Leaf _ -> assert false
              | Node(_,t') -> t'
          in
            t'
      | _ -> (Format.printf "%a@." pp_print_term t; assert false)

and remove_pair t =
  let t' = root (remove_pair_aux t None) in
  let () = Type_check.check t' TUnit in
    t'
























let counter = ref 0
let new_evar () = incr counter; !counter

type typed_term = {t_cps:t_cps; typ_cps:typ_cps; typ_orig:typ; effect:effect_var}
and typed_ident = {id_cps:id; id_typ:typ_cps}
and t_cps =
    UnitCPS
  | TrueCPS
  | FalseCPS
  | UnknownCPS
  | IntCPS of int
  | BottomCPS
  | RandIntCPS
  | VarCPS of typed_ident
  | FunCPS of typed_ident * typed_term
  | AppCPS of typed_term * typed_term
  | IfCPS of typed_term * typed_term * typed_term
  | LetCPS of Flag.rec_flag * (typed_ident * typed_term) list * typed_term
  | BinOpCPS of binop * typed_term * typed_term
  | NotCPS of typed_term
  | LabelCPS of bool * typed_term
  | EventCPS of string
  | FstCPS of typed_term
  | SndCPS of typed_term
  | PairCPS of typed_term * typed_term
and typ_cps =
    TBaseCPS of Syntax.typ
  | TFunCPS of effect_var * typ_cps * typ_cps
  | TPairCPS of typ_cps * typ_cps
  | TPredCPS of typ_cps * Syntax.typed_term list
and effect = EUnkown (* for debug *) | ENone | ECont | EExcepHandler
and effect_var = int
and effect_constr =
    CGeq of effect_var * effect
  | CGeqVar of effect_var * effect_var

let effect_max x y =
  match x, y with
      EUnkown, _
    | _, EUnkown -> assert false
    | ENone, _ -> y
    | ECont, EExcepHandler -> EExcepHandler
    | ECont, _ -> ECont
    | EExcepHandler, _ -> EExcepHandler

let should_insert e = e <> ENone || !Flag.cps_simpl

let sol = ref (fun (n:int) -> EUnkown)

let rec print_typ_cps' fm = function
    TBaseCPS typ -> Format.fprintf fm "%a" Syntax.print_typ typ
  | TFunCPS(e,typ1,typ2) ->
      Format.fprintf fm "(%a -e%d-> %a)" print_typ_cps' typ1 e print_typ_cps' typ2
  | TPairCPS(typ1,typ2) ->
      Format.fprintf fm "(%a * %a)" print_typ_cps' typ1 print_typ_cps' typ2
  | TPredCPS(typ,ps) ->
      Format.fprintf fm "%a[...])" print_typ_cps' typ

let rec print_typ_cps fm = function
    TBaseCPS typ -> Format.fprintf fm "%a" Syntax.print_typ typ
  | TFunCPS(e,typ1,typ2) when !sol e = EUnkown ->
      Format.fprintf fm "(%a -%a-> %a)" print_typ_cps typ1 print_evar e print_typ_cps typ2
  | TFunCPS(e,typ1,typ2) when !sol e = ENone ->
      Format.fprintf fm "(%a -> %a)" print_typ_cps typ1 print_typ_cps typ2
  | TFunCPS(e,typ1,typ2) when !sol e = ECont ->
      Format.fprintf fm "(%a => %a)" print_typ_cps typ1 print_typ_cps typ2
  | TFunCPS(e,typ1,typ2) when !sol e = EExcepHandler ->
      Format.fprintf fm "(%a -=> %a)" print_typ_cps typ1 print_typ_cps typ2
  | TFunCPS _ -> assert false
  | TPairCPS(typ1,typ2) ->
      Format.fprintf fm "(%a * %a)" print_typ_cps typ1 print_typ_cps typ2
  | TPredCPS(typ,ps) ->
      Format.fprintf fm "%a[...])" print_typ_cps typ


and print_typed_termlist fm = List.iter (fun bd -> Format.fprintf fm "@;%a" print_typed_term bd)

and print_typed_term fm {t_cps=t; typ_cps=typ; effect=e} =
  Format.fprintf fm "(%a : %a)" print_t_cps t print_typ_cps typ

and print_t_cps fm = function
    UnitCPS -> Format.fprintf fm "unit"
  | TrueCPS -> Format.fprintf fm "true"
  | FalseCPS -> Format.fprintf fm "false"
  | UnknownCPS -> Format.fprintf fm "***"
  | IntCPS n -> Format.fprintf fm "%d" n
  | BottomCPS -> Format.fprintf fm "_|_"
  | RandIntCPS -> Format.fprintf fm "rand_int"
  | VarCPS x -> print_id fm x.id_cps
  | FunCPS(x, t) ->
      Format.fprintf fm "fun %a : %a -> %a" print_id x.id_cps print_typ_cps x.id_typ print_typed_term t
  | AppCPS(t1, t2) ->
      Format.fprintf fm "%a%a" print_typed_term t1 print_typed_term t2
  | IfCPS(t1, t2, t3) ->
      Format.fprintf fm "@[@[if %a@]@;then @[%a@]@;else @[%a@]@]"
        print_typed_term t1 print_typed_term t2 print_typed_term t3
  | LetCPS(flag, bindings, t) ->
      let is_rec = match flag with Flag.Nonrecursive -> false | Flag.Recursive -> true in
      let head = ref (if is_rec then "let rec" else "let") in
      let pr fm ((f:typed_ident),(t:typed_term)) =
        Format.fprintf fm "%s %a : %a = @[%a@]@;" !head print_id f.id_cps print_typ_cps f.id_typ print_typed_term t;
        head := "and"
      in
        Format.fprintf fm "@[<v>@[<hov 2>%a@]@;in@;%a@]" (print_list pr "" false) bindings print_typed_term t
  | BinOpCPS(op, t1, t2) ->
      let op =
        match op with
            Eq -> "="
          | Lt -> "<"
          | Gt -> ">"
          | Leq -> "<="
          | Geq -> ">="
          | And -> "&&"
          | Or -> "||"
          | Add -> "+"
          | Sub -> "-"
          | Mult -> "*"
      in
        Format.fprintf fm "%a %s %a" print_typed_term t1 op print_typed_term t2
  | NotCPS t ->
      Format.fprintf fm "not %a" print_typed_term t
  | LabelCPS(true, t) ->
      Format.fprintf fm "l_then %a" print_typed_term t
  | LabelCPS(false, t) ->
      Format.fprintf fm "l_else %a" print_typed_term t
  | EventCPS s -> Format.fprintf fm "{%s}" s
  | FstCPS t ->
      Format.fprintf fm "fst %a" print_typed_term t
  | SndCPS t ->
      Format.fprintf fm "snd %a" print_typed_term t
  | PairCPS(t1, t2) ->
      Format.fprintf fm "(%a, %a)" print_typed_term t1 print_typed_term t2

and print_effect fm = function
    EUnkown -> Format.fprintf fm "EUnknown"
  | ENone -> Format.fprintf fm "ENone"
  | ECont -> Format.fprintf fm "ECont"
  | EExcepHandler -> Format.fprintf fm "EExcepHandler"

and print_evar fm x = Format.fprintf fm "e%d" x

let print_econstr fm = function
    CGeqVar(x, y) -> Format.fprintf fm "%a :> %a" print_evar x print_evar y
  | CGeq(x, e) -> Format.fprintf fm "%a :> %a" print_evar x print_effect e

let constraints = ref []

let rec unify typ1 typ2 =
  match typ1, typ2 with
      TBaseCPS typ1, TBaseCPS typ2 -> ()
    | TFunCPS(x1,typ11,typ12), TFunCPS(x2,typ21,typ22) ->
        constraints := CGeqVar(x1, x2) :: CGeqVar(x2, x1) :: !constraints;
        unify typ11 typ21;
        unify typ12 typ22
    | TPairCPS(typ11,typ12), TPairCPS(typ21,typ22) ->
        unify typ11 typ21;
        unify typ12 typ22
    | TPredCPS(typ1,_), typ2
    | typ1, TPredCPS(typ2,_) -> unify typ1 typ2
    | typ1,typ2 ->
        Format.printf "Bug?@.typ1: %a@.typ2: %a@."
          print_typ_cps typ1 print_typ_cps typ2;
        assert false

let rec lift_letrec_typ typed =
  match typed.t_cps, typed.typ_cps with
      FunCPS(_, ({t_cps=FunCPS _} as typed1)), _ ->
        lift_letrec_typ typed1
    | FunCPS _, TFunCPS(e, _, _) ->
        constraints := CGeq(e, ECont) :: !constraints
    | FunCPS _, _ -> assert false
    | _ -> ()

let rec infer_effect_typ typ =
  match typ with
      TUnit
    | TInt _
    | TBool -> TBaseCPS typ
    | TFun(x,typ2) ->
        let typ1 = Id.typ x in
        let e = new_evar () in
          (match typ2 with TFun _ -> () | _ -> constraints := CGeq(e, ECont) :: !constraints);
          TFunCPS(e, infer_effect_typ typ1, infer_effect_typ typ2)
    | TPair(typ1,typ2) -> TPairCPS(infer_effect_typ typ1, infer_effect_typ typ2)
    | TPred(typ,ps) -> TPredCPS(infer_effect_typ typ, ps)
    | _ -> Format.printf "%a@." print_typ typ; assert false

let new_var x = {id_cps=x; id_typ=infer_effect_typ (Id.typ x)}

let rec infer_effect env t =
  match t.desc with
      Unit -> {t_cps=UnitCPS; typ_cps=TBaseCPS t.typ; typ_orig=t.typ; effect=new_evar()}
    | True -> {t_cps=TrueCPS; typ_cps=TBaseCPS t.typ; typ_orig=t.typ; effect=new_evar()}
    | False -> {t_cps=FalseCPS; typ_cps=TBaseCPS t.typ; typ_orig=t.typ; effect=new_evar()}
    | Unknown -> assert false (*{t_cps=UnknownCPS; typ_cps=TBaseCPS t.typ; typ_orig=t.typ}*)
    | Int n -> {t_cps=IntCPS n; typ_cps=TBaseCPS t.typ; typ_orig=t.typ; effect=new_evar()}
    | Bottom ->
        let e = new_evar () in
          constraints := CGeq(e, ECont) :: !constraints;
          {t_cps=BottomCPS; typ_cps=infer_effect_typ t.typ; typ_orig=t.typ; effect=e}
    | NInt x -> assert false
    | RandInt true -> assert false
    | RandInt false ->
        let e = new_evar () in
        let typ = TFunCPS(e, TBaseCPS TUnit, TBaseCPS TInt) in
          constraints := CGeq(e, ECont) :: !constraints;
          {t_cps=RandIntCPS; typ_cps=typ; typ_orig=t.typ; effect=new_evar()}
    | Var x ->
        let typ =
	  try
	    List.assoc (Id.to_string x) env
	  with
	      Not_found when is_external x -> infer_effect_typ t.typ
	    | Not_found when is_parameter x -> TBaseCPS(TInt)
	    | Not_found -> Format.printf "%a@." print_id x; assert false
        in
          {t_cps=VarCPS{id_cps=x;id_typ=typ}; typ_cps=typ; typ_orig=t.typ; effect=new_evar()}
    | Fun(x, t1) ->
        let x_typ = infer_effect_typ (Id.typ x) in
        let x' = {id_cps=x; id_typ=x_typ} in
        let env' = (Id.to_string x, x_typ) :: env in
        let typed = infer_effect env' t1 in
        let typ' = infer_effect_typ t.typ in
        let e,r_typ = match typ' with TFunCPS(e,_,typ) -> e,typ | _ -> assert false in
          constraints := CGeqVar(e, typed.effect) :: !constraints;
          unify r_typ typed.typ_cps;
          {t_cps=FunCPS(x',typed); typ_cps=typ'; typ_orig=t.typ; effect=new_evar()}
    | App(t1, []) -> assert false
    | App(t1, t2::t3::ts) ->
        let typ = (make_app t1 [t2]).typ in
          infer_effect env {desc=App({desc=App(t1,[t2]);typ=typ}, t3::ts); typ=t.typ}
    | App(t1, [t2]) ->
        let typed1 = infer_effect env t1 in
        let typed2 = infer_effect env t2 in
        let typ_result = infer_effect_typ t.typ in
        let e0 = new_evar () in
        let typ = TFunCPS(e0, typed2.typ_cps, typ_result) in
        let e = new_evar () in
          constraints := CGeqVar(e, typed1.effect) :: !constraints;
          constraints := CGeqVar(e, typed2.effect) :: !constraints;
          constraints := CGeqVar(e, e0) :: !constraints;
          unify typed1.typ_cps typ;
          {t_cps=AppCPS(typed1,typed2); typ_cps=typ_result; typ_orig=t.typ; effect=e}
    | If(t1, t2, t3) ->
        let typed1 = infer_effect env t1 in
        let typed2 = infer_effect env t2 in
        let typed3 = infer_effect env t3 in
        let e = new_evar () in
(*
          constraints := CGeqVar(e, typed1.effect) :: !constraints;
          constraints := CGeqVar(e, typed2.effect) :: !constraints;
          constraints := CGeqVar(e, typed3.effect) :: !constraints;
*)
          constraints := CGeq(e, ECont) :: !constraints;
          unify typed1.typ_cps (TBaseCPS TBool);
          unify typed2.typ_cps typed3.typ_cps;
          {t_cps=IfCPS(typed1,typed2,typed3); typ_cps=typed2.typ_cps; typ_orig=t.typ; effect=e}
    | Let(flag, bindings, t1) ->
        let make_env (f,_,_) = Id.to_string f, infer_effect_typ (Id.typ f) in
        let env_f = List.map make_env bindings in
        let env' = env_f @@ env in
        let env'' = match flag with Flag.Nonrecursive -> env | Flag.Recursive -> env' in
        let aux (f, xs, t1) =
          let f' = {id_cps=f; id_typ=List.assoc (Id.to_string f) env_f} in
          let t1' = List.fold_right make_fun xs t1 in
          let typed = infer_effect env'' t1' in
          let () =
            match flag with
                Flag.Nonrecursive -> ()
              | Flag.Recursive -> lift_letrec_typ typed
          in
            unify f'.id_typ typed.typ_cps;
            f', typed
        in
        let bindings' = List.map aux bindings in
        let typed = infer_effect env' t1 in
        let aux (_,typed) e =
          let e' = new_evar () in
            constraints := CGeqVar(e', typed.effect) :: !constraints;
            constraints := CGeqVar(e', e) :: !constraints;
            e'
        in
        let e = List.fold_right aux bindings' typed.effect in
          {t_cps=LetCPS(flag, bindings', typed); typ_cps=typed.typ_cps; typ_orig=t.typ; effect=e}
    | BinOp(op, t1, t2) ->
        let typed1 = infer_effect env t1 in
        let typed2 = infer_effect env t2 in
        let _ = unify typed1.typ_cps typed2.typ_cps in
        let e = new_evar () in
          constraints := CGeqVar(e, typed1.effect) :: !constraints;
          constraints := CGeqVar(e, typed2.effect) :: !constraints;
          {t_cps=BinOpCPS(op,typed1,typed2); typ_cps=TBaseCPS t.typ; typ_orig=t.typ; effect=e}
    | Not t1 ->
        let typed = infer_effect env t1 in
        let _ = unify typed.typ_cps (TBaseCPS t.typ) in
          {t_cps=NotCPS typed; typ_cps=TBaseCPS t.typ; typ_orig=t.typ; effect=typed.effect}
    | Event(s,true) -> assert false
    | Event(s,false) ->
        let e = new_evar () in
        let typ = TFunCPS(e, TBaseCPS TUnit, TBaseCPS TUnit) in
          constraints := CGeq(e, ECont) :: !constraints;
          {t_cps=EventCPS s; typ_cps=typ; typ_orig=t.typ; effect=new_evar()}
    | Fst t1 ->
        let typed = infer_effect env t1 in
        let typ = infer_effect_typ t1.typ in
        let typ1 = match typ with TPairCPS(typ1,_) -> typ1 | _ -> assert false in
          unify typed.typ_cps typ;
          {t_cps=FstCPS typed; typ_cps=typ1; typ_orig=t.typ; effect=typed.effect}
    | Snd t1 ->
        let typed = infer_effect env t1 in
        let typ = infer_effect_typ t1.typ in
        let typ2 = match typ with TPairCPS(_,typ2) -> typ2 | _ -> assert false in
          unify typed.typ_cps typ;
          {t_cps=SndCPS typed; typ_cps=typ2; typ_orig=t.typ; effect=typed.effect}
    | Pair(t1,t2) ->
        let typed1 = infer_effect env t1 in
        let typed2 = infer_effect env t2 in
        let typ = TPairCPS(typed1.typ_cps,typed2.typ_cps) in
        let e = new_evar () in
          constraints := CGeqVar(e, typed1.effect) :: !constraints;
          constraints := CGeqVar(e, typed2.effect) :: !constraints;
          {t_cps=PairCPS(typed1,typed2); typ_cps=typ; typ_orig=t.typ; effect=e}
    | TryWith (_, _) -> assert false
    | Raise _ -> assert false
    | Match (_, _) -> assert false
    | Constr (_, _) -> assert false
    | Cons (_, _) -> assert false
    | SetField (_, _, _, _, _, _) -> assert false
    | Proj (_, _, _, _) -> assert false
    | Record _ -> assert false
    | Branch (_, _) -> assert false
    | RandValue (_, _) -> assert false
    | Nil -> assert false


exception Loop of effect_var list

let solve_constraints constrs =
  if false then
    begin
      Format.printf "@.CONSTRAINTS:@.";
      List.iter (Format.printf " %a@." print_econstr) constrs;
      Format.printf "@."
    end;
  let num = !counter + 1 in
  let tbl = Array.make num [] in
  let sol = Array.make num None in
  let cgeqs,cgeqvars = List.partition (function CGeq _ -> true | CGeqVar _ -> false) constrs in
  let cgeqvars' = List.map (function CGeqVar(x,y) -> x,y | _ -> assert false) cgeqvars in
  let cgeqvars'' = List.filter (fun (x,y) -> x <> y) cgeqvars' in
  let cgeqs' = List.map (function CGeq(x,e) -> x,e | _ -> assert false) cgeqs in
  let () = List.iter (fun (x,y) -> tbl.(x) <- `CVar y::tbl.(x)) cgeqvars'' in
  let () = List.iter (fun (x,e) -> tbl.(x) <- `CConst e::tbl.(x)) cgeqs' in
  let rec elim_loop x =
    if x < num
    then
      let rec reachable x y acc =
        if x = y && acc <> [] then raise (Loop acc);
        let xs = rev_map_flatten (function `CConst e -> [] | `CVar y -> [y]) tbl.(x) in
        let xs = List.filter (fun x -> not (List.mem x acc)) xs in
          List.iter (fun x -> reachable x y (x::acc)) xs
      in
        try
          reachable x x [];
          elim_loop (x+1)
        with Loop xs ->
          assert (List.hd xs = x);
          if false then
            begin
              Format.printf " LOOP:";
              List.iter (Format.printf " %a" print_evar) xs;
              Format.printf "@."
            end;
          let xs' = List.tl xs in
          let cs = rev_map_flatten (fun x -> tbl.(x)) xs in
          let cs = List.filter (fun c -> not (List.exists (fun y -> c = `CVar y) xs)) cs in
            tbl.(x) <- cs;
            List.iter (fun y -> tbl.(y) <- [`CVar x]) xs';
  if false then
    begin
      Format.printf "eliminate:@.";
      Array.iteri (fun i xs ->
                    Format.printf " %a :>" print_evar i;
                    List.iter (function
                                   `CVar x -> Format.printf " %a" print_evar x
                                 | `CConst e -> Format.printf " %a" print_effect e) xs;
                    Format.printf "@."
                  )
        tbl;
      Format.printf "@."
    end;
            elim_loop x
  in
  if false then
    begin
      Format.printf "before eliminate:@.";
      Array.iteri (fun i xs ->
                    Format.printf " %a :>" print_evar i;
                    List.iter (function
                                   `CVar x -> Format.printf " %a" print_evar x
                                 | `CConst e -> Format.printf " %a" print_effect e) xs;
                    Format.printf "@."
                  )
        tbl;
      Format.printf "@."
    end;
  let () = elim_loop 0 in
  if false then
    begin
      Format.printf "CONSTRAINTS:@.";
      Array.iteri (fun i xs ->
                    Format.printf " %a :>" print_evar i;
                    List.iter (function
                                   `CVar x -> Format.printf " %a" print_evar x
                                 | `CConst e -> Format.printf " %a" print_effect e) xs;
                    Format.printf "@."
                  )
        tbl;
      Format.printf "@."
    end;
  let rec solve x =
    match sol.(x) with
        None ->
          let es = List.map (function `CConst e -> e | `CVar y -> solve y) tbl.(x) in
          let e = List.fold_right effect_max es ENone in
            sol.(x) <- Some e; e
      | Some e -> e
  in
  let () = Array.iteri (fun x _ -> ignore (solve x)) tbl in
    fun e ->
      match sol.(e) with
          None -> ENone
        | Some e -> e





let rec get_arg_num = function
    TBaseCPS _ -> 0
  | TFunCPS(e,typ1,typ2) when should_insert (!sol e) -> 1
  | TFunCPS(_,typ1,typ2) -> 1 + get_arg_num typ2
  | TPairCPS _ -> assert false
  | TPredCPS _ -> assert false


let rec app_typ typ typs =
  match typ,typs with
      TFunCPS(_,_,typ2), _::typs' -> app_typ typ2 typs'
    | _, [] -> typ
    | _ -> raise (Fatal "bug? (CPS.app_typ)")




let rec add_preds_cont_aux k t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | NInt y -> NInt y
      | RandInt b -> RandInt b
      | RandValue(typ,b) -> RandValue(typ,b)
      | Var y -> Var y
      | Fun(y, t) -> Fun(y, add_preds_cont_aux k t)
      | App(t1, ts) ->
          let aux t (typ,ts) =
            let x, typ' =
              match typ with
                  TFun(x,typ) -> x, subst_type x t typ
                | _ -> assert false
            in
            let t' =
              if t.desc = Var k
              then make_var (Id.set_typ k (Id.typ x))
              else add_preds_cont_aux k t
            in
              typ', t'::ts
          in
          let _,ts' = List.fold_right aux ts (t1.typ,[]) in
            App(add_preds_cont_aux k t1, ts')
      | If(t1, t2, t3) -> If(add_preds_cont_aux k t1, add_preds_cont_aux k t2, add_preds_cont_aux k t3)
      | Branch(t1, t2) -> Branch(add_preds_cont_aux k t1, add_preds_cont_aux k t2)
      | Let(flag, bindings, t2) ->
          let bindings' = List.map (fun (f,xs,t) -> f, xs, add_preds_cont_aux k t) bindings in
          let t2' = add_preds_cont_aux k t2 in
            Let(flag, bindings', t2')
      | BinOp(op, t1, t2) -> BinOp(op, add_preds_cont_aux k t1, add_preds_cont_aux k t2)
      | Not t1 -> Not (add_preds_cont_aux k t1)
      | Event(s,b) -> Event(s,b)
      | Record fields ->  Record (List.map (fun (f,(s,t1)) -> f,(s,add_preds_cont_aux k t1)) fields)
      | Proj(i,s,f,t1) -> Proj(i,s,f,add_preds_cont_aux k t1)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,add_preds_cont_aux k t1,add_preds_cont_aux k t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(add_preds_cont_aux k t1, add_preds_cont_aux k t2)
      | Constr(s,ts) -> Constr(s, List.map (add_preds_cont_aux k) ts)
      | Match(t1,pats) ->
          let aux (pat,cond,t1) = pat, add_preds_cont_aux k cond, add_preds_cont_aux k t1 in
            Match(add_preds_cont_aux k t1, List.map aux pats)
      | Raise t -> Raise (add_preds_cont_aux k t)
      | TryWith(t1,t2) -> TryWith(add_preds_cont_aux k t1, add_preds_cont_aux k t2)
      | Pair(t1,t2) -> Pair(add_preds_cont_aux k t1, add_preds_cont_aux k t2)
      | Fst t -> Fst(add_preds_cont_aux k t)
      | Snd t -> Snd(add_preds_cont_aux k t)
      | Bottom -> Bottom
  in
    {desc=desc; typ=t.typ}

let add_preds_cont k t =
  let t' = add_preds_cont_aux k t in
  let ks = List.filter (Id.same k) (get_fv t') in
  Format.printf "APC: %a, %a ===> %a@." Id.print k pp_print_term t pp_print_term t';
    if List.length ks = 0
    then (assert (t.desc = Bottom); k, t')
    else (assert (List.length ks = 1); List.hd ks, t')



let rec trans_typ typ_orig typ =
  match typ_orig,typ with
    | _, TBaseCPS _ -> typ_orig
    | TFun(x_orig,typ), TFunCPS(e,typ1,typ2) when should_insert (!sol e) ->
        let typ1' = trans_typ (Id.typ x_orig) typ1 in
        let x = Id.new_var "x" typ1' in
        let typ2' = subst_type x_orig (make_var x) (trans_typ typ typ2) in
        let r = Id.new_var "r" typ2' in
        let k = Id.new_var "k" (TFun(r,TUnit)) in
          TFun(x, TFun(k, TUnit))
    | TFun(x_orig,typ), TFunCPS(_,typ1,typ2) ->
        let typ1' = trans_typ (Id.typ x_orig) typ1 in
        let x = Id.new_var "x" typ1' in
        let typ2' = subst_type x_orig (make_var x) (trans_typ typ typ2) in
          TFun(x, typ2')
    | TPair(typ_orig1,typ_orig2), TPairCPS(typ1,typ2) ->
        TPair(trans_typ typ_orig1 typ1, trans_typ typ_orig2 typ2)
    | TPred(typ1,ps), typ2 -> TPred(trans_typ typ1 typ2, ps)
    | _ ->
        Format.printf "%a,%a@." print_typ typ_orig print_typ_cps typ;
        raise (Fatal "bug? (CPS.trans_typ)")

let trans_var x = Id.set_typ x.id_cps (trans_typ (Id.typ x.id_cps) x.id_typ)

let get_tfun_effect = function
    TFunCPS(e, _, _) -> e
  | _ -> assert false

let make_app_cont e t k =
  match !sol e with
      EUnkown -> assert false
    | ENone -> make_app k [t]
    | ECont -> make_app t [k]
    | EExcepHandler -> assert false

let rec transform k_post {t_cps=t; typ_cps=typ; typ_orig=typ_orig; effect=e} =
  match t, !sol e with
      UnitCPS, ENone -> unit_term
    | TrueCPS, ENone -> true_term
    | FalseCPS, ENone -> false_term
    | IntCPS n, ENone -> make_int n
    | BottomCPS, ECont ->
        let r = Id.new_var "r" (trans_typ typ_orig typ) in
        let k = Id.new_var ("k" ^ k_post) (TFun(r,TUnit)) in
          make_fun k (make_bottom TUnit)
    | RandIntCPS, ENone -> make_randint_cps TUnit
    | VarCPS x, ENone -> make_var (trans_var x)
    | FunCPS(x, t1), ENone when !sol (get_tfun_effect typ) = ENone ->
        let x' = trans_var x in
          make_fun x' (transform k_post t1)
    | FunCPS(x, t1), ENone when !sol (get_tfun_effect typ) = ECont ->
        let x' = trans_var x in
        let r = Id.new_var "r" (trans_typ t1.typ_orig t1.typ_cps) in
        let k = Id.new_var ("k" ^ k_post) (TFun(r,TUnit)) in
        let t1' = transform k_post t1 in
          make_fun x' (make_fun k (make_app_cont t1.effect t1' (make_var k)))
    | AppCPS(t1, t2), ENone ->
        let t1' = transform k_post t1 in
        let t2' = transform k_post t2 in
          make_app t1' [t2']
    | AppCPS(t1, t2), ECont ->
        let t1' = transform k_post t1 in
        let t2' = transform k_post t2 in
        let r = Id.new_var "r" (trans_typ typ_orig typ) in
        let k = Id.new_var ("k" ^ k_post) (TFun(r,TUnit)) in
        let x1 = Id.new_var "x" (trans_typ t1.typ_orig t1.typ_cps) in
        let x2 = Id.new_var "x" (trans_typ t2.typ_orig t2.typ_cps) in
        let e0 = get_tfun_effect t1.typ_cps in
          make_fun k (make_app_cont t1.effect t1' (make_fun x1 (make_app_cont t2.effect t2' (make_fun x2
            (make_app_cont e0 (make_app (make_var x1) [make_var x2]) (make_var k))))))
    | IfCPS(t1, t2, t3), ENone ->
        let t1' = transform k_post t1 in
        let t2' = transform k_post t2 in
        let t3' = transform k_post t3 in
          make_if t1' t2' t3'
    | IfCPS(t1, t2, t3), ECont ->
        let t1' = transform k_post t1 in
        let t2' = transform k_post t2 in
        let t3' = transform k_post t3 in
        let r = Id.new_var "r" (trans_typ typ_orig typ) in
        let k = Id.new_var ("k" ^ k_post) (TFun(r,TUnit)) in
        let k' = Id.new_var ("k" ^ k_post) (TFun(r,TUnit)) in
        let b = Id.new_var "b" (trans_typ t1.typ_orig t1.typ_cps) in
          make_let [k', [], make_var k]
            (make_fun k
               (make_app_cont t1.effect t1'
                  (make_fun b
                     (make_if (make_var b)
                        (make_app_cont t2.effect t2' (make_var k'))
                        (make_app_cont t3.effect t3' (make_var k'))))))
    | LetCPS(flag, bindings, t1), ENone ->
        let aux (f,t) =
          let f' = trans_var f in
            f', [], transform (k_post ^ "_" ^ Id.name f') t
        in
        let bindings' = List.map aux bindings in
        let t1' = transform k_post t1 in
          make_let_f flag bindings' t1'
    | LetCPS(flag, bindings, t1), ECont ->
        let r = Id.new_var "r" (trans_typ typ_orig typ) in
        let k = Id.new_var ("k" ^ k_post) (TFun(r,TUnit)) in
        let xs = List.map (fun (_,t) -> Id.new_var "x" (trans_typ t.typ_orig t.typ_cps)) bindings in
        let bindings' = List.map2 (fun x (f,_) -> trans_var f, [], make_var x) xs bindings in
        let aux (f,t) =
          let f' = trans_var f in
            t.effect, transform (k_post ^ "_" ^ Id.name f') t
        in
        let ets = List.map aux bindings in
        let t1' = transform k_post t1 in
        let t0 = make_let_f flag bindings' (make_app_cont t1.effect t1' (make_var k)) in
          make_fun k (List.fold_right2 (fun x (e,t) t' -> make_app_cont e t (make_fun x t')) xs ets t0)
    | BinOpCPS(op, t1, t2), ENone ->
        let t1' = transform k_post t1 in
        let t2' = transform k_post t2 in
          {desc=BinOp(op, t1', t2'); typ=typ_orig}
    | BinOpCPS(op, t1, t2), ECont ->
        let r = Id.new_var "r" (trans_typ typ_orig typ) in
        let k = Id.new_var ("k" ^ k_post) (TFun(r,TUnit)) in
        let x1 = Id.new_var "x" (trans_typ t1.typ_orig t1.typ_cps) in
        let x2 = Id.new_var "x" (trans_typ t2.typ_orig t2.typ_cps) in
        let t1' = transform k_post t1 in
        let t2' = transform k_post t2 in
          make_fun k (make_app_cont t1.effect t1' (make_fun x1 (make_app_cont t2.effect t2' (make_fun x2
            (make_app (make_var k) [{desc=BinOp(op, make_var x1, make_var x2); typ=typ_orig}])))))
    | NotCPS t1, ENone ->
        let t1' = transform k_post t1 in
          make_not t1'
    | NotCPS t1, ECont ->
        let r = Id.new_var "r" (trans_typ typ_orig typ) in
        let k = Id.new_var ("k" ^ k_post) (TFun(r,TUnit)) in
        let b = Id.new_var "b" (trans_typ t1.typ_orig t1.typ_cps) in
        let t1' = transform k_post t1 in
          make_fun k (make_app_cont t1.effect t1' (make_fun b (make_app (make_var k) [make_not (make_var b)])))
    | UnknownCPS, _ -> assert false
    | EventCPS s, ENone -> make_event_cps s
    | FstCPS t1, ENone ->
        let t1' = transform k_post t1 in
          make_fst t1'
    | FstCPS t1, ECont ->
        let r = Id.new_var "r" (trans_typ typ_orig typ) in
        let k = Id.new_var ("k" ^ k_post) (TFun(r,TUnit)) in
        let p = Id.new_var "p" (trans_typ t1.typ_orig t1.typ_cps) in
        let t1' = transform k_post t1 in
          make_fun k (make_app_cont t1.effect t1' (make_fun p (make_app (make_var k) [make_fst (make_var p)])))
    | SndCPS t1, ENone ->
        let t1' = transform k_post t1 in
          make_snd t1'
    | SndCPS t1, ECont ->
        let r = Id.new_var "r" (trans_typ typ_orig typ) in
        let k = Id.new_var ("k" ^ k_post) (TFun(r,TUnit)) in
        let p = Id.new_var "p" (trans_typ t1.typ_orig t1.typ_cps) in
        let t1' = transform k_post t1 in
          make_fun k (make_app_cont t1.effect t1' (make_fun p (make_app (make_var k) [make_snd (make_var p)])))
    | PairCPS(t1,t2), ENone ->
        let t1' = transform k_post t1 in
        let t2' = transform k_post t2 in
          make_pair t1' t2'
    | PairCPS(t1,t2), ECont ->
        let r = Id.new_var "r" (trans_typ typ_orig typ) in
        let k = Id.new_var ("k" ^ k_post) (TFun(r,TUnit)) in
        let x1 = Id.new_var "x" (trans_typ t1.typ_orig t1.typ_cps) in
        let x2 = Id.new_var "x" (trans_typ t2.typ_orig t2.typ_cps) in
        let t1' = transform k_post t1 in
        let t2' = transform k_post t2 in
          make_fun k (make_app_cont t1.effect t1' (make_fun x1 (make_app_cont t2.effect t2' (make_fun x2
            (make_app (make_var k) [make_pair (make_var x1) (make_var x2)])))))
    | t, e -> (Format.printf "%a, %a@." print_t_cps t print_effect e; assert false)


let transform t =
  let x = Id.new_var "x" TUnit in
  let t = transform "" t in
  let t = make_app t [make_fun x (make_var x)] in
    Trans.propagate_typ_arg t









let rec short_circuit_eval t =
  let desc =
    match t.desc with
        Unit
      | True
      | False
      | Unknown
      | Int _
      | NInt _
      | RandInt _
      | RandValue _
      | Var _ -> t.desc
      | Fun(y, t) -> Fun(y, short_circuit_eval t)
      | App(t1, ts) -> App(short_circuit_eval t1, List.map short_circuit_eval ts)
      | If(t1, t2, t3) -> If(short_circuit_eval t1, short_circuit_eval t2, short_circuit_eval t3)
      | Branch(t1, t2) -> Branch(short_circuit_eval t1, short_circuit_eval t2)
      | Let(flag, bindings, t2) ->
          let bindings' = List.map (fun (f,xs,t) -> f, xs, short_circuit_eval t) bindings in
            Let(flag, bindings', short_circuit_eval t2)
      | BinOp(And, t1, t2) -> If(short_circuit_eval t1, short_circuit_eval t2, false_term)
      | BinOp(Or, t1, t2) -> If(short_circuit_eval t1, true_term, short_circuit_eval t2)
      | BinOp(op, t1, t2) -> BinOp(op, short_circuit_eval t1, short_circuit_eval t2)
      | Not t1 -> Not (short_circuit_eval t1)
      | Event(s,b) -> Event(s,b)
      | Record fields -> Record (List.map (fun (f,(s,t1)) -> f,(s,short_circuit_eval t1)) fields)
      | Proj(i,s,f,t1) -> Proj(i,s,f,short_circuit_eval t1)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,short_circuit_eval t1,short_circuit_eval t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(short_circuit_eval t1,short_circuit_eval t2)
      | Constr(s,ts) -> Constr(s, List.map short_circuit_eval ts)
      | Match(t1,pats) ->
          let aux (pat,cond,t1) = pat, short_circuit_eval cond, short_circuit_eval t1 in
            Match(short_circuit_eval t1, List.map aux pats)
      | Raise t -> Raise (short_circuit_eval t)
      | TryWith(t1,t2) -> TryWith(short_circuit_eval t1, short_circuit_eval t2)
      | Pair(t1,t2) -> Pair(short_circuit_eval t1, short_circuit_eval t2)
      | Fst t -> Fst (short_circuit_eval t)
      | Snd t -> Snd (short_circuit_eval t)
      | Bottom -> Bottom
  in
    {desc=desc; typ=t.typ}





let trans1 t =
  let t = short_circuit_eval t in
  let t =
    let () = counter := 0 in
    let () = constraints := [] in
    let typed = infer_effect [] t in
      constraints := CGeq(typed.effect, ECont) :: !constraints;
      if false then Format.printf "CPS_infer_effect:@.%a@." print_typed_term typed;
      sol := solve_constraints !constraints;
      if false then Format.printf "CPS_infer_effect:@.%a@." print_typed_term typed;
      transform typed
  in
  let t = Trans.eta_reduce t in
  let t = Trans.expand_let_val t in
    Type_check.check t TUnit;
    t





























(*
let make_let' f xs t1 t2 =
  match xs,t1.desc with
      [r], App({desc=Var _} as k, [{desc=Var r'}]) when r = r' -> subst f k t2
    | _ -> make_let [f, xs, t1] t2




let rec trans_exc_typ = function
    TUnit -> TUnit
  | TBool -> TBool
  | TAbsBool -> TAbsBool
  | TInt -> TInt
  | TRInt p -> TRInt p
  | TVar _ -> assert false
  | TFun(x,typ) ->
      let typ1 = trans_exc_typ (Id.typ x) in
      let typ2 = trans_exc_typ typ in
      let y = Id.new_var "x" typ2 in
      let k = Id.new_var "k" (TFun(y,TUnit)) in
      let e = Id.new_var "e" !typ_excep in
      let h = Id.new_var "h" (TFun(e,TUnit)) in
        TFun(Id.set_typ x typ1, TFun(k,TFun(h,TUnit)))
  | TList typ -> TList (trans_exc_typ typ)
  | TConstr(s,b) -> TConstr(s,b)
  | TUnknown -> assert false
  | TVariant _ -> assert false
  | TPred(typ,ps) -> TPred(trans_exc_typ typ, ps)
  | TPair(typ1,typ2) -> TPair(trans_exc_typ typ1, trans_exc_typ typ2)

let trans_exc_var x = Id.set_typ x (trans_exc_typ (Id.typ x))

let rec trans_exc ct ce t =
  match t.desc with
      Unit
    | True
    | False
    | Int _
    | NInt _ -> ct t
    | RandInt false ->
        let r = Id.new_var "r" TInt in
        let k = Id.new_var "k" (TFun(r,TUnit)) in
          make_let [k, [r], ct (make_var r)] (make_app (make_var k) [randint_term])
    | RandInt _ -> assert false
    | Var x -> ct (make_var (trans_exc_var x))
    | Fun(x, t) ->
(*
        let f = Id.new_var "f" t.typ in
          trans_exc ct ce (make_let f [x] t (make_var f))
*)
        let x' = trans_exc_var x in
        let r = Id.new_var "r" (trans_exc_typ t.typ) in
        let k = Id.new_var "k" (TFun(r,TUnit)) in
        let e = Id.new_var "e" !typ_excep in
        let h = Id.new_var "h" (TFun(e,TUnit)) in
        let ct' y = make_app (make_var k) [y] in
        let ce' y = make_app (make_var h) [y] in
          ct (make_fun x' (make_fun k (make_fun h (trans_exc ct' ce' t))))
    | App(_, []) -> assert false
    | App(t1, [t2]) ->
        let r = Id.new_var "r" (trans_exc_typ t.typ) in
        let k = Id.new_var "k" (TFun(r,TUnit)) in
        let e = Id.new_var "e" !typ_excep in
        let h = Id.new_var "h" (TFun(e,TUnit)) in
        let ct' x = trans_exc (fun y -> make_app x [y; make_var  k; make_var h]) ce t2 in
        let t2' = trans_exc ct' ce t1 in
          make_let [k, [r], ct (make_var r)] (make_let [h, [e], ce (make_var e)] t2')
    | App(t1, t2::ts) ->
        let x,typ = match t1.typ with TFun(x,typ) -> x,typ | _ -> assert false in
          trans_exc ct ce {desc=App({desc=App(t1,[t2]);typ=typ}, ts); typ=t.typ}
    | If(t1, t2, t3) ->
        let x = Id.new_var "x" t.typ in
        let k = Id.new_var "k" (TFun(x,TUnit)) in
        let ct' y = make_app (make_var k) [y] in
        let t2' = trans_exc ct' ce t2 in
        let t3' = trans_exc ct' ce t3 in
        let ct'' y = make_let [k, [x], ct (make_var x)] (make_if y t2' t3') in
          trans_exc ct'' ce t1
(*
    | Let(flag, bindings, t2) ->
        let aux (f,xs,t1) t2 =
          match flag,xs with
              Flag.Nonrecursive, [] ->
                let ct' t = subst x t (trans_exc ct ce t2) in
                  trans_exc ct' ce t1
            | Flag.Recursive, [] -> assert false
            | _, [x] ->
                let x' = trans_exc_var x in
                let r = Id.new_var "r" (trans_exc_typ t1.typ) in
                let k = Id.new_var "k" (TFun(r,TUnit)) in
                let e = Id.new_var "e" !typ_excep in
                let h = Id.new_var "h" (TFun(e,TUnit)) in
                let f' = trans_exc_var f in
                let ct' y = make_app (make_var k) [y] in
                let ce' y = make_app (make_var h) [y] in
                let t1' = trans_exc ct' ce' t1 in
                let t2' = trans_exc ct ce t2 in
                  make_let_f flag f' [x';k;h] t1' t2'
            | _, x::xs ->
                let typ = match Id.typ f with TFun(_,typ) -> typ | _ -> assert false in
                let g = Id.new_var (Id.name f) typ in
                let t1' = make_let g xs t1 (make_var g) in
                  trans_exc ct ce (make_let_f flag [f, [x], t1'] t2)
*)
    | Let(Flag.Nonrecursive, [x, [], t1], t2) ->
        let ct' t = subst x t (trans_exc ct ce t2) in
          trans_exc ct' ce t1
    | Let(Flag.Recursive, [f, [], t1], t2) -> assert false
    | Let(flag, [f, [x], t1], t2) ->
        let x' = trans_exc_var x in
        let r = Id.new_var "r" (trans_exc_typ t1.typ) in
        let k = Id.new_var "k" (TFun(r,TUnit)) in
        let e = Id.new_var "e" !typ_excep in
        let h = Id.new_var "h" (TFun(e,TUnit)) in
        let f' = trans_exc_var f in
        let ct' y = make_app (make_var k) [y] in
        let ce' y = make_app (make_var h) [y] in
        let t1' = trans_exc ct' ce' t1 in
        let t2' = trans_exc ct ce t2 in
          make_let_f flag [f', [x';k;h], t1'] t2'
    | Let(flag, [f, x::xs, t1], t2) ->
        let typ = match Id.typ f with TFun(_,typ) -> typ | _ -> assert false in
        let g = Id.new_var (Id.name f) typ in
        let t1' = make_let [g, xs, t1] (make_var g) in
          trans_exc ct ce (make_let_f flag [f, [x], t1'] t2)
    | BinOp(op, t1, t2) ->
        let ct1 t1' t2' = ct {desc=BinOp(op, t1', t2'); typ=t.typ} in
        let ct2 y1 = trans_exc (fun y2 -> ct1 y1 y2) ce t2 in
          trans_exc ct2 ce t1
    | Not t ->
        let ct' t1 = ct (make_not t1) in
          trans_exc ct' ce t
    | Unknown -> ct {desc=Unknown;typ=t.typ}
    | Event(s,false) -> ct (make_event_cps s)
    | Nil -> ct (make_nil (t.typ))
    | Cons(t1, t2) ->
        let ct1 t1' t2' = ct {desc=Cons(t1', t2'); typ=t.typ} in
        let ct2 y1 = trans_exc (fun y2 -> ct1 y1 y2) ce t2 in
          trans_exc ct2 ce t1
    | Constr(cstr,[]) -> ct {desc=Constr(cstr,[]);typ=t.typ}
    | Constr(cstr,t1::ts) ->
        let x = Id.new_var "x" t.typ in
        let k = Id.new_var "k" (TFun(x,TUnit)) in
        let aux t1 t2 =
          match t2.desc with
              App({desc=Var k}, [{desc=Constr(c,ts)}]) -> make_app (make_var k) [{desc=Constr(c,t1::ts);typ=t.typ}]
            | _ -> assert false
        in
        let ct1 x = make_app (make_var k) [{desc=Constr(cstr,[x]);typ=t.typ}] in
        let ct' = List.fold_right (fun t ct -> fun x -> trans_exc (fun y -> ct (aux x y)) ce t) ts ct1 in
          make_let [k, [x], ct (make_var x)] (trans_exc ct' ce t1)
    | Match(t1,pats) ->
        let x = Id.new_var "x" t.typ in
        let k = Id.new_var "k" (TFun(x,TUnit)) in
        let ct' y = make_app (make_var k) [y] in
        let aux (pat,c,t) =
          assert (c = None);
          pat, None, trans_exc ct' ce t
        in
        let pats' = List.map aux pats in
        let ct'' y = make_let [k, [x], ct (make_var x)] {desc=Match(y,pats');typ=TUnit} in
          trans_exc ct'' ce t1
    | Raise t -> trans_exc ce ce t
    | TryWith(t1,{desc=Fun(x,t2)}) ->
        let ce' e = subst x e (trans_exc ct ce t2) in
          trans_exc ct ce' t1
    | Bottom -> make_bottom TUnit
    | Pair(t1,t2) ->
        let ct1 t1' t2' = ct (make_pair t1' t2') in
        let ct2 y1 = trans_exc (fun y2 -> ct1 y1 y2) ce t2 in
          trans_exc ct2 ce t1
    | Fst t ->
        let ct' t1 = ct (make_fst t1) in
          trans_exc ct' ce t
    | Snd t ->
        let ct' t1 = ct (make_snd t1) in
          trans_exc ct' ce t
    | _ -> Format.printf "%a@." pp_print_term t; assert false


let root x =
  match x with
      Leaf t -> t
    | Node _ -> assert false
let rec flatten = function
    Leaf f -> [f]
  | Node(lhs,rhs) -> flatten lhs @ flatten rhs

let rec map f path = function
    Leaf t -> Leaf (f path t)
  | Node(t1,t2) -> Node(map f (path@[1]) t1, map f (path@[2]) t2)
let map f t = map f [] t

let rec fold f_node f_leaf = function
    Leaf typ -> f_leaf typ
  | Node(t1,t2) -> f_node (fold f_node f_leaf t1) (fold f_node f_leaf t2)

let rec proj path t =
  match path,t with
      [],_ -> t
    | 1::path',Node(t',_) -> proj path' t'
    | 2::path',Node(_,t') -> proj path' t'
    | _ -> assert false


let rec remove_pair_typ = function
    TUnit -> Leaf TUnit
  | TBool -> Leaf TBool
  | TAbsBool -> Leaf TAbsBool
  | TInt -> Leaf TInt
  | TRInt p -> Leaf (TRInt p)
  | TVar _ -> assert false
  | TFun _ as typ ->
      let xs,typ' = decomp_tfun typ in
      let aux x = flatten (remove_pair_var x) in
      let xs' = List.flatten (List.map aux xs) in
        Leaf (List.fold_right (fun x typ -> TFun(x,typ)) xs' typ')
  | TPair(typ1,typ2) -> Node(remove_pair_typ typ1, remove_pair_typ typ2)
  | TList typ -> Leaf (TList (root (remove_pair_typ typ)))
  | TConstr(s,b) -> Leaf (TConstr(s,b))
  | TUnknown -> Leaf TUnknown
  | TVariant _ -> assert false
  | TPred(TPair(typ1,typ2),ps) ->
      let ps' = List.map remove_pair ps in
        remove_pair_typ (TPair(TPred(typ1,ps'),typ2))
  | TPred(typ,ps) ->
      let ps' = List.map remove_pair ps in
      let typ' =
        match remove_pair_typ typ with
            Leaf typ -> typ
          | Node _ -> raise (Fatal "Not implemented CPS.remove_pair_typ(TPred)")
      in
        Leaf (TPred(typ', ps'))

and remove_pair_var x =
  let to_string path = List.fold_left (fun acc i -> acc ^ string_of_int i) "" path in
  let aux path typ = Id.set_typ (Id.add_name x (to_string path)) typ in
    map aux (remove_pair_typ (Id.typ x))

and remove_pair_aux t typ_opt =
  let typ = match typ_opt with None -> t.typ | Some typ -> typ in
  let typs = remove_pair_typ typ in
    match t.desc with
        Unit
      | True
      | False
      | Int _
      | NInt _
      | RandInt _
      | Event _ -> Leaf t
      | Bottom -> map (fun _ -> make_bottom) typs
      | Var x -> map (fun _ x -> make_var x) (remove_pair_var x)
      | Fun(x, t) ->
          let xs = flatten (remove_pair_var x) in
          let t' = root (remove_pair_aux t None) in
            Leaf (List.fold_right make_fun xs t')
      | App(t, ts) ->
          let typs = get_argtyps t.typ in
          let typs' = take typs (List.length ts) in
          let t' = root (remove_pair_aux t None) in
          let ts' = List.flatten (List.map2 (fun t typ -> flatten (remove_pair_aux t (Some typ))) ts typs') in
            Leaf (make_app t' ts')
      | If(t1, t2, t3) ->
          let t1' = root (remove_pair_aux t1 None) in
          let t2' = root (remove_pair_aux t2 None) in
          let t3' = root (remove_pair_aux t3 None) in
            Leaf (make_if t1' t2' t3')
      | Branch(t1, t2) ->
          let t1' = root (remove_pair_aux t1 None) in
          let t2' = root (remove_pair_aux t2 None) in
            Leaf {desc=Branch(t1',t2'); typ=t1'.typ}
      | Let(flag, [f, xs, t1], t2) ->
          let f' = root (remove_pair_var f) in
          let xs' = List.flatten (List.map (fun x -> flatten (remove_pair_var x)) xs) in
          let t1' = root (remove_pair_aux t1 None) in
          let t2' = root (remove_pair_aux t2 None) in
            Leaf (make_let_f flag [f', xs', t1'] t2')
      | Let _ -> assert false
      | BinOp(op, t1, t2) ->
          let t1' = root (remove_pair_aux t1 None) in
          let t2' = root (remove_pair_aux t2 None) in
            Leaf {desc=BinOp(op, t1', t2'); typ=root typs}
      | Not t1 ->
          let t1' = root (remove_pair_aux t1 None) in
            Leaf (make_not t1')
      | Record fields -> assert false
      | Proj(i,s,f,t1) -> assert false
      | SetField(n,i,s,f,t1,t2) -> assert false
      | Nil -> assert false
      | Cons(t1,t2) -> assert false
      | Constr(s,ts) -> assert false
      | Match(t1,pats) -> assert false
      | TryWith(t1,t2) -> assert false
      | Pair(t1,t2) -> Node(remove_pair_aux t1 None, remove_pair_aux t2 None)
      | Fst {desc=Var x} when x = abst_var -> Leaf (make_var x) (* for predicates *)
      | Fst t ->
          let t' =
            match remove_pair_aux t None with
                Leaf _ -> Format.printf "%a@." pp_print_term t;assert false
              | Node(t',_) -> t'
          in
            t'
      | Snd {desc=Var x} when x = abst_var -> Leaf (make_var x) (* for predicates *)
      | Snd t ->
          let t' =
            match remove_pair_aux t None with
                Leaf _ -> assert false
              | Node(_,t') -> t'
          in
            t'
      | _ -> (Format.printf "%a@." pp_print_term t; assert false)

and remove_pair t =
  let t' = root (remove_pair_aux t None) in
  let () = Type_check.check t' TUnit in
    t'


























type typed_term_ = {t_cps:t_cps_; typ_cps:typ_cps_; typ_orig:typ}
and typed_ident__ = {id_cps:id; id_typ:typ_cps_}
and t_cps_ =
    UnitCPS
  | TrueCPS
  | FalseCPS
  | UnknownCPS
  | IntCPS of int
  | BottomCPS
  | RandIntCPS
  | VarCPS of typed_ident_
  | FunCPS of typed_ident_ * typed_term_
  | AppCPS of typed_term_ * typed_term_ list
  | IfCPS of typed_term_ * typed_term_ * typed_term_
  | LetCPS of Flag.rec_flag * typed_ident * typed_ident list * typed_term * typed_term
  | BinOpCPS of binop * typed_term * typed_term
  | NotCPS of typed_term
  | LabelCPS of bool * typed_term
  | EventCPS of string
  | FstCPS of typed_term
  | SndCPS of typed_term
  | PairCPS of typed_term * typed_term
and typ_cps =
    TBaseCPS of Syntax.typ
  | TVarCPS of typ_cps option ref
  | TFunCPS of bool ref * typ_cps * typ_cps
  | TPairCPS of typ_cps * typ_cps
  | TPredCPS of typ_cps * Syntax.typed_term list

let should_insert b = !b || !Flag.cps_simpl

let rec print_typ_cps fm = function
    TBaseCPS typ -> Format.fprintf fm "%a" Syntax.print_typ typ
  | TVarCPS {contents = None} -> Format.fprintf fm "?"
  | TVarCPS {contents = Some typ} -> Format.fprintf fm "%a" print_typ_cps typ
  | TFunCPS({contents=b},typ1,typ2) ->
      Format.fprintf fm "(%a %s %a)" print_typ_cps typ1 (if b then "=>" else "->") print_typ_cps typ2
  | TPairCPS(typ1,typ2) ->
      Format.fprintf fm "(%a * %a)" print_typ_cps typ1 print_typ_cps typ2
  | TPredCPS(typ,ps) ->
      Format.fprintf fm "%a[...])" print_typ_cps typ


and print_typed_termlist fm = List.iter (fun bd -> Format.fprintf fm "@;%a" print_typed_term bd)

and print_typed_term fm {t_cps=t; typ_cps=typ} =
  Format.fprintf fm "(%a : %a)" print_t_cps t print_typ_cps typ

and print_t_cps fm = function
    UnitCPS -> Format.fprintf fm "unit"
  | TrueCPS -> Format.fprintf fm "true"
  | FalseCPS -> Format.fprintf fm "false"
  | UnknownCPS -> Format.fprintf fm "***"
  | IntCPS n -> Format.fprintf fm "%d" n
  | BottomCPS -> Format.fprintf fm "_|_"
  | RandIntCPS -> Format.fprintf fm "rand_int()"
  | VarCPS x -> print_id fm x.id_cps
  | FunCPS(x, t) ->
      Format.fprintf fm "fun %a -> %a" print_id x.id_cps print_typed_term t
  | AppCPS(t, ts) ->
      Format.fprintf fm "%a%a" print_typed_term t print_typed_termlist ts
  | IfCPS(t1, t2, t3) ->
      Format.fprintf fm "@[@[if %a@]@;then @[%a@]@;else @[%a@]@]"
        print_typed_term t1 print_typed_term t2 print_typed_term t3
  | LetCPS(flag, f, xs, t1, t2) ->
      let s_rec = match flag with Flag.Nonrecursive -> "" | Flag.Recursive -> " rec" in
      let p_ids fm xs = Format.fprintf fm "%a" print_ids (List.map (fun x -> x.id_cps) xs)
      in
        begin
          match t2.t_cps with
              LetCPS _ ->
                Format.fprintf fm "@[<v>@[<hov 2>let%s %a= @,%a@]@;in@;%a@]"
                  s_rec p_ids (f::xs) print_typed_term t1 print_typed_term t2
            | _ ->
                Format.fprintf fm "@[<v>@[<hov 2>let%s %a= @,%a @]@;@[<v 2>in@;@]@[<hov>%a@]@]"
                  s_rec p_ids (f::xs) print_typed_term t1 print_typed_term t2
        end
  | BinOpCPS(op, t1, t2) ->
      let op =
        match op with
            Eq -> "="
          | Lt -> "<"
          | Gt -> ">"
          | Leq -> "<="
          | Geq -> ">="
          | And -> "&&"
          | Or -> "||"
          | Add -> "+"
          | Sub -> "-"
          | Mult -> "*"
      in
        Format.fprintf fm "%a %s %a" print_typed_term t1 op print_typed_term t2
  | NotCPS t ->
      Format.fprintf fm "not %a" print_typed_term t
  | LabelCPS(true, t) ->
      Format.fprintf fm "l_then %a" print_typed_term t
  | LabelCPS(false, t) ->
      Format.fprintf fm "l_else %a" print_typed_term t
  | EventCPS s -> Format.fprintf fm "{%s}" s
  | FstCPS t ->
      Format.fprintf fm "fst %a" print_typed_term t
  | SndCPS t ->
      Format.fprintf fm "snd %a" print_typed_term t
  | PairCPS(t1, t2) ->
      Format.fprintf fm "(%a, %a)" print_typed_term t1 print_typed_term t2





let new_tvar () = TVarCPS (ref None)
let new_tvar' typ = TVarCPS (ref (Some typ))
let new_var x = {id_cps=x; id_typ=new_tvar()}



let rec flatten = function
    TVarCPS{contents = Some typ} -> flatten typ
  | typ -> typ

let rec flatten_ref r =
  match !r with
    Some (TVarCPS{contents=Some(TVarCPS r)}) -> flatten_ref r
  | _ -> r

let rec occurs r typ =
  match flatten_ref r, flatten typ with
      r, TFunCPS(_,typ1,typ2) -> occurs r typ1 || occurs r typ2
    | r, TVarCPS({contents = None} as r') -> r == r'
    | _ -> false

let rec unify typ1 typ2 =
  match typ1, typ2 with
      TVarCPS({contents = Some typ} as r), typ'
    | typ, TVarCPS({contents = Some typ'} as r) ->
        let typ'' = unify typ typ' in
          r := Some (flatten typ'');
          TVarCPS r
    | TVarCPS r1, TVarCPS r2 when r1 == r2 -> TVarCPS r1
    | TVarCPS({contents = None} as r), typ
    | typ, TVarCPS({contents = None} as r) ->
        if occurs r typ
        then raise Typing.CannotUnify
        else r := Some typ;
        typ
    | TBaseCPS typ1, TBaseCPS typ2 ->
        TBaseCPS typ1
    | TFunCPS(b1,typ11,typ12), TFunCPS(b2,typ21,typ22) ->
        let b = max !b1 !b2 in
        let () = b1 := b in
        let () = b2 := b in
        let typ1' = unify typ11 typ21 in
        let typ2' = unify typ12 typ22 in
          TFunCPS(b1,typ1',typ2')
    | TPairCPS(typ11,typ12), TPairCPS(typ21,typ22) ->
        let typ1' = unify typ11 typ21 in
        let typ2' = unify typ12 typ22 in
          TPairCPS(typ1',typ2')
    | TPredCPS(typ1,_), typ2
    | typ1, TPredCPS(typ2,_) -> unify typ1 typ2
    | typ1,typ2 ->
        Format.printf "Cannot unify.@.typ1: %a@.typ2: %a@."
          print_typ_cps typ1 print_typ_cps typ2; assert false


let rec trans_cont_pos_typ force typ =
  match typ with
      TUnit
    | TInt _
    | TBool -> TBaseCPS typ
    | TFun(x,typ2) ->
        let typ1 = Id.typ x in
        let b = force || match typ2 with TFun _ -> false | _ -> true in
          TFunCPS(ref b, trans_cont_pos_typ force typ1, trans_cont_pos_typ force typ2)
    | TPair(typ1,typ2) -> TPairCPS(trans_cont_pos_typ force typ1, trans_cont_pos_typ force typ2)
    | TPred(typ,ps) -> TPredCPS(trans_cont_pos_typ force typ, ps)
    | _ -> assert false

let rec infer_cont_pos env t =
  match t.desc with
      Unit -> {t_cps=UnitCPS; typ_cps=TBaseCPS t.typ; typ_orig=t.typ}
    | True -> {t_cps=TrueCPS; typ_cps=TBaseCPS t.typ; typ_orig=t.typ}
    | False -> {t_cps=FalseCPS; typ_cps=TBaseCPS t.typ; typ_orig=t.typ}
    | Unknown -> {t_cps=UnknownCPS; typ_cps=TBaseCPS t.typ; typ_orig=t.typ}
    | Int n -> {t_cps=IntCPS n; typ_cps=TBaseCPS t.typ; typ_orig=t.typ}
    | Bottom -> {t_cps=BottomCPS; typ_cps=trans_cont_pos_typ false t.typ; typ_orig=t.typ}
    | NInt x -> assert false
    | RandInt true -> assert false
    | RandInt false -> {t_cps=RandIntCPS; typ_cps=TFunCPS(ref false, TBaseCPS TUnit, TBaseCPS TInt); typ_orig=t.typ}
    | Var x ->
        let typ =
		        try
		          List.assoc (Id.to_string x) env
		        with
		            Not_found when is_external x -> trans_cont_pos_typ false t.typ
		          | Not_found when is_parameter x -> TBaseCPS(TInt)
		          | Not_found -> Format.printf "%a@." print_id x; assert false
        in
          {t_cps=VarCPS{id_cps=x;id_typ=typ}; typ_cps=typ; typ_orig=t.typ}
    | Fun(x, t') ->
(*
        let typ = new_tvar() in
        let env' = (Id.to_string x, typ)::env in
        let typed = infer_cont_pos env' t' in
          {t_cps=FunCPS({id_cps=x;id_typ=typ},typed); typ_cps=TFunCPS(ref true,typ,typed.typ_cps)}
*)
        let f = Id.new_var "f" t.typ in
          infer_cont_pos env (make_let [f, [x], t'] (make_var f))
    | App(t1, ts) ->
        let typed1 = infer_cont_pos env t1 in
        let typeds = List.map (infer_cont_pos env) ts in
        let typ_result = new_tvar () in
        let aux typed (typ,b) = TFunCPS(ref b,typed.typ_cps,typ), false in
        let typ,_ = List.fold_right aux typeds (typ_result,true) in
        let _ = unify typed1.typ_cps typ in
          {t_cps=AppCPS(typed1,typeds); typ_cps=typ_result; typ_orig=t.typ}
    | If(t1, t2, t3) ->
        let typed1 = infer_cont_pos env t1 in
        let typed2 = infer_cont_pos env t2 in
        let typed3 = infer_cont_pos env t3 in
        let _ = unify typed1.typ_cps (TBaseCPS TBool) in
        let typ = unify typed2.typ_cps typed3.typ_cps in
          {t_cps=IfCPS(typed1,typed2,typed3); typ_cps=typ; typ_orig=t.typ}
    | Let(flag, [f, xs, t1], t2) ->
        let typ_f = new_tvar' (trans_cont_pos_typ false (Id.typ f)) in
        let f' = {id_cps=f; id_typ=typ_f} in
        let typ_args = List.map (fun _ -> new_tvar ()) xs in
        let xs' = List.map2 (fun x typ -> {id_cps=x; id_typ=typ}) xs typ_args in
        let env2 = (Id.to_string f, typ_f) :: env in
        let env1 = List.combine (List.map Id.to_string xs) typ_args @@ if flag = Flag.Nonrecursive then env else env2 in
        let typed1 = infer_cont_pos env1 t1 in
        let typed2 = infer_cont_pos env2 t2 in
        let b = ref true in
        let aux typ1 typ2 =
          let typ = TFunCPS(ref !b,typ1,typ2) in
            b := false; typ
        in
        let typ = List.fold_right aux typ_args typed1.typ_cps in
        let _ = unify typ_f typ in
          {t_cps=LetCPS(flag,f',xs',typed1,typed2); typ_cps=typed2.typ_cps; typ_orig=t.typ}
    | Let _ -> assert false
    | BinOp(op, t1, t2) ->
        let typed1 = infer_cont_pos env t1 in
        let typed2 = infer_cont_pos env t2 in
        let _ = unify typed1.typ_cps typed2.typ_cps in
          {t_cps=BinOpCPS(op,typed1,typed2); typ_cps=TBaseCPS t.typ; typ_orig=t.typ}
    | Not t ->
        let typed = infer_cont_pos env t in
        let _ = unify typed.typ_cps (TBaseCPS t.typ) in
          {t_cps=NotCPS typed; typ_cps=TBaseCPS t.typ; typ_orig=t.typ}
    | Event(s,true) -> assert false
    | Event(s,false) -> {t_cps=EventCPS s; typ_cps=TFunCPS(ref false, TBaseCPS TUnit, TBaseCPS TUnit); typ_orig=t.typ}
    | Fst t ->
        let typed = infer_cont_pos env t in
        let typ1 = new_tvar() in
        let typ2 = new_tvar() in
        let typ = TPairCPS(typ1,typ2) in
        let _ = unify typed.typ_cps typ in
          {t_cps=FstCPS typed; typ_cps=typ1; typ_orig=t.typ}
    | Snd t ->
        let typed = infer_cont_pos env t in
        let typ1 = new_tvar() in
        let typ2 = new_tvar() in
        let typ = TPairCPS(typ1,typ2) in
        let _ = unify typed.typ_cps typ in
          {t_cps=SndCPS typed; typ_cps=typ2; typ_orig=t.typ}
    | Pair(t1,t2) ->
        let typed1 = infer_cont_pos env t1 in
        let typed2 = infer_cont_pos env t2 in
          {t_cps=PairCPS(typed1,typed2); typ_cps=TPairCPS(typed1.typ_cps,typed2.typ_cps); typ_orig=t.typ}
    | TryWith (_, _) -> assert false
    | Raise _ -> assert false
    | Match (_, _) -> assert false
    | Constr (_, _) -> assert false
    | Cons (_, _) -> assert false
    | SetField (_, _, _, _, _, _) -> assert false
    | Proj (_, _, _, _) -> assert false
    | Record _ -> assert false
    | Branch (_, _) -> assert false
    | RandValue (_, _) -> assert false
    | Nil -> assert false









let rec get_arg_num = function
    TBaseCPS _ -> 0
  | TVarCPS{contents=None} -> 0
  | TVarCPS{contents=Some typ} -> get_arg_num typ
  | TFunCPS(b,typ1,typ2) when should_insert b -> 1
  | TFunCPS(_,typ1,typ2) -> 1 + get_arg_num typ2
  | TPairCPS _ -> assert false
  | TPredCPS _ -> assert false


let rec app_typ typ typs =
  match typ,typs with
      TVarCPS{contents=Some typ},_ -> app_typ typ typs
    | TFunCPS(_,_,typ2), _::typs' -> app_typ typ2 typs'
    | _, [] -> typ
    | _ -> raise (Fatal "bug? (CPS.app_typ)")




let rec add_preds_cont_aux k t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | NInt y -> NInt y
      | RandInt b -> RandInt b
      | RandValue(typ,b) -> RandValue(typ,b)
      | Var y -> Var y
      | Fun(y, t) -> Fun(y, add_preds_cont_aux k t)
      | App(t1, ts) ->
          let aux t (typ,ts) =
            let x, typ' =
              match typ with
                  TFun(x,typ) -> x, subst_type x t typ
                | _ -> assert false
            in
            let t' =
              if t.desc = Var k
              then make_var (Id.set_typ k (Id.typ x))
              else add_preds_cont_aux k t
            in
              typ', t'::ts
          in
          let _,ts' = List.fold_right aux ts (t1.typ,[]) in
            App(add_preds_cont_aux k t1, ts')
      | If(t1, t2, t3) -> If(add_preds_cont_aux k t1, add_preds_cont_aux k t2, add_preds_cont_aux k t3)
      | Branch(t1, t2) -> Branch(add_preds_cont_aux k t1, add_preds_cont_aux k t2)
      | Let(flag, bindings, t2) ->
          let bindings' = List.map (fun (f,xs,t) -> f, xs, add_preds_cont_aux k t) bindings in
          let t2' = add_preds_cont_aux k t2 in
            Let(flag, bindings', t2')
      | BinOp(op, t1, t2) -> BinOp(op, add_preds_cont_aux k t1, add_preds_cont_aux k t2)
      | Not t1 -> Not (add_preds_cont_aux k t1)
      | Event(s,b) -> Event(s,b)
      | Record fields ->  Record (List.map (fun (f,(s,t1)) -> f,(s,add_preds_cont_aux k t1)) fields)
      | Proj(i,s,f,t1) -> Proj(i,s,f,add_preds_cont_aux k t1)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,add_preds_cont_aux k t1,add_preds_cont_aux k t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(add_preds_cont_aux k t1, add_preds_cont_aux k t2)
      | Constr(s,ts) -> Constr(s, List.map (add_preds_cont_aux k) ts)
      | Match(t1,pats) ->
          let aux (pat,cond,t1) = pat, apply_opt (add_preds_cont_aux k) cond, add_preds_cont_aux k t1 in
            Match(add_preds_cont_aux k t1, List.map aux pats)
      | Raise t -> Raise (add_preds_cont_aux k t)
      | TryWith(t1,t2) -> TryWith(add_preds_cont_aux k t1, add_preds_cont_aux k t2)
      | Pair(t1,t2) -> Pair(add_preds_cont_aux k t1, add_preds_cont_aux k t2)
      | Fst t -> Fst(add_preds_cont_aux k t)
      | Snd t -> Snd(add_preds_cont_aux k t)
      | Bottom -> Bottom
  in
    {desc=desc; typ=t.typ}

let add_preds_cont k t =
  let t' = add_preds_cont_aux k t in
  let ks = List.filter (Id.same k) (get_fv t') in
  Format.printf "APC: %a, %a ===> %a@." Id.print k pp_print_term t pp_print_term t';
    if List.length ks = 0
    then (assert (t.desc = Bottom); k, t')
    else (assert (List.length ks = 1); List.hd ks, t')



let rec trans_typ typ_orig typ =
  match typ_orig,typ with
      _, TVarCPS{contents=Some typ} -> trans_typ typ_orig typ
    | _, TBaseCPS _ -> typ_orig
    | TFun(x_orig,typ), TFunCPS(b,typ1,typ2) when should_insert b ->
        let typ1' = trans_typ (Id.typ x_orig) typ1 in
        let x = Id.new_var "x" typ1' in
        let typ2' = subst_type x_orig (make_var x) (trans_typ typ typ2) in
        let r = Id.new_var "r" typ2' in
        let k = Id.new_var "k" (TFun(r,TUnit)) in
          TFun(x, TFun(k, TUnit))
    | TFun(x_orig,typ), TFunCPS(_,typ1,typ2) ->
        let typ1' = trans_typ (Id.typ x_orig) typ1 in
        let x = Id.new_var "x" typ1' in
        let typ2' = subst_type x_orig (make_var x) (trans_typ typ typ2) in
          TFun(x, typ2')
    | TPair(typ_orig1,typ_orig2), TPairCPS(typ1,typ2) -> TPair(trans_typ typ_orig1 typ1, trans_typ typ_orig2 typ2)
    | TPred(typ1,ps), typ2 -> TPred(trans_typ typ1 typ2, ps)
    | _ ->
        Format.printf "%a,%a@." print_typ typ_orig print_typ_cps typ;
        raise (Fatal "bug? (CPS.trans_typ)")

let trans_var x = Id.set_typ x.id_cps (trans_typ (Id.typ x.id_cps) x.id_typ)

let rec transform k_post c {t_cps=t; typ_cps=typ; typ_orig=typ_orig} =
  match t with
      UnitCPS -> c unit_term
    | TrueCPS -> c true_term
    | FalseCPS -> c false_term
    | IntCPS n -> c (make_int n)
    | BottomCPS -> make_bottom TUnit
    | RandIntCPS -> c (make_randint_cps TUnit)
    | VarCPS x -> c (make_var (trans_var x))
    | FunCPS(x, t) -> assert false
    | AppCPS(t1, ts) ->
        let n = get_arg_num t1.typ_cps in
          if n = List.length ts
          then
            let r = Id.new_var "r" (trans_typ typ_orig typ) in
            let k = Id.new_var ("k" ^ k_post) (TFun(r,TUnit)) in
            let c1 x = make_app x [make_var k] in
            let aux t cc = fun x -> transform k_post (fun y -> cc (make_app x [y])) t in
            let cc = List.fold_right aux ts c1 in
              (*
                let k',t1' = add_preds_cont k (transform k_post cc t1) in
              *)
              make_let' k [r] (c (make_var r)) (transform k_post cc t1)
          else
            let ts1,ts2 = take2 ts n in
            let typ' = app_typ t1.typ_cps ts1 in
            let typ_orig' = Type.app_typ t1.typ_orig ts1 in
            let t1' = {t_cps=AppCPS(t1,ts1);typ_cps=typ';typ_orig=typ_orig'} in
              transform k_post c {t_cps=AppCPS(t1',ts2); typ_cps=typ; typ_orig=typ_orig}
    | IfCPS(t1, t2, t3) ->
        let x = Id.new_var "b" (trans_typ typ_orig typ) in
        let k = Id.new_var ("k" ^ k_post) (TFun(x,TUnit)) in
        let c' y = make_app (make_var k) [y] in
        let t2' = transform k_post c' t2 in
        let t3' = transform k_post c' t3 in
        let c'' y = make_let' k [x] (c (make_var x)) (make_if y t2' t3') in
          transform k_post c'' t1
    | LetCPS(Flag.Nonrecursive, f, [], t1, t2) ->
        let f' = trans_var f in
        let c' t = subst f' t (transform k_post c t2) in
          transform k_post c' t1
    | LetCPS(Flag.Recursive, _, [],  _, _) -> assert false
    | LetCPS(flag, f, xs, t1, t2) ->
        let n = get_arg_num f.id_typ in
          if n = List.length xs
          then
            let r = Id.new_var "r" (trans_typ t1.typ_orig t1.typ_cps) in
            let k = Id.new_var "k" (TFun(r,TUnit)) in
            let f' = trans_var f in
            let xs' = List.map trans_var xs @ [k] in
            let k_post' = "_" ^ Id.name f' in
            let t1' = transform k_post' (fun y -> make_app (make_var k) [y]) t1 in
            let t2' = transform k_post' c t2 in
              make_let_f flag [f', xs', t1'] t2'
          else
            let xs1,xs2 = take2 xs n in
            let typ_g = app_typ f.id_typ xs1 in
            let typ_orig_g = Type.app_typ (Id.typ f.id_cps) xs1 in
            let g = {id_cps=Id.new_var (Id.name f.id_cps) typ_orig_g; id_typ=typ_g} in
            let t_g = {t_cps=VarCPS g; typ_cps=typ_g; typ_orig=typ_orig_g} in
            let t1' = {t_cps=LetCPS(Flag.Nonrecursive, g, xs2, t1, t_g); typ_cps=typ_g; typ_orig=typ_orig_g} in
              transform k_post c {t_cps=LetCPS(flag,f,xs1,t1',t2); typ_cps=typ; typ_orig=typ_orig}
    | BinOpCPS(op, t1, t2) ->
        let c1 t1' t2' = c {desc=BinOp(op, t1', t2'); typ=trans_typ typ_orig typ} in
        let c2 y1 = transform k_post (fun y2 -> c1 y1 y2) t2 in
          transform k_post c2 t1
    | NotCPS t ->
        let c' t1 = c (make_not t1) in
          transform k_post c' t
    | UnknownCPS -> assert false
    | EventCPS s ->  c (make_event_cps s)
    | FstCPS t ->
        let c' t1 = c (make_fst t1) in
          transform k_post c' t
    | SndCPS t ->
        let c' t1 = c (make_snd t1) in
          transform k_post c' t
    | PairCPS(t1,t2) ->
        let c1 t1' t2' = c (make_pair t1' t2') in
        let c2 y1 = transform k_post (fun y2 -> c1 y1 y2) t2 in
          transform k_post c2 t1
    | t -> (Format.printf "%a@." print_t_cps t; assert false)

let transform t = Trans.propagate_typ_arg (transform "" (fun x -> x) t)









let rec has_exception t =
  match t.desc with
      Unit -> false
    | True -> false
    | False -> false
    | Unknown -> false
    | Int n -> false
    | NInt y -> false
    | RandInt b -> false
    | RandValue(typ,b) -> false
    | Var y -> false
    | Fun(y, t) -> has_exception t
    | App(t1, ts) -> has_exception t1 || List.exists has_exception ts
    | If(t1, t2, t3) -> has_exception t1 || has_exception t2 || has_exception t3
    | Branch(t1, t2) -> has_exception t1 || has_exception t2
    | Let(flag, bindings, t2) ->
        has_exception t2 || List.exists (fun (_,_,t) -> has_exception t) bindings
    | BinOp(op, t1, t2) -> has_exception t1 || has_exception t2
    | Not t1 -> has_exception t1
    | Event(s,b) -> false
    | Record fields ->  List.exists (fun (f,(s,t1)) -> has_exception t1) fields
    | Proj(i,s,f,t1) -> has_exception t1
    | SetField(n,i,s,f,t1,t2) -> has_exception t1 || has_exception t2
    | Nil -> false
    | Cons(t1,t2) -> has_exception t1 || has_exception t2
    | Constr(s,ts) -> List.exists has_exception ts
    | Match(t1,pats) ->
        let aux (pat,cond,t1) = (match cond with None -> false | Some c -> has_exception c) || has_exception t1 in
          has_exception t1 || List.exists aux pats
    | Raise t -> true
    | TryWith(t1,t2) -> true || has_exception t1
    | Pair(t1,t2) -> has_exception t1 || has_exception t2
    | Fst t -> has_exception t
    | Snd t -> has_exception t
    | Bottom -> false





let rec short_circuit_eval t =
  let desc =
    match t.desc with
        Unit
      | True
      | False
      | Unknown
      | Int _
      | NInt _
      | RandInt _
      | RandValue _
      | Var _ -> t.desc
      | Fun(y, t) -> Fun(y, short_circuit_eval t)
      | App(t1, ts) -> App(short_circuit_eval t1, List.map short_circuit_eval ts)
      | If(t1, t2, t3) -> If(short_circuit_eval t1, short_circuit_eval t2, short_circuit_eval t3)
      | Branch(t1, t2) -> Branch(short_circuit_eval t1, short_circuit_eval t2)
      | Let(flag, bindings, t2) ->
          let bindings' = List.map (fun (f,xs,t) -> f, xs, short_circuit_eval t) bindings in
            Let(flag, bindings', short_circuit_eval t2)
      | BinOp(And, t1, t2) -> If(short_circuit_eval t1, short_circuit_eval t2, false_term)
      | BinOp(Or, t1, t2) -> If(short_circuit_eval t1, true_term, short_circuit_eval t2)
      | BinOp(op, t1, t2) -> BinOp(op, short_circuit_eval t1, short_circuit_eval t2)
      | Not t1 -> Not (short_circuit_eval t1)
      | Event(s,b) -> Event(s,b)
      | Record fields -> Record (List.map (fun (f,(s,t1)) -> f,(s,short_circuit_eval t1)) fields)
      | Proj(i,s,f,t1) -> Proj(i,s,f,short_circuit_eval t1)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,short_circuit_eval t1,short_circuit_eval t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(short_circuit_eval t1,short_circuit_eval t2)
      | Constr(s,ts) -> Constr(s, List.map short_circuit_eval ts)
      | Match(t1,pats) ->
          let aux (pat,cond,t1) = pat, apply_opt short_circuit_eval cond, short_circuit_eval t1 in
            Match(short_circuit_eval t1, List.map aux pats)
      | Raise t -> Raise (short_circuit_eval t)
      | TryWith(t1,t2) -> TryWith(short_circuit_eval t1, short_circuit_eval t2)
      | Pair(t1,t2) -> Pair(short_circuit_eval t1, short_circuit_eval t2)
      | Fst t -> Fst (short_circuit_eval t)
      | Snd t -> Snd (short_circuit_eval t)
      | Bottom -> Bottom
  in
    {desc=desc; typ=t.typ}





let trans2 t =
  let t = short_circuit_eval t in
  let t = Trans.elim_fun t in
  let t_cps =
    if has_exception t
    then
      let u = Id.new_var "u" typ_event in
      let ce _ = make_let [u, [], fail_term] unit_term in
        trans_exc (fun x -> x) ce t
    else
      let cps_pre = infer_cont_pos [] t in
      let () = if false then Format.printf "CPS_infer_cont_pos:@.%a@." print_t_cps cps_pre.t_cps in
      let cps = transform cps_pre in
        cps
  in
    Type_check.check t_cps TUnit;
    t_cps





















*)
(*
let n = Id.new_var "n" TInt
let x = Id.new_var "x" TInt
let f = Id.new_var "f" (TFun(x,TInt))
let test = make_let [f, [x], make_add (make_var x) (make_int 10)] (make_app (make_var f) [make_int 20])
let test = make_fun x (make_add (make_var x) (make_int 10))
let test = make_let [f, [], make_fun x (make_add (make_var x) (make_int 10))] (make_app (make_var f) [make_int 20])
let () = Format.printf "%a@." pp_print_term test
let test' = trans1 test
*)
let trans = trans1
