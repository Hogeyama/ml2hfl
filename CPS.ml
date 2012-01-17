
open Syntax
open Type
open Utilities




let rec trans_simpl_typ = function
    TFun(x,typ) ->
      let typ1 = trans_simpl_typ (Id.typ x) in
      let typ2 = trans_simpl_typ typ in
      let y = Id.new_var "x" typ2 in
      let k = Id.new_var "k" (TFun(y,TUnit)) in
        TFun(Id.set_typ x typ1, TFun(k,TUnit))
  | TPair(typ1,typ2) -> TPair(trans_simpl_typ typ1, trans_simpl_typ typ2)
  | TVar _ -> assert false
  | TUnknown -> assert false
  | TList _ -> assert false
  | TVar {contents = None} -> assert false
  | TVar {contents = Some typ} -> trans_simpl_typ typ
  | typ -> typ

let trans_simpl_var x = Id.set_typ x (trans_simpl_typ (Id.typ x))

let make_let' f xs t1 t2 =
  match xs,t1.desc with
      [r], App({desc=Var _} as k, [{desc=Var r'}]) when r = r' -> subst f k t2
    | _ -> make_let [f, xs, t1] t2

let rec trans_simpl c t =
  let typ = trans_simpl_typ t.typ in
    match t.desc with
        Unit
      | True
      | False
      | Int _
      | NInt _ -> c t
      | Bottom -> make_bottom typ
      | RandInt false ->
          let r = Id.new_var "r" (TInt[]) in
          let k = Id.new_var "k" (TFun(r,TUnit)) in
          let t = make_let' k [r] (c (make_var r)) (make_var k) in
            make_app (make_randint_cps TUnit) [t]
      | RandInt true -> assert false
      | Var x -> c (make_var (trans_simpl_var x))
      | Fun(x, t) ->
          let r = Id.new_var "r" typ in
          let k = Id.new_var "k" (TFun(r,TUnit)) in
            c (make_fun x (make_fun k (trans_simpl (fun y -> make_app (make_var k) [y]) t)))
      | App(_, []) -> assert false
      | App(t1, [t2]) ->
          let r = Id.new_var "r" (trans_simpl_typ t.typ) in
          let k = Id.new_var "k" (TFun(r,TUnit)) in
          let c' x = trans_simpl (fun y -> make_app x [y; make_var k]) t2 in
          let t2' = trans_simpl c' t1 in
            make_let' k [r] (c (make_var r)) t2'
      | App(t1, t2::ts) ->
          let typ = match t1.typ with TFun(_,typ) -> typ | _ -> assert false in
            trans_simpl c {desc=App({desc=App(t1,[t2]);typ=typ}, ts); typ=t.typ}
      | If(t1, t2, t3) ->
          let x = Id.new_var "x" typ in
          let k = Id.new_var "k" (TFun(x,TUnit)) in
          let c' y = make_app (make_var k) [y] in
          let t2' = trans_simpl c' t2 in
          let t3' = trans_simpl c' t3 in
          let c'' y = make_let' k [x] (c (make_var x)) (make_if y t2' t3') in
            trans_simpl c'' t1
      | Let _ -> assert false
          (*
            | Let(Flag.Nonrecursive, x, [], t1, t2) ->
            let x' = trans_simpl_var x in
            let c' t = subst x' t (trans_simpl c t2) in
            trans_simpl c' t1
            | Let(Flag.Recursive, f, [], t1, t2) -> assert false
            | Let(flag, f, [x], t1, t2) ->
            let x' = trans_simpl_var x in
            let r = Id.new_var "r" typ in
            let k = Id.new_var "k" (TFun(r,TUnit)) in
            let f' = trans_simpl_var f in
            let c' y = make_app (make_var k) [y] in
            let t1' = trans_simpl c' t1 in
            let t2' = trans_simpl c t2 in
            make_let_f flag f' [x';k] t1' t2'
            | Let(flag, f, x::xs, t1, t2) ->
            let typ = match Id.typ f with TFun(_,typ) -> typ | _ -> assert false in
            let g = Id.new_var (Id.name f) typ in
            let t1' = make_let g xs t1 (make_var g) in
            trans_simpl c (make_let_f flag f [x] t1' t2)
          *)
      | BinOp(op, t1, t2) ->
          let c1 t1' t2' = c {desc=BinOp(op, t1', t2'); typ=typ} in
          let c2 y1 = trans_simpl (fun y2 -> c1 y1 y2) t2 in
            trans_simpl c2 t1
      | Not t ->
          let c' t1 = c (make_not t1) in
            trans_simpl c' t
      | Unknown -> c {desc=Unknown;typ=t.typ}
      | Event(s,false) -> c (make_event_cps s)
      | Pair(t1, t2) ->
          let c1 t1' t2' = c (make_pair t1' t2') in
          let c2 y1 = trans_simpl (fun y2 -> c1 y1 y2) t2 in
            trans_simpl c2 t1
      | Fst t ->
          let c' t1 = c (make_fst t1) in
            trans_simpl c' t
      | Snd t ->
          let c' t1 = c (make_snd t1) in
            trans_simpl c' t
      | _ -> (Format.printf "%a@." pp_print_term t; assert false)
let trans_simpl = trans_simpl (fun x -> x)


(*
let trans t =
  let cps = trans_simpl t in
  let () = if true then Format.printf "CPS:@.%a@." pp_print_term cps in
  let () = if true then Format.printf "CPS:@.%a@." (Syntax.print_term' Syntax.ML 0 false) cps in
  let () = Type_check.check cps in
  let extracted = extract_records cps in
  let () = if true then Format.printf "EXTRACTED:@.%a@." pp_print_term extracted in
  let normalized = normalize extracted in
  let inlined = inlining !funs [] normalized in
    part_eval inlined
*)

















let rec trans_exc_typ = function
    TUnit -> TUnit
  | TBool -> TBool
  | TAbsBool -> TAbsBool
  | TInt ps -> TInt ps
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
  | TLabel _ -> assert false
  | TPred _ -> assert false
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
        let r = Id.new_var "r" (TInt[]) in
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
  | TInt ps -> Leaf (TInt ps)
  | TRInt p -> Leaf (TRInt p)
  | TVar _ -> assert false
  | TFun _ as typ ->
      let xs,typ' = decomp_tfun typ in
      let aux x =
        let typs = remove_pair_typ (Id.typ x) in
          match flatten typs with
              [typ] -> [Id.set_typ x typ]
            | typs' -> mapi (fun i typ -> Id.set_typ (Id.add_name x (string_of_int i)) typ) typs'
      in
      let xs' = List.flatten (List.map aux xs) in
        Leaf (List.fold_right (fun x typ -> TFun(x,typ)) xs' typ')
  | TPair(typ1,typ2) -> Node(remove_pair_typ typ1, remove_pair_typ typ2)
  | TList typ -> Leaf (TList(root (remove_pair_typ typ)))
  | TConstr(s,b) -> Leaf (TConstr(s,b))
  | TUnknown -> assert false
  | TVariant _ -> assert false
  | TLabel _ -> assert false
  | TPred(x,typ) ->
      let x' = remove_pair_var x in
      let typ' = remove_pair_typ typ in
        map (fun path x -> TPred(x, root (proj path typ'))) x'

and remove_pair_var x =
  let to_string path = List.fold_left (fun acc i -> acc ^ string_of_int i) "" path in
  let aux path typ = Id.set_typ (Id.add_name x (to_string path)) typ in
    map aux (remove_pair_typ (Id.typ x))

and remove_pair t typ_opt =
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
          let t' = root (remove_pair t None) in
            Leaf (List.fold_right make_fun xs t')
      | App(t, ts) ->
          let typs = get_argtyps t.typ in
          let typs' = take typs (List.length ts) in
          let t' = root (remove_pair t None) in
          let ts' = List.flatten (List.map2 (fun t typ -> flatten (remove_pair t (Some typ))) ts typs') in
            Leaf (make_app t' ts')
      | If(t1, t2, t3) ->
          let t1' = root (remove_pair t1 None) in
          let t2' = root (remove_pair t2 None) in
          let t3' = root (remove_pair t3 None) in
            Leaf (make_if t1' t2' t3')
      | Branch(t1, t2) ->
          let t1' = root (remove_pair t1 None) in
          let t2' = root (remove_pair t2 None) in
            Leaf {desc=Branch(t1',t2'); typ=t1'.typ}
      | Let(flag, [f, xs, t1], t2) ->
          let f' = root (remove_pair_var f) in
          let xs' = List.flatten (List.map (fun x -> flatten (remove_pair_var x)) xs) in
          let t1' = root (remove_pair t1 None) in
          let t2' = root (remove_pair t2 None) in
            Leaf (make_let_f flag [f', xs', t1'] t2')
      | Let _ -> assert false
      | BinOp(op, t1, t2) ->
          let t1' = root (remove_pair t1 None) in
          let t2' = root (remove_pair t2 None) in
            Leaf {desc=BinOp(op, t1', t2'); typ=root typs}
      | Not t1 ->
          let t1' = root (remove_pair t1 None) in
            Leaf (make_not t1')
      | Record fields -> assert false
      | Proj(i,s,f,t1) -> assert false
      | SetField(n,i,s,f,t1,t2) -> assert false
      | Nil -> assert false
      | Cons(t1,t2) -> assert false
      | Constr(s,ts) -> assert false
      | Match(t1,pats) -> assert false
      | TryWith(t1,t2) -> assert false
      | Pair(t1,t2) -> Node(remove_pair t1 None, remove_pair t2 None)
      | Fst t ->
          let t' = 
            match remove_pair t None with
                Leaf _ -> assert false
              | Node(t',_) -> t'
          in
            t'
      | Snd t ->
          let t' = 
            match remove_pair t None with
                Leaf _ -> assert false
              | Node(_,t') -> t'
          in
            t'
      | _ -> (Format.printf "%a@." pp_print_term t; assert false)

let remove_pair t =
  let t' = root (remove_pair t None) in
  let () = Type_check.check t' TUnit in
    t'


























type typed_term = {t_cps:t_cps; typ_cps:typ_cps; typ_orig:typ}
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
  | AppCPS of typed_term * typed_term list
  | IfCPS of typed_term * typed_term * typed_term
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
  | TFunCPS of bool ref * Syntax.id option * typ_cps * typ_cps
  | TPairCPS of typ_cps * typ_cps
  | TPredCPS of Syntax.id * typ_cps



let rec print_typ_cps fm = function
    TBaseCPS typ -> Format.fprintf fm "%a" Syntax.print_typ typ
  | TVarCPS {contents = None} -> Format.fprintf fm "?"
  | TVarCPS {contents = Some typ} -> Format.fprintf fm "%a" print_typ_cps typ
  | TFunCPS({contents=b},_,typ1,typ2) ->
      Format.fprintf fm "(%a %s %a)" print_typ_cps typ1 (if b then "=>" else "->") print_typ_cps typ2
  | TPairCPS(typ1,typ2) ->
      Format.fprintf fm "(%a * %a)" print_typ_cps typ1 print_typ_cps typ2
  | TPredCPS(x,typ) ->
      Format.fprintf fm "(%a|[%a])" print_typ_cps typ Id.print x


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
      r, TFunCPS(_,_,typ1,typ2) -> occurs r typ1 || occurs r typ2
    | r, TVarCPS({contents = None} as r') -> r == r'
    | _ -> false

let rec unify typ1 typ2 =
  match typ1, typ2 with
      typ1, TPredCPS(x,typ2)
    | TPredCPS(x,typ1), typ2 -> TPredCPS(x, unify typ1 typ2)
    | TVarCPS({contents = Some typ} as r), typ'
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
    | TFunCPS(b1,_,typ11,typ12), TFunCPS(b2,_,typ21,typ22) ->
        let b = max !b1 !b2 in
        let () = b1 := b in
        let () = b2 := b in
        let typ1' = unify typ11 typ21 in
        let typ2' = unify typ12 typ22 in
          TFunCPS(b1,None,typ1',typ2')
    | TPairCPS(typ11,typ12), TPairCPS(typ21,typ22) ->
        let typ1' = unify typ11 typ21 in
        let typ2' = unify typ12 typ22 in
          TPairCPS(typ1',typ2')
    | typ1,typ2 -> Format.printf "typ1: %a@.typ2: %a@." print_typ_cps typ1 print_typ_cps typ2; assert false


let rec trans_cont_pos_typ typ =
  match typ with
      TUnit
    | TInt _
    | TBool -> TBaseCPS typ
    | TFun(x,typ2) ->
        let typ1 = Id.typ x in
        let b = match typ2 with TFun _ -> false | _ -> true in
          TFunCPS(ref b, None, trans_cont_pos_typ typ1, trans_cont_pos_typ typ2)
    | TPair(typ1,typ2) -> TPairCPS(trans_cont_pos_typ typ1, trans_cont_pos_typ typ2)
    | TPred(x,typ) -> TPredCPS(x, trans_cont_pos_typ typ)
    | _ -> assert false

let rec infer_cont_pos env t =
  match t.desc with
      Unit -> {t_cps=UnitCPS; typ_cps=TBaseCPS t.typ; typ_orig=t.typ}
    | True -> {t_cps=TrueCPS; typ_cps=TBaseCPS t.typ; typ_orig=t.typ}
    | False -> {t_cps=FalseCPS; typ_cps=TBaseCPS t.typ; typ_orig=t.typ}
    | Unknown -> {t_cps=UnknownCPS; typ_cps=TBaseCPS t.typ; typ_orig=t.typ}
    | Int n -> {t_cps=IntCPS n; typ_cps=TBaseCPS t.typ; typ_orig=t.typ}
    | Bottom -> {t_cps=BottomCPS; typ_cps=trans_cont_pos_typ t.typ; typ_orig=t.typ}
    | NInt x -> assert false
    | RandInt true -> assert false
    | RandInt false -> {t_cps=RandIntCPS; typ_cps=TFunCPS(ref false, None, TBaseCPS TUnit, TBaseCPS (TInt[])); typ_orig=t.typ}
    | Var x ->
        let typ = try List.assoc (Id.to_string x) env with Not_found -> Format.printf "%a@." print_id x; assert false in
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
        let aux typed (typ,b) = TFunCPS(ref b,None,typed.typ_cps,typ), false in
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
        let typ_f = new_tvar' (trans_cont_pos_typ (Id.typ f)) in
        let f' = {id_cps=f; id_typ=typ_f} in
        let typ_args = List.map (fun _ -> new_tvar ()) xs in
        let xs' = List.map2 (fun x typ -> {id_cps=x; id_typ=typ}) xs typ_args in
        let env2 = (Id.to_string f, typ_f) :: env in
        let env1 = List.combine (List.map Id.to_string xs) typ_args @@ if flag = Flag.Nonrecursive then env else env2 in
        let typed1 = infer_cont_pos env1 t1 in
        let typed2 = infer_cont_pos env2 t2 in
        let b = ref true in
        let aux typ1 typ2 =
          let typ = TFunCPS(ref !b,None,typ1,typ2) in
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
    | Event(s,false) -> {t_cps=EventCPS s; typ_cps=TFunCPS(ref false, None, TBaseCPS TUnit, TBaseCPS TUnit); typ_orig=t.typ}
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
  | TFunCPS({contents=true},_,typ1,typ2) -> 1
  | TFunCPS({contents=false},_,typ1,typ2) -> 1 + get_arg_num typ2
  | TPairCPS _ -> assert false


let rec app_typ typ typs =
  match typ,typs with
      TVarCPS{contents=Some typ},_ -> app_typ typ typs
    | TFunCPS(_,_,_,typ2), _::typs' -> app_typ typ2 typs'
    | _, [] -> typ
    | _ -> assert false

let rec from_orig_typ = function
    TUnit
  | TBool
  | TAbsBool
  | TInt _
  | TRInt _ as typ -> TBaseCPS typ
  | TFun(x,typ) -> TFunCPS(ref false, Some x, from_orig_typ (Id.typ x), from_orig_typ typ)
  | TPair(typ1,typ2) -> TPairCPS(from_orig_typ typ1, from_orig_typ typ2)
  | TConstr _ -> assert false
  | TUnknown -> assert false
  | TVar _ -> assert false
  | TVariant _ -> assert false
  | TLabel _ -> assert false
  | TPred(x,typ) -> TPredCPS(x, from_orig_typ typ)

let rec merge_typ typ1 typ2 =
  match typ1,typ2 with
      TVarCPS{contents=Some typ1}, typ2 -> merge_typ typ1 typ2
    | TBaseCPS _, TBaseCPS typ -> TBaseCPS typ
    | TFunCPS(b1,x1,typ11,typ12), TFunCPS(b2,x2,typ21,typ22) ->
        let x =
          match x1,x2 with
              Some x,None
            | None,Some x -> x
            | _ -> assert false
        in
          TFunCPS(ref (max !b1 !b2), Some x, merge_typ typ11 typ21, merge_typ typ12 typ22)
    | TPairCPS(typ11,typ12), TPairCPS(typ21,typ22) ->
        TPairCPS(merge_typ typ11 typ21, merge_typ typ12 typ22)
    | typ1, TPredCPS(x,typ2) -> TPredCPS(x, merge_typ typ1 typ2)
    | TPredCPS(_,typ1), typ2 -> merge_typ typ1 typ2
    | _ -> Format.printf "%a,%a@." print_typ_cps typ1 print_typ_cps typ2; assert false

let rec trans_typ_aux = function
    TBaseCPS typ -> typ
  | TFunCPS(b,Some x_orig,typ1,typ2) when !b ->
      let typ1' = trans_typ_aux typ1 in
      let x = Id.new_var "x" typ1' in
      let typ2' = subst_type x_orig (make_var x) (trans_typ_aux typ2) in
      let r = Id.new_var "r" typ2' in
      let k = Id.new_var "k" (TFun(r,TUnit)) in
        TFun(x, TFun(k, TUnit))
  | TFunCPS(_,Some x_orig,typ1,typ2) ->
      let typ1' = trans_typ_aux typ1 in
      let x = Id.new_var "x" typ1' in
      let typ2' = subst_type x_orig (make_var x) (trans_typ_aux typ2) in
        TFun(x, typ2')
  | TFunCPS _ -> assert false
  | TPairCPS(typ1,typ2) -> TPair(trans_typ_aux typ1, trans_typ_aux typ2)
  | TVarCPS{contents=None} -> assert false
  | TVarCPS{contents=Some typ} -> trans_typ_aux typ
  | TPredCPS(x,typ) ->
      let typ' = trans_typ_aux typ in
        TPred(Id.set_typ x typ', typ')

let trans_typ typ_orig typ =
  trans_typ_aux (merge_typ typ (from_orig_typ typ_orig))

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
    | FunCPS(x, t) ->
(*
        let x' = trans_var x in
        let r = Id.new_var "r" (trans_typ typ) in
        let k = Id.new_var "k" (TFun(r,TUnit)) in
          c (make_fun x' (make_fun k (transform k_post (fun y -> make_app (make_var k) [y]) t)))
*)
        assert false
    | AppCPS(t1, ts) ->
        let n = get_arg_num t1.typ_cps in
          if n = List.length ts
          then
            let r = Id.new_var "r" (trans_typ typ_orig typ) in
            let k = Id.new_var ("k" ^ k_post) (TFun(r,TUnit)) in
            let c1 x = make_app x [make_var k] in
            let cc = List.fold_right (fun t cc -> fun x -> transform k_post (fun y -> cc (make_app x [y])) t) ts c1 in
              make_let' k [r] (c (make_var r)) (transform k_post cc t1)
          else
            let ts1,ts2 = take2 ts n in
            let typ' = app_typ t1.typ_cps ts1 in
            let typ_orig' = Type.app_typ t1.typ_orig ts1 in
              transform k_post c {t_cps=AppCPS({t_cps=AppCPS(t1,ts1);typ_cps=typ';typ_orig=typ_orig'},ts2); typ_cps=typ; typ_orig=typ_orig}
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
            let xs' = List.map trans_var xs in
            let k_post' = "_" ^ Id.name f' in
            let t1' = transform k_post' (fun y -> make_app (make_var k) [y]) t1 in
            let t2' = transform k_post' c t2 in
              make_let_f flag [f', xs'@[k], t1'] t2'
          else
            let xs1,xs2 = take2 xs n in
            let typ_g = app_typ f.id_typ xs1 in
            let typ_orig_g = Type.app_typ (Id.typ f.id_cps) xs1 in
            let g = {id_cps=Id.new_var (Id.name f.id_cps) typ_orig_g; id_typ=typ_g} in
            let t1' = {t_cps=LetCPS(Flag.Nonrecursive, g, xs2, t1, {t_cps=VarCPS g;typ_cps=typ_g; typ_orig=typ_orig_g}); typ_cps=typ_g; typ_orig=typ_orig_g} in
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
let transform = transform "" (fun x -> x)









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


let trans t =
  let t' =
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
    Type_check.check t' TUnit;
    t'



