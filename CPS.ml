open Syntax
open Type
open Utilities

module RT = Ref_type

let debug = false

let rec element_num = function
    TPair(typ1,typ2) -> element_num typ1 + element_num typ2
  | _ -> 1

let rec uncurry_typ rtyp typ =
  if debug then Format.printf "rtyp:%a@.typ:%a@.@."
    RT.print rtyp pp_print_typ typ;
  match rtyp,typ with
      RT.Inter rtyps, _ ->
        RT.Inter (List.map (fun rtyp -> uncurry_typ rtyp typ) rtyps)
    | _, TFun(x,typ2) ->
        let typ1 = Id.typ x in
        let n = element_num typ1 in
        let exts,xrtyps,rtyp2 = RT.decomp_fun n rtyp in
        let rtyps = List.map snd xrtyps in
        let map,rtyp1' = uncurry_typ_arg rtyps typ1 in
        let rtyp2' = uncurry_typ rtyp2 typ2 in
        let aux (x,typ1) typ2 = RT.ExtArg(x,typ1,typ2) in
        let x = Id.new_var "x" typ_unknown in
        let rtyp2'' = List.fold_left (fun typ (x',f) -> RT.subst x' (f x) typ) rtyp2' map in
          List.fold_right aux exts (RT.Fun(x, rtyp1', rtyp2''))
    | _ -> rtyp

and get_arg_var = function
    RT.Base(_,x,_) -> x
  | _ -> Id.new_var "x" typ_unknown

and uncurry_typ_arg rtyps typ =
  match rtyps,typ with
      _, TPair(typ1,typ2) ->
        let rtyps1,rtyps2 = take2 rtyps (element_num typ1) in
        let map1,rtyp1 = uncurry_typ_arg rtyps1 typ1 in
        let map2,rtyp2 = uncurry_typ_arg rtyps2 typ2 in
        let map1' = List.map (fun (x,f) -> x, fun x' -> make_fst (f x')) map1 in
        let map2' = List.map (fun (x,f) -> x, fun x' -> make_snd (f x')) map1 in
          map1'@@map2', RT.Pair(get_arg_var rtyp1, rtyp1, rtyp2)
    | [RT.Base(base,x,p) as rtyp], _ -> [x, fun x' -> make_var x'], uncurry_typ rtyp typ
    | [rtyp], _ -> [], uncurry_typ rtyp typ
    | _ -> assert false

let uncurry_rtyp t f rtyp =
  let typ = Trans.assoc_typ f t in
  let rtyp' = uncurry_typ rtyp typ in
    if debug then Format.printf "%a:@.rtyp:%a@.typ:%a@.===> %a@.@."
      Id.print f RT.print rtyp pp_print_typ typ RT.print rtyp';
    rtyp'

type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree

let root = function
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
          let () = assert (List.length typs >= List.length ts) in
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
      | Let(flag, bindings, t) ->
          let aux (f,xs,t) =
            let f' = root (remove_pair_var f) in
            let xs' = List.flatten (List.map (fun x -> flatten (remove_pair_var x)) xs) in
            let t' = root (remove_pair_aux t None) in
              f', xs', t'
          in
          let bindings' = List.map aux bindings in
          let t' = root (remove_pair_aux t None) in
            Leaf (make_let_f flag bindings' t')
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

and remove_pair t = root (remove_pair_aux t None)

let remove_pair t =
  let t' = remove_pair t in
  let () = Type_check.check t' TUnit in
    t', uncurry_rtyp t




















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
  | LetCPS of rec_flag * (typed_ident * typed_term) list * typed_term
  | BinOpCPS of binop * typed_term * typed_term
  | NotCPS of typed_term
  | EventCPS of string
  | FstCPS of typed_term
  | SndCPS of typed_term
  | PairCPS of typed_term * typed_term
  | RaiseCPS of typed_term
  | TryWithCPS of typed_term * typed_term
and typ_cps =
    TBaseCPS of Syntax.typ
  | TFunCPS of effect_var * typ_cps * typ_cps
  | TPairCPS of typ_cps * typ_cps
and effect = EUnknown (* for debug *) | ENone | ECont | EExcep
and effect_var = int
and effect_constr =
    CGeq of effect_var * effect
  | CGeqVar of effect_var * effect_var

let effect_max x y =
  match x, y with
      EUnknown, _
    | _, EUnknown -> assert false
    | ENone, _ -> y
    | ECont, EExcep -> EExcep
    | ECont, _ -> ECont
    | EExcep, _ -> EExcep

let sol = ref (fun (n:int) -> EUnknown)

let rec print_typ_cps' fm = function
    TBaseCPS typ -> Format.fprintf fm "%a" Syntax.print_typ typ
  | TFunCPS(e,typ1,typ2) ->
      Format.fprintf fm "(%a -e%d-> %a)" print_typ_cps' typ1 e print_typ_cps' typ2
  | TPairCPS(typ1,typ2) ->
      Format.fprintf fm "(%a * %a)" print_typ_cps' typ1 print_typ_cps' typ2

let rec print_typ_cps fm = function
    TBaseCPS typ -> Format.fprintf fm "%a" Syntax.print_typ typ
  | TFunCPS(e,typ1,typ2) when !sol e = EUnknown ->
      Format.fprintf fm "(%a -%a-> %a)" print_typ_cps typ1 print_evar e print_typ_cps typ2
  | TFunCPS(e,typ1,typ2) when !sol e = ENone ->
      Format.fprintf fm "(%a -> %a)" print_typ_cps typ1 print_typ_cps typ2
  | TFunCPS(e,typ1,typ2) when !sol e = ECont ->
      Format.fprintf fm "(%a => %a)" print_typ_cps typ1 print_typ_cps typ2
  | TFunCPS(e,typ1,typ2) when !sol e = EExcep ->
      Format.fprintf fm "(%a -=> %a)" print_typ_cps typ1 print_typ_cps typ2
  | TFunCPS _ -> assert false
  | TPairCPS(typ1,typ2) ->
      Format.fprintf fm "(%a * %a)" print_typ_cps typ1 print_typ_cps typ2


and print_typed_termlist fm = List.iter (fun bd -> Format.fprintf fm "@;%a" print_typed_term bd)

and print_typed_term fm {t_cps=t; typ_cps=typ; effect=e} =
  match true, !sol e with
      true, EUnknown -> Format.fprintf fm "(%a :%a: %a)" print_t_cps t print_evar e print_typ_cps typ
    | true, e -> Format.fprintf fm "(%a :%a: %a)" print_t_cps t print_effect e print_typ_cps typ
    | _ -> Format.fprintf fm "(%a : %a)" print_t_cps t print_typ_cps typ

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
      Format.fprintf fm "@[<hov 2>fun %a : %a ->@ %a@]" print_id x.id_cps print_typ_cps x.id_typ print_typed_term t
  | AppCPS(t1, t2) ->
      Format.fprintf fm "%a%a" print_typed_term t1 print_typed_term t2
  | IfCPS(t1, t2, t3) ->
      Format.fprintf fm "@[@[if %a@]@;then @[%a@]@;else @[%a@]@]"
        print_typed_term t1 print_typed_term t2 print_typed_term t3
  | LetCPS(flag, bindings, t) ->
      let is_rec = match flag with Nonrecursive -> false | Recursive -> true in
      let head = ref (if is_rec then "let rec" else "let") in
      let pr fm (f,t) =
        Format.fprintf fm "@[<hov 2>%s %a : %a =@ @[%a@]@]@;"
          !head print_id f.id_cps print_typ_cps f.id_typ print_typed_term t;
        head := "and"
      in
        Format.fprintf fm "@[<v>%a@;in@;%a@]" (print_list pr "" false) bindings print_typed_term t
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
  | EventCPS s -> Format.fprintf fm "{%s}" s
  | FstCPS t ->
      Format.fprintf fm "fst %a" print_typed_term t
  | SndCPS t ->
      Format.fprintf fm "snd %a" print_typed_term t
  | PairCPS(t1, t2) ->
      Format.fprintf fm "(%a,@ %a)" print_typed_term t1 print_typed_term t2
  | RaiseCPS t ->
      Format.fprintf fm "@[raise %a@]" print_typed_term t
  | TryWithCPS(t1,t2) ->
      Format.fprintf fm "@[<hov 2>@[<hov 2>try@ %a@]@ with@ %a@]" print_typed_term t1 print_typed_term t2

and print_effect fm = function
    EUnknown -> Format.fprintf fm "EUnknown"
  | ENone -> Format.fprintf fm "ENone"
  | ECont -> Format.fprintf fm "ECont"
  | EExcep -> Format.fprintf fm "EExcep"

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
    | TPred(typ,ps) -> infer_effect_typ typ
    | _ -> Format.printf "%a@." print_typ typ; assert false

let new_var x = {id_cps=x; id_typ=infer_effect_typ (Id.typ x)}

let _TFunCPS(e, typ1, typ2) =
  if !Flag.cps_simpl then constraints := CGeq(e, ECont) :: !constraints;
  TFunCPS(e, typ1, typ2)

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
    | RandInt true -> assert false
    | RandInt false ->
        let e = new_evar () in
        let typ = _TFunCPS(e, TBaseCPS TUnit, TBaseCPS TInt) in
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
        let e,a_typ,r_typ = match typ' with TFunCPS(e,typ1,typ2) -> e,typ1,typ2 | _ -> assert false in
          if !Flag.cps_simpl then constraints := CGeq(e, ECont) :: !constraints;
          constraints := CGeqVar(e, typed.effect) :: !constraints;
          unify a_typ x_typ;
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
        let typ = _TFunCPS(e0, typed2.typ_cps, typ_result) in
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
          constraints := CGeqVar(e, typed1.effect) :: !constraints;
          constraints := CGeqVar(e, typed2.effect) :: !constraints;
          constraints := CGeqVar(e, typed3.effect) :: !constraints;
          constraints := CGeq(e, ECont) :: !constraints; (* for TRecS *)
          unify typed2.typ_cps typed3.typ_cps;
          {t_cps=IfCPS(typed1,typed2,typed3); typ_cps=typed2.typ_cps; typ_orig=t.typ; effect=e}
    | Let(flag, bindings, t1) ->
        let make_env (f,_,_) = Id.to_string f, infer_effect_typ (Id.typ f) in
        let env_f = List.map make_env bindings in
        let env' = env_f @@ env in
        let env'' = match flag with Nonrecursive -> env | Recursive -> env' in
        let aux (f, xs, t1) =
          let f' = {id_cps=f; id_typ=List.assoc (Id.to_string f) env_f} in
          let t1' = List.fold_right make_fun xs t1 in
          let typed = infer_effect env'' t1' in
          let () =
            match flag with
                Nonrecursive -> ()
              | Recursive -> lift_letrec_typ typed
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
        let e = new_evar () in
          unify typed1.typ_cps typed2.typ_cps;
          constraints := CGeqVar(e, typed1.effect) :: !constraints;
          constraints := CGeqVar(e, typed2.effect) :: !constraints;
          {t_cps=BinOpCPS(op,typed1,typed2); typ_cps=TBaseCPS t.typ; typ_orig=t.typ; effect=e}
    | Not t1 ->
        let typed = infer_effect env t1 in
          unify typed.typ_cps (TBaseCPS t.typ);
          {t_cps=NotCPS typed; typ_cps=TBaseCPS t.typ; typ_orig=t.typ; effect=typed.effect}
    | Event(s,true) -> assert false
    | Event(s,false) ->
        let e = new_evar () in
        let typ = _TFunCPS(e, TBaseCPS TUnit, TBaseCPS TUnit) in
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
          constraints := CGeq(e, ECont) :: !constraints; (* for remove_pair *)
          {t_cps=PairCPS(typed1,typed2); typ_cps=typ; typ_orig=t.typ; effect=e}
    | TryWith(t1, t2) ->
        let typed1 = infer_effect env t1 in
        let typed2 = infer_effect env t2 in
        let e = new_evar () in
          constraints := CGeqVar(e, typed1.effect) :: !constraints;
          {t_cps=TryWithCPS(typed1,typed2); typ_cps=infer_effect_typ t.typ; typ_orig=t.typ; effect=e}
    | Raise t1 ->
        let typed = infer_effect env t1 in
        let e = new_evar () in
          constraints := CGeq(e, EExcep) :: !constraints;
          {t_cps=RaiseCPS typed; typ_cps=infer_effect_typ t.typ; typ_orig=t.typ; effect=e}
    | Match (_, _) -> assert false
    | Constr (_, _) -> assert false
    | Cons (_, _) -> assert false
    | SetField (_, _, _, _, _, _) -> assert false
    | Proj (_, _, _, _) -> assert false
    | Record _ -> assert false
    | Branch (_, _) -> assert false
    | RandValue (_, _) -> assert false
    | Nil -> assert false
    | Label _ -> assert false


exception Loop of effect_var list

let solve_constraints constrs =
  if debug then
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
  let () = List.iter (fun (x,y) -> tbl.(y) <- x::tbl.(y)) cgeqvars'' in
  let cgeqs_excep, cgeqs'' = List.partition (fun xe -> snd xe = EExcep) cgeqs' in
  let cgeqs_excep' = List.map fst cgeqs_excep in
  let cgeqs_cont, cgeqs''' = List.partition (fun xe -> snd xe = ECont) cgeqs'' in
  let cgeqs_cont' = List.map fst cgeqs_cont in
  let () = assert (cgeqs''' = []) in
  let solve_const c inits =
    let rec aux x =
      match sol.(x) with
          None ->
            sol.(x) <- Some c;
            List.iter aux tbl.(x)
        | Some e -> ()
    in
      List.iter aux inits ;
  in
    solve_const EExcep cgeqs_excep';
    solve_const ECont cgeqs_cont';
    sol.(0) <- Some ECont;
    fun e ->
      match sol.(e) with
          None -> ENone
        | Some e -> e





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
      | Label _ -> assert false
  in
    {desc=desc; typ=t.typ}

let add_preds_cont k t =
  let t' = add_preds_cont_aux k t in
  let ks = List.filter (Id.same k) (get_fv t') in
  Format.printf "APC: %a, %a ===> %a@." Id.print k pp_print_term t pp_print_term t';
    if List.length ks = 0
    then (assert (t.desc = Bottom); k, t')
    else (assert (List.length ks = 1); List.hd ks, t')


let rec force_cont = function
    TBaseCPS typ -> TBaseCPS typ
  | TFunCPS(e,typ1,typ2) -> TFunCPS(0, force_cont typ1, force_cont typ2)
  | TPairCPS(typ1,typ2) -> TPairCPS(force_cont typ1, force_cont typ2)

let rec trans_typ typ_orig typ =
  match typ_orig,typ with
    | _, TBaseCPS _ -> typ_orig
    | TFun(x_orig,typ), TFunCPS(e,typ1,typ2) when !sol e = EExcep ->
        let typ1' = trans_typ (Id.typ x_orig) typ1 in
        let x = Id.new_var "x" typ1' in
        let r = Id.new_var "r" (subst_type x_orig (make_var x) (trans_typ typ typ2)) in
        let k = Id.new_var "k" (TFun(r,TUnit)) in
        let e = Id.new_var "e" !typ_excep in
        let h = Id.new_var "h" (TFun(e,TUnit)) in
          TFun(x, TFun(k, TFun(h, TUnit)))
    | TFun(x_orig,typ), TFunCPS(e,typ1,typ2) when !sol e = ECont ->
        let typ1' = trans_typ (Id.typ x_orig) typ1 in
        let x = Id.new_var "x" typ1' in
        let r = Id.new_var "r" (subst_type x_orig (make_var x) (trans_typ typ typ2)) in
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
      EUnknown -> assert false
    | ENone -> make_app k [t]
    | ECont -> make_app t [k]
    | EExcep -> assert false

let make_app_excep e t k h =
  match !sol e with
      EUnknown -> assert false
    | ENone -> make_app k [t]
    | ECont -> make_app t [k]
    | EExcep -> make_app t [k; h]

let rec transform k_post {t_cps=t; typ_cps=typ; typ_orig=typ_orig; effect=e} =
  if debug then Format.printf "TRANS: @[%a@.@."
      print_typed_term {t_cps=t; typ_cps=typ; typ_orig=typ_orig; effect=e};
  let r =
    match t, !sol e with
        UnitCPS, ENone -> unit_term
      | TrueCPS, ENone -> true_term
      | FalseCPS, ENone -> false_term
      | IntCPS n, ENone -> make_int n
      | BottomCPS, ECont ->
          let r = Id.new_var "r" (trans_typ typ_orig typ) in
          let k = Id.new_var ("k" ^ k_post) (TFun(r,TUnit)) in
            make_fun k (make_bottom TUnit)
      | RandIntCPS, ENone ->
          let e = get_tfun_effect typ in
            begin
              match !sol e with
                  ECont -> make_randint_cps TUnit
                | EExcep ->
                    let e = Id.new_var "e" !typ_excep in
                    let h = Id.new_var "h" (TFun(e,TUnit)) in
                      make_fun h (make_randint_cps TUnit)
                | _ -> assert false
            end
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
      | FunCPS(x, t1), ENone when !sol (get_tfun_effect typ) = EExcep ->
          let x' = trans_var x in
          let r = Id.new_var "r" (trans_typ t1.typ_orig t1.typ_cps) in
          let k = Id.new_var ("k" ^ k_post) (TFun(r,TUnit)) in
          let e = Id.new_var "e" !typ_excep in
          let h = Id.new_var "h" (TFun(e,TUnit)) in
          let t1' = transform k_post t1 in
            make_fun x' (make_fun k (make_fun h (make_app_excep t1.effect t1' (make_var k) (make_var h))))
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
            make_fun k
              (make_app_cont t1.effect t1'
                 (make_fun x1
                    (make_app_cont t2.effect t2'
                       (make_fun x2
                          (make_app_cont e0 (make_app (make_var x1) [make_var x2]) (make_var k))))))
      | AppCPS(t1, t2), EExcep ->
          let t1' = transform k_post t1 in
          let t2' = transform k_post t2 in
          let r = Id.new_var "r" (trans_typ typ_orig typ) in
          let k = Id.new_var ("k" ^ k_post) (TFun(r,TUnit)) in
          let x1 = Id.new_var "x" (trans_typ t1.typ_orig t1.typ_cps) in
          let x2 = Id.new_var "x" (trans_typ t2.typ_orig t2.typ_cps) in
          let e = Id.new_var "e" !typ_excep in
          let h = Id.new_var "h" (TFun(e,TUnit)) in
          let h' = Id.new_var "h" (TFun(e,TUnit)) in
          let e0 = get_tfun_effect t1.typ_cps in
            make_fun k
              (make_fun h
                 (make_let [h', [], make_var h] (* to prevent the increase of code size in eta-reduction *)
                    (make_app_excep t1.effect t1'
                       (make_fun x1
                          (make_app_excep t2.effect t2'
                             (make_fun x2
                                (make_app_excep e0
                                   (make_app (make_var x1) [make_var x2]) (make_var k) (make_var h')))
                             (make_var h')))
                       (make_var h'))))
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
            make_fun k
              (make_let [k', [], make_var k]
                 (make_app_cont t1.effect t1'
                    (make_fun b
                       (make_if (make_var b)
                          (make_app_cont t2.effect t2' (make_var k'))
                          (make_app_cont t3.effect t3' (make_var k'))))))
      | IfCPS(t1, t2, t3), EExcep ->
          let t1' = transform k_post t1 in
          let t2' = transform k_post t2 in
          let t3' = transform k_post t3 in
          let r = Id.new_var "r" (trans_typ typ_orig typ) in
          let k = Id.new_var ("k" ^ k_post) (TFun(r,TUnit)) in
          let k' = Id.new_var ("k" ^ k_post) (TFun(r,TUnit)) in
          let b = Id.new_var "b" (trans_typ t1.typ_orig t1.typ_cps) in
          let e = Id.new_var "e" !typ_excep in
          let h = Id.new_var "h" (TFun(e,TUnit)) in
          let h' = Id.new_var "h" (TFun(e,TUnit)) in
            make_fun k
              (make_let [k', [], make_var k] (* to prevent the increase of code size in eta-reduction *)
                 (make_fun h
                    (make_let [h', [], make_var h] (* to prevent the increase of code size in eta-reduction *)
                       (make_app_excep t1.effect t1'
                          (make_fun b
                             (make_if (make_var b)
                                (make_app_excep t2.effect t2' (make_var k') (make_var h'))
                                (make_app_excep t3.effect t3' (make_var k') (make_var h'))))
                          (make_var h')))))
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
      | LetCPS(flag, bindings, t1), EExcep ->
          let r = Id.new_var "r" (trans_typ typ_orig typ) in
          let k = Id.new_var ("k" ^ k_post) (TFun(r,TUnit)) in
          let e = Id.new_var "e" !typ_excep in
          let h = Id.new_var "h" (TFun(e,TUnit)) in
          let h' = Id.new_var "h" (TFun(e,TUnit)) in
          let xs = List.map (fun (_,t) -> Id.new_var "x" (trans_typ t.typ_orig t.typ_cps)) bindings in
          let bindings' = List.map2 (fun x (f,_) -> trans_var f, [], make_var x) xs bindings in
          let aux (f,t) =
            let f' = trans_var f in
              t.effect, transform (k_post ^ "_" ^ Id.name f') t
          in
          let ets = List.map aux bindings in
          let t1' = transform k_post t1 in
          let t0 = make_let_f flag bindings' (make_app_excep t1.effect t1' (make_var k) (make_var h')) in
          let aux x (e,t) t' = make_app_excep e t (make_fun x t') (make_var h') in
            make_fun k
              (make_fun h
                 (make_let [h', [], make_var h] (* to prevent the increase of code size in eta-reduction *)
                    (List.fold_right2 aux xs ets t0)))
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
            make_fun k
              (make_app_cont t1.effect t1'
                 (make_fun x1
                    (make_app_cont t2.effect t2'
                       (make_fun x2
                          (make_app (make_var k) [{desc=BinOp(op, make_var x1, make_var x2); typ=typ_orig}])))))
      | BinOpCPS(op, t1, t2), EExcep ->
          let r = Id.new_var "r" (trans_typ typ_orig typ) in
          let k = Id.new_var ("k" ^ k_post) (TFun(r,TUnit)) in
          let x1 = Id.new_var "x" (trans_typ t1.typ_orig t1.typ_cps) in
          let x2 = Id.new_var "x" (trans_typ t2.typ_orig t2.typ_cps) in
          let e = Id.new_var "e" !typ_excep in
          let h = Id.new_var "h" (TFun(e,TUnit)) in
          let h' = Id.new_var "h" (TFun(e,TUnit)) in
          let t1' = transform k_post t1 in
          let t2' = transform k_post t2 in
            make_fun k
              (make_fun h
                 (make_let [h', [], make_var h] (* to prevent the increase of code size in eta-reduction *)
                    (make_app_excep t1.effect t1'
                       (make_fun x1
                          (make_app_excep t2.effect t2'
                             (make_fun x2
                                (make_app (make_var k)
                                   [{desc=BinOp(op, make_var x1, make_var x2); typ=typ_orig}]))
                             (make_var h')))
                       (make_var h'))))
      | NotCPS t1, ENone ->
          let t1' = transform k_post t1 in
            make_not t1'
      | NotCPS t1, ECont ->
          let r = Id.new_var "r" (trans_typ typ_orig typ) in
          let k = Id.new_var ("k" ^ k_post) (TFun(r,TUnit)) in
          let b = Id.new_var "b" (trans_typ t1.typ_orig t1.typ_cps) in
          let t1' = transform k_post t1 in
            make_fun k (make_app_cont t1.effect t1' (make_fun b (make_app (make_var k) [make_not (make_var b)])))
      | NotCPS t1, EExcep ->
          let r = Id.new_var "r" (trans_typ typ_orig typ) in
          let k = Id.new_var ("k" ^ k_post) (TFun(r,TUnit)) in
          let b = Id.new_var "b" (trans_typ t1.typ_orig t1.typ_cps) in
          let e = Id.new_var "e" !typ_excep in
          let h = Id.new_var "h" (TFun(e,TUnit)) in
          let t1' = transform k_post t1 in
            make_fun k
              (make_fun h
                 (make_app_excep t1.effect t1'
                    (make_fun b (make_app (make_var k) [make_not (make_var b)]))
                    (make_var h)))
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
      | FstCPS t1, EExcep ->
          let r = Id.new_var "r" (trans_typ typ_orig typ) in
          let k = Id.new_var ("k" ^ k_post) (TFun(r,TUnit)) in
          let p = Id.new_var "p" (trans_typ t1.typ_orig t1.typ_cps) in
          let e = Id.new_var "e" !typ_excep in
          let h = Id.new_var "h" (TFun(e,TUnit)) in
          let t1' = transform k_post t1 in
            make_fun k
              (make_fun h
                 (make_app_excep t1.effect t1'
                    (make_fun p
                       (make_app (make_var k) [make_fst (make_var p)]))
                    (make_var h)))
      | SndCPS t1, ENone ->
          let t1' = transform k_post t1 in
            make_snd t1'
      | SndCPS t1, ECont ->
          let r = Id.new_var "r" (trans_typ typ_orig typ) in
          let k = Id.new_var ("k" ^ k_post) (TFun(r,TUnit)) in
          let p = Id.new_var "p" (trans_typ t1.typ_orig t1.typ_cps) in
          let t1' = transform k_post t1 in
            make_fun k (make_app_cont t1.effect t1' (make_fun p (make_app (make_var k) [make_snd (make_var p)])))
      | SndCPS t1, EExcep ->
          let r = Id.new_var "r" (trans_typ typ_orig typ) in
          let k = Id.new_var ("k" ^ k_post) (TFun(r,TUnit)) in
          let p = Id.new_var "p" (trans_typ t1.typ_orig t1.typ_cps) in
          let e = Id.new_var "e" !typ_excep in
          let h = Id.new_var "h" (TFun(e,TUnit)) in
          let t1' = transform k_post t1 in
            make_fun k
              (make_fun h
                 (make_app_excep t1.effect t1'
                    (make_fun p
                       (make_app (make_var k) [make_snd (make_var p)]))
                    (make_var h)))
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
            make_fun k
              (make_app_cont t1.effect t1'
                 (make_fun x1
                    (make_app_cont t2.effect t2'
                       (make_fun x2
                          (make_app (make_var k) [make_pair (make_var x1) (make_var x2)])))))
      | PairCPS(t1,t2), EExcep ->
          let r = Id.new_var "r" (trans_typ typ_orig typ) in
          let k = Id.new_var ("k" ^ k_post) (TFun(r,TUnit)) in
          let x1 = Id.new_var "x" (trans_typ t1.typ_orig t1.typ_cps) in
          let x2 = Id.new_var "x" (trans_typ t2.typ_orig t2.typ_cps) in
          let e = Id.new_var "e" !typ_excep in
          let h = Id.new_var "h" (TFun(e,TUnit)) in
          let h' = Id.new_var "h" (TFun(e,TUnit)) in
          let t1' = transform k_post t1 in
          let t2' = transform k_post t2 in
            make_fun k
              (make_fun h
                 (make_let [h', [], make_var h] (* to prevent the increase of code size in eta-reduction *)
                    (make_app_excep t1.effect t1'
                       (make_fun x1
                          (make_app_excep t2.effect t2'
                             (make_fun x2
                                (make_app (make_var k) [make_pair (make_var x1) (make_var x2)]))
                             (make_var h')))
                       (make_var h'))))
      | RaiseCPS t1, EExcep ->
          let u = Id.new_var "u" (trans_typ typ_orig typ) in
          let k = Id.new_var "k" (TFun(u,TUnit)) in
          let e = Id.new_var "e" !typ_excep in
          let h = Id.new_var "h" (TFun(e,TUnit)) in
          let h' = Id.new_var "h" (TFun(e,TUnit)) in
          let t1' = transform k_post t1 in
            make_fun k
              (make_fun h
                 (make_let [h', [], make_var h] (* to prevent the increase of code size in eta-reduction *)
                    (make_app_excep t1.effect t1' (make_var h') (make_var h'))))
      | TryWithCPS(t1,t2), ENone ->
          transform k_post t1
      | TryWithCPS(t1,t2), ECont ->
          transform k_post t1
      | TryWithCPS(t1,t2), EExcep ->
          let r = Id.new_var "r" (trans_typ t1.typ_orig t1.typ_cps) in
          let f = Id.new_var "h" (trans_typ t2.typ_orig t2.typ_cps) in
          let k = Id.new_var "k" (TFun(r,TUnit)) in
          let e = Id.new_var "e" !typ_excep in
          let h = Id.new_var "h" (TFun(e,TUnit)) in
          let t1' = transform k_post t1 in
          let t2' = transform k_post t2 in
            assert (!sol t2.effect = ENone); (* bind h' to h when eliminating this assertion *)
            make_fun k
              (make_fun h
                 (make_app_excep t1.effect t1'
                    (make_var k)
                    (make_fun e
                       (make_app_excep t2.effect
                          t2'
                          (make_fun f
                             (make_app_excep (get_tfun_effect t2.typ_cps)
                                (make_app (make_var f) [make_var e]) (make_var k) (make_var h)))
                          (make_var h)))))
      | t, e -> (Format.printf "%a, %a@." print_t_cps t print_effect e; assert false)
  in
    if debug then Format.printf "%a@. ===>@. %a@.@."
      print_typed_term {t_cps=t; typ_cps=typ; typ_orig=typ_orig; effect=e}
      pp_print_term r;
    r








let rec short_circuit_eval t =
  let desc =
    match t.desc with
        Unit
      | True
      | False
      | Unknown
      | Int _
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
      | Label(info,t) -> Label(info, short_circuit_eval t)
  in
    {desc=desc; typ=t.typ}



let rec assoc_typ_cps f {t_cps=t; typ_cps=typ; typ_orig=typ_orig; effect=e} =
  match t with
      UnitCPS -> []
    | TrueCPS -> []
    | FalseCPS -> []
    | IntCPS n -> []
    | BottomCPS -> []
    | RandIntCPS -> []
    | VarCPS x -> []
    | FunCPS(x, t1) ->
        assoc_typ_cps f t1
    | AppCPS(t1, t2) ->
        assoc_typ_cps f t1 @@ assoc_typ_cps f t2
    | IfCPS(t1, t2, t3) ->
        assoc_typ_cps f t1 @@ assoc_typ_cps f t2 @@ assoc_typ_cps f t3
    | LetCPS(flag, bindings, t1) ->
        let aux (g,t) =
          let typs1 = if Id.same f g.id_cps then [g.id_typ] else [] in
            typs1 @@ assoc_typ_cps f t
        in
          assoc_typ_cps f t1 @@ rev_flatten_map aux bindings
    | BinOpCPS(op, t1, t2) ->
        assoc_typ_cps f t1 @@ assoc_typ_cps f t2
    | NotCPS t1 ->
        assoc_typ_cps f t1
    | UnknownCPS -> []
    | EventCPS s -> []
    | FstCPS t1 ->
        assoc_typ_cps f t1
    | SndCPS t1 ->
        assoc_typ_cps f t1
    | PairCPS(t1,t2) ->
        assoc_typ_cps f t1 @@ assoc_typ_cps f t2
    | RaiseCPS t1 ->
        assoc_typ_cps f t1
    | TryWithCPS(t1,t2) ->
        assoc_typ_cps f t1 @@ assoc_typ_cps f t2

let assoc_typ_cps f typed =
  match assoc_typ_cps f typed with
      [] -> raise Not_found
    | [typ] -> typ
    | typs -> Format.printf "%a: %d@." Id.print f (List.length typs); assert false


let rec uncps_ref_type rtyp e etyp =
  if debug then Format.printf "rtyp:%a@.e:%a@.etyp:%a@.@."
    RT.print rtyp print_effect e print_typ_cps etyp;
  match rtyp, e, etyp with
      RT.Inter rtyps, ENone, _ ->
        RT.Inter (List.map (fun rtyp1 -> uncps_ref_type rtyp1 e etyp) rtyps)
    | RT.Base(b,x,ps), ENone, TBaseCPS _ -> RT.Base(b,x,ps)
    | RT.Fun(x,rtyp1,rtyp2), ENone, TFunCPS(e,etyp1,etyp2) ->
        let rtyp1' = uncps_ref_type rtyp1 ENone etyp1 in
        let rtyp2' = uncps_ref_type rtyp2 (!sol e) etyp2 in
          RT.Fun(x, rtyp1', rtyp2')
    | RT.Fun(_, RT.Fun(_,rtyp,RT.Base(RT.Unit,_,_)), RT.Base(RT.Unit,_,_)),
      ECont, _ ->
        uncps_ref_type rtyp ENone etyp
    | RT.Fun(_, RT.Fun(_,rtyp, RT.Base(RT.Unit,_,_)), RT.Fun(_,_,RT.Base(RT.Unit,_,_))),
      EExcep, _ -> (* TODO: refine *)
        uncps_ref_type rtyp ENone etyp
    | RT.Fun(_, RT.Inter rtyps, RT.Base(RT.Unit,_,_)), ECont, _ ->
        let aux = function
            RT.Fun(_,rtyp1,RT.Base(RT.Unit,_,_)) -> uncps_ref_type rtyp1 ENone etyp
          | _ -> assert false
        in
          RT.Union (List.map aux rtyps)
    | RT.Pair(x,rtyp1,rtyp2), _, TPairCPS(etyp1,etyp2) ->
        let rtyp1' = uncps_ref_type rtyp1 e etyp1 in
        let rtyp2' = uncps_ref_type rtyp2 e etyp2 in
          RT.Pair(x, rtyp1', rtyp2')
    | RT.ExtArg(x,rtyp1,rtyp2), _, _ ->
        RT.ExtArg(x, rtyp1, uncps_ref_type rtyp2 e etyp)
    | _ ->
        Format.printf "rtyp:%a@.e:%a@.etyp:%a@."
          RT.print rtyp print_effect e print_typ_cps etyp;
        assert false

let get_rtyp_of typed f rtyp =
  let etyp = assoc_typ_cps f typed in
  if debug then Format.printf "%a:@.rtyp:%a@.etyp:%a@.@."
    Id.print f RT.print rtyp print_typ_cps etyp;
    uncps_ref_type rtyp ENone etyp



let trans t =
  let () = sol := fun (n:int) -> if n = 0 then ECont else EUnknown in
  let () = typ_excep := trans_typ !typ_excep (force_cont (infer_effect_typ !typ_excep)) in
  let t = short_circuit_eval t in
  let () = counter := 0 in
  let () = constraints := [] in
  let typed = infer_effect [] t in
  let () = if debug then Format.printf "CPS_infer_effect:@.%a@." print_typed_term typed in
  let () = sol := solve_constraints !constraints in
  let () = if debug then Format.printf "CPS_infer_effect:@.%a@." print_typed_term typed in
  let x = Id.new_var "x" TUnit in
  let t = transform "" typed in
  let e = Id.new_var "e" !typ_excep in
  let k = make_fun x (make_var x) in
  let h = make_fun e (make_app fail_term [unit_term]) in
  let t = make_app_excep typed.effect t k h in
  let () = if debug then Format.printf "CPS:@.%a@." pp_print_term' t in
  let t = Trans.propagate_typ_arg t in
  let t = Trans.eta_reduce t in
  let t = Trans.expand_let_val t in
    Type_check.check t TUnit;
    t, get_rtyp_of typed
