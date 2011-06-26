
open Syntax
open Util


type typed_term = {t_cps:t_cps; typ_cps:typ_cps}
and typed_ident = {id_cps:ident; id_typ:typ_cps}
and t_cps =
    UnitCPS
  | TrueCPS
  | FalseCPS
  | UnknownCPS
  | IntCPS of int
  | NIntCPS of typed_ident
  | RandIntCPS
  | VarCPS of typed_ident
  | FunCPS of typed_ident * typed_term
  | AppCPS of typed_term * typed_term list
  | IfCPS of typed_term * typed_term * typed_term
  | BranchCPS of typed_term * typed_term
  | LetCPS of rec_flag * typed_ident * typed_ident list * typed_term * typed_term
  | BinOpCPS of binop * typed_term * typed_term
  | NotCPS of typed_term
  | FailCPS
  | LabelCPS of bool * typed_term
  | EventCPS of string
  | RecordCPS of bool * (string * (mutable_flag * typed_term)) list
  | ProjCPS of int option * int * string * mutable_flag * typed_term
  | NilCPS
  | ConsCPS of typed_term * typed_term
  | MatchCPS of typed_term * typed_term * typed_ident * typed_ident * typed_term
  | ConstrCPS of string * typed_term list
  | Match_CPS of typed_term * (typed_pattern * typed_term option * typed_term) list
  | Type_declCPS of (string * (typ list * type_kind)) list * typed_term
and typ_cps =
    TBaseCPS
  | TVarCPS of typ_cps option ref
  | TFunCPS of bool ref * typ_cps * typ_cps
  | TListCPS of typ_cps
  | TVariantCPS of (string * typ_cps list) list
  | TRecordCPS of bool * (string * (mutable_flag * typ_cps)) list
  | TAbsCPS of string
  | TUnknownCPS
and typed_pattern = {pat_cps:pattern_cps; pat_typ:typ_cps}
and pattern_cps =
    PVarCPS of typed_ident
  | PConstCPS of typed_term
  | PConstructCPS of string * typed_pattern list
  | PRecordCPS of bool * (int * (string * mutable_flag * typed_pattern)) list
  | POrCPS of typed_pattern * typed_pattern



let rec print_typ_cps fm = function
    TBaseCPS -> Format.fprintf fm "o"
  | TVarCPS {contents = None} -> Format.fprintf fm "?"
  | TVarCPS {contents = Some typ} -> Format.fprintf fm "[%a]" print_typ_cps typ
  | TFunCPS({contents=b},typ1,typ2) ->
      Format.fprintf fm "(%a %s %a)" print_typ_cps typ1 (if b then "=>" else "->") print_typ_cps typ2
  | TListCPS typ -> Format.fprintf fm "%a list" print_typ_cps typ
  | TVariantCPS ctypss ->
      let aux (c,typs) =
        let rec aux' fm = function
            [] -> ()
          | [typ] -> print_typ_cps fm typ
          | typ::typs -> Format.fprintf fm "%a * %a" print_typ_cps typ aux' typs
        in
          Format.fprintf fm "| %s " c;
          if typs <> [] then Format.pp_print_string fm "of ";
          Format.fprintf fm "%a " aux' typs
      in
        List.iter aux ctypss
  | TRecordCPS(b,typs) ->
      let rec aux fm = function
          [] -> ()
        | (s,(f,typ))::fields ->
            match is_int s, fields=[] with
                true,true -> Format.fprintf fm "%a" print_typ_cps typ
              | true,false -> Format.fprintf fm "%a * %a" print_typ_cps typ aux fields
              | false,true -> Format.fprintf fm "%s:%a" s print_typ_cps typ
              | false,false -> Format.fprintf fm "%s:%a; %a" s print_typ_cps typ aux fields
      in
        if b
        then Format.fprintf fm "(%a)" aux typs
        else Format.fprintf fm "{%a}" aux typs
  | TUnknownCPS -> Format.fprintf fm "???"
  | TAbsCPS s -> Format.pp_print_string fm s

and print_typed_termlist fm = List.iter (fun bd -> Format.fprintf fm "@;%a" print_typed_term bd)

and print_typed_term fm {t_cps=t; typ_cps=typ} =
  Format.fprintf fm "(%a : %a)" print_t_cps t print_typ_cps typ

and print_t_cps fm = function
    UnitCPS -> Format.fprintf fm "unit"
  | TrueCPS -> Format.fprintf fm "true"
  | FalseCPS -> Format.fprintf fm "false"
  | UnknownCPS -> Format.fprintf fm "***"
  | IntCPS n -> Format.fprintf fm "%d" n
  | NIntCPS x -> Format.fprintf fm "?%a?" print_id x.id_cps
  | RandIntCPS -> Format.fprintf fm "rand_int()"
  | VarCPS x -> print_id fm x.id_cps
  | FunCPS(x, t) ->
      Format.fprintf fm "fun %a -> %a" print_id x.id_cps print_typed_term t
  | AppCPS(t, ts) ->
      Format.fprintf fm "%a%a" print_typed_term t print_typed_termlist ts
  | IfCPS(t1, t2, t3) ->
      Format.fprintf fm "@[@[if %a@]@;then @[%a@]@;else @[%a@]@]"
        print_typed_term t1 print_typed_term t2 print_typed_term t3
  | BranchCPS(t1, t2) ->
      Format.fprintf fm "br %a %a" print_typed_term t1 print_typed_term t2
  | LetCPS(flag, f, xs, t1, t2) ->
      let s_rec = match flag with Nonrecursive -> "" | Recursive -> " rec" in
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
  | FailCPS -> Format.fprintf fm "fail"
  | LabelCPS(true, t) ->
      Format.fprintf fm "l_then %a" print_typed_term t
  | LabelCPS(false, t) ->
      Format.fprintf fm "l_else %a" print_typed_term t
  | EventCPS s -> Format.fprintf fm "{%s}" s
  | RecordCPS(b,fields) ->
      let rec aux fm = function
          [] -> ()
        | (s,(_,t))::fields ->
            match b, fields=[] with
                true,true -> Format.fprintf fm "%a" print_typed_term t
              | true,false -> Format.fprintf fm "%a, %a" print_typed_term t aux fields
              | false,true -> Format.fprintf fm "%s=%a" s print_typed_term t
              | false,false -> Format.fprintf fm "%s=%a; %a" s print_typed_term t aux fields
      in
        if is_int (fst (List.hd fields))
        then Format.fprintf fm "(%a)" aux fields
        else Format.fprintf fm "{%a}" aux fields
  | NilCPS -> Format.fprintf fm "[]"
  | ConsCPS(t1,t2) ->
      Format.fprintf fm "%a::%a" print_typed_term t1 print_typed_term t2
  | ConstrCPS(s,ts) ->
      let aux fm = function
          [] -> ()
        | [t] -> Format.fprintf fm "(%a)" print_typed_term t
        | t::ts ->
            Format.fprintf fm "(%a" print_typed_term t;
            List.iter (Format.fprintf fm ",%a" print_typed_term) ts;
            Format.pp_print_string fm ")"
      in
        Format.fprintf fm "%s%a" s aux ts
  | MatchCPS _ -> assert false
  | Match_CPS(t,pats) ->
      let aux = function
          (pat,None,t) -> Format.fprintf fm "| ... -> %a@;" print_typed_term t
        | (pat,Some cond,t) -> Format.fprintf fm "| ... when %a -> %a@;" print_typed_term cond print_typed_term t
      in
        Format.fprintf fm "match %a with@;" print_typed_term t;
        List.iter aux pats
  | Type_declCPS(decls,t) ->
      let aux (x,kind) =
        Format.fprintf fm "type %s = ...@." x
      in
        List.iter aux decls;
        print_typed_term fm t





let rec find_var_typ x env =
  try
    match List.assoc x env with
        TAbsCPS s -> find_var_typ s env
      | typ -> typ
  with Not_found ->
    if Flag.debug
    then Format.printf "@.not found: %s@." x;
    assert false



let new_tvar () = TVarCPS (ref None)
let new_var x = {id_cps=x; id_typ=new_tvar()}

let rec set_tfun = function
    TBaseCPS -> assert false
  | TFunCPS(r, typ1, typ2) -> r := true
(*
  | TVarCPS({contents=Some typ}) -> set_tfun typ
*)
  | _ -> ()

let rec typ_cps_of_typ = function
    TUnit
  | TBool
  | TAbsBool
  | TInt _
  | TRInt _ -> TBaseCPS
  | TVar _ -> assert false
  | TFun((x,typ1),typ2) ->
      let typ_cps1 = typ_cps_of_typ typ1 in
      let typ_cps2 = typ_cps_of_typ typ2 in
        TFunCPS(ref false,typ_cps1,typ_cps2)
  | TList(typ,_) ->
      let typ_cps = typ_cps_of_typ typ in
        TListCPS typ_cps
  | TVariant ctypss ->
      let aux (c,typs) = c, List.map typ_cps_of_typ typs in
        TVariantCPS (List.map aux ctypss)
  | TRecord(b,fields) ->
      TRecordCPS(b,List.map (fun (s,(f,typ)) -> s,(f,typ_cps_of_typ typ)) fields)
  | TUnknown -> TUnknownCPS

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
      TBaseCPS, TBaseCPS -> TBaseCPS
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
    | TFunCPS(b1,typ11,typ12), TFunCPS(b2,typ21,typ22) ->
        let b = max !b1 !b2 in
        let () = b1 := b in
        let () = b2 := b in
        let typ1' = unify typ11 typ21 in
        let typ2' = unify typ12 typ22 in
          TFunCPS(b1,typ1',typ2')
    | TListCPS typ1, TListCPS typ2 ->
        let typ = unify typ1 typ2 in
          TListCPS typ
    | TVariantCPS ctypss1, TVariantCPS ctypss2 ->
        assert (ctypss1 = ctypss2);
        TVariantCPS ctypss1
    | TRecordCPS(b1,typs1), TRecordCPS(b2,typs2) ->
        let ss = uniq (List.map fst (typs1 @@ typs2)) in
        let aux s =
          let find typs = try snd (List.assoc s typs) with Not_found -> new_tvar() in 
            ignore (unify (find typs1) (find typs2))
        in
          List.iter aux ss;
          if List.length typs1 > List.length typs2
          then TRecordCPS(b1,typs1)
          else TRecordCPS(b2,typs2)
    | typ1,typ2 -> Format.printf "typ1: %a@.typ2: %a@." print_typ_cps typ1 print_typ_cps typ2; assert false
let unify typ1 typ2 = ignore (unify typ1 typ2)

let rec infer_cont_pos env : Syntax.t -> typed_term = function
    Unit -> {t_cps=UnitCPS; typ_cps=TBaseCPS}
  | True -> {t_cps=TrueCPS; typ_cps=TBaseCPS}
  | False -> {t_cps=FalseCPS; typ_cps=TBaseCPS}
  | Unknown -> {t_cps=UnknownCPS; typ_cps=TBaseCPS}
  | Int n -> {t_cps=IntCPS n; typ_cps=TBaseCPS}
  | NInt x -> {t_cps=NIntCPS{id_cps=x;id_typ=TBaseCPS}; typ_cps=TBaseCPS}
  | RandInt t ->
      assert (t = None);
      {t_cps=RandIntCPS; typ_cps=TBaseCPS}
  | Var x ->
      let typ = try List.assoc x.name env with Not_found -> assert false in
        {t_cps=VarCPS{id_cps=x;id_typ=typ}; typ_cps=typ}
  | Fun(x, t1) -> assert false
  | App(t1, ts) ->
      let typed1 = infer_cont_pos env t1 in
      let typeds = List.map (infer_cont_pos env) ts in
      let typ_result = new_tvar () in
      let aux typed (typ,b) = TFunCPS(ref b,typed.typ_cps,typ), false in
      let typ,_ = List.fold_right aux typeds (typ_result,true) in
        unify typed1.typ_cps typ;
        {t_cps=AppCPS(typed1,typeds); typ_cps=typ_result}
  | If(t1, t2, t3) ->
      let typed1 = infer_cont_pos env t1 in
      let typed2 = infer_cont_pos env t2 in
      let typed3 = infer_cont_pos env t3 in
        unify typed1.typ_cps TBaseCPS;
        unify typed2.typ_cps typed3.typ_cps;
        {t_cps=IfCPS(typed1,typed2,typed3); typ_cps=typed2.typ_cps}
  | Branch(t1, t2) ->
      let typed1 = infer_cont_pos env t1 in
      let typed2 = infer_cont_pos env t2 in
        unify typed1.typ_cps typed2.typ_cps;
        {t_cps=BranchCPS(typed1,typed2); typ_cps=typed1.typ_cps}
  | Let(flag, f, xs, t1, t2) ->
      let typ_f = new_tvar () in
      let f' = {id_cps=f; id_typ=typ_f} in
      let typ_args = List.map (fun _ -> new_tvar ()) xs in
      let xs' = List.map2 (fun x typ -> {id_cps=x; id_typ=typ}) xs typ_args in
      let env2 = (f.name, typ_f) :: env in
      let env1 = List.combine (List.map (fun x -> x.name) xs) typ_args @@ if flag = Nonrecursive then env else env2 in
      let typed1 = infer_cont_pos env1 t1 in
      let typed2 = infer_cont_pos env2 t2 in
      let b = ref true in
      let aux typ1 typ2 =
        let typ = TFunCPS(ref !b,typ1,typ2) in
          b := false; typ
      in
      let typ = List.fold_right aux typ_args typed1.typ_cps in
        unify typ_f typ;
        {t_cps=LetCPS(flag,f',xs',typed1,typed2); typ_cps=typed2.typ_cps}
  | BinOp(op, t1, t2) ->
      let typed1 = infer_cont_pos env t1 in
      let typed2 = infer_cont_pos env t2 in
        unify typed1.typ_cps typed2.typ_cps;
        {t_cps=BinOpCPS(op,typed1,typed2); typ_cps=TBaseCPS}
  | Not t ->
      let typed = infer_cont_pos env t in
        unify typed.typ_cps TBaseCPS;
        {t_cps=NotCPS typed; typ_cps=TBaseCPS}
  | Fail ->
      let typ = new_tvar () in
        {t_cps=FailCPS; typ_cps=TFunCPS(ref false,TBaseCPS,typ)}
  | Label(b,t) ->
      let typed = infer_cont_pos env t in
        {t_cps=LabelCPS(b, typed); typ_cps=typed.typ_cps}
  | Event s ->
      {t_cps=EventCPS s; typ_cps=TFunCPS(ref false, TBaseCPS, TBaseCPS)}
  | Record(b,fields) ->
      let aux (s,(f,t)) =
        let typed = infer_cont_pos env t in
          (s,(f,typed)), (s,(f,typed.typ_cps))
      in
      let fields',typs = List.split (List.map aux fields) in
        {t_cps=RecordCPS(b,fields'); typ_cps=TRecordCPS(b,typs)}
  | Proj(n,i,s,f,t) ->
      let typed = infer_cont_pos env t in
      let typ_pi = new_tvar() in
        unify typed.typ_cps (TRecordCPS(n<>None,[s,(f,typ_pi)]));
        {t_cps=ProjCPS(n,i,s,f,typed); typ_cps=typ_pi}
  | Nil ->
      let typ_f = new_tvar () in
        {t_cps=NilCPS; typ_cps=typ_f}
  | Cons(t1,t2) ->
      let typed1 = infer_cont_pos env t1 in
      let typed2 = infer_cont_pos env t2 in
        unify (TListCPS typed1.typ_cps) typed2.typ_cps;
        {t_cps=ConsCPS(typed1,typed2); typ_cps=typed2.typ_cps}
  | Constr(c,ts) ->
      let aux typ t =
        let typed = infer_cont_pos env t in
          unify typ typed.typ_cps;
          typed
      in
      let typ = find_var_typ c env in
      let typs =
        match typ with
            TVariantCPS vtyps -> List.assoc c vtyps
          | _ -> assert false
      in
        {t_cps=ConstrCPS(c, List.map2 aux typs ts); typ_cps=typ}
  | Match(t1,t2,x,y,t3) ->
      let x' = new_var x in
      let y' = new_var y in
      let env' = (x.name,x'.id_typ)::(y.name,y'.id_typ)::env in
      let typed1 = infer_cont_pos env t1 in
      let typed2 = infer_cont_pos env t2 in
      let typed3 = infer_cont_pos env' t3 in
        unify (TListCPS x'.id_typ) y'.id_typ;
        unify typed1.typ_cps y'.id_typ;
        unify typed2.typ_cps typed3.typ_cps;
        {t_cps=MatchCPS(typed1,typed2,x',y',typed3); typ_cps=typed2.typ_cps}
  | Match_(t,pats) ->
      let typed = infer_cont_pos env t in
      let typ = new_tvar () in
      let aux (pat,cond,t) pats =
        let typed_pat,env' = infer_pattern_cont_pos env pat in
        let typed' = infer_cont_pos env' t in
        let cond' =
          match cond with
              None -> None
            | Some t ->
                let typed = infer_cont_pos env t in
                  unify typed.typ_cps TBaseCPS;
                  Some typed
        in
          unify typed.typ_cps typed_pat.pat_typ;
          unify typ typed'.typ_cps;
          (typed_pat,cond',typed')::pats
      in
      let pats' = List.fold_right aux pats [] in
        {t_cps=Match_CPS(typed,pats'); typ_cps=typ}
  | Type_decl(decls,t) ->
      let rec aux env (x,(_,kind)) =
        let typ = TAbsCPS x in
        let aux typ_cps typs =
          let env0 = (x, typ_cps)::env in
            List.fold_left (fun env (y,_) -> (y,typ)::env) env0 typs
        in
          match kind with
              KVariant ctypss -> aux (typ_cps_of_typ (TVariant ctypss)) ctypss
            | KRecord fields -> aux (typ_cps_of_typ (TRecord(true,fields))) fields
      in
      let env' = List.fold_left aux env decls in
      let typed = infer_cont_pos env' t in
        {t_cps=Type_declCPS(decls, typed); typ_cps=typed.typ_cps}

and infer_pattern_cont_pos env = function
    PVar x ->
      let x' = new_var x in
      let typ = x'.id_typ in
        {pat_cps=PVarCPS x'; pat_typ=typ}, (x.name,typ)::env
  | PConst c ->
      let c' = infer_cont_pos env c in
        {pat_cps=PConstCPS c'; pat_typ=c'.typ_cps}, env
  | PConstruct(x,pats) ->
      let typ = find_var_typ x env in
      let vtyps =
        match typ with
            TVariantCPS vtyps -> vtyps
          | _ -> assert false
      in
      let typs = List.assoc x vtyps in
      let aux pat typ (pats,env) =
        let pat',env' = infer_pattern_cont_pos env pat in
          unify typ pat'.pat_typ;
          pat'::pats, env'
      in
      let pats',env' = List.fold_right2 aux pats typs ([],env) in
        {pat_cps=PConstructCPS(x,pats'); pat_typ=typ}, env'
  | PRecord(b,fields) -> assert false
  | POr(pat1,pat2) ->
      let pat1',env1 = infer_pattern_cont_pos env pat1 in
      let pat2',env2 = infer_pattern_cont_pos env1 pat2 in
        unify pat1'.pat_typ pat2'.pat_typ;
        {pat_cps=POrCPS(pat1',pat2'); pat_typ=pat1'.pat_typ}, env2



let funs = ref []


let rec term_of_typed_term {t_cps=t} =
  match t with
      UnitCPS -> Unit
    | TrueCPS -> True
    | FalseCPS -> False
    | UnknownCPS -> Unknown
    | IntCPS n -> Int n
    | _ -> assert false

let rec pattern_of_typed_pat {pat_cps=pat} =
  match pat with
      PVarCPS x -> PVar x.id_cps
    | PConstCPS c -> PConst (term_of_typed_term c)
    | PConstructCPS(c,pats) -> PConstruct(c,List.map pattern_of_typed_pat pats)
    | PRecordCPS(b,cpats) ->
        let aux (i,(c,f,pat)) = i, (c, f, pattern_of_typed_pat pat) in
          PRecord(b,List.map aux cpats)
    | POrCPS(pat1,pat2) -> POr(pattern_of_typed_pat pat1, pattern_of_typed_pat pat2)


let rec get_arg_num = function
    TBaseCPS -> 0
  | TVarCPS{contents=None} -> 0
  | TVarCPS{contents=Some typ} -> get_arg_num typ
  | TFunCPS({contents=true},typ1,typ2) -> 1
  | TFunCPS({contents=false},typ1,typ2) -> 1 + get_arg_num typ2
  | TListCPS _ -> 0

let rec app_typ typ typs =
  match typ,typs with
      TVarCPS{contents=Some typ},_ -> app_typ typ typs
    | TFunCPS(_,_,typ2), _::typs' -> app_typ typ2 typs'
    | _, [] -> typ
    | _ -> assert false

let rec transform c {t_cps=t; typ_cps=typ} =
  match t with
      UnitCPS -> c Unit
    | TrueCPS -> c True
    | FalseCPS -> c False
    | IntCPS n -> c (Int n)
    | NIntCPS x -> c (NInt x.id_cps)
    | RandIntCPS ->
        let k = new_var' "k" in
        let r = new_var' "r" in
          RandInt (Some (Let(Nonrecursive, k, [r], c (Var r), Var k)))
    | VarCPS x -> c (Var x.id_cps)
    | FunCPS(x, t) -> assert false
    | AppCPS(t1, ts) ->
        let n = get_arg_num t1.typ_cps in
          if n = List.length ts
          then
            let k = new_var' "k" in
            let r = new_var' "r" in
            let c1 x = app2app x [Var k] in
            let cc = List.fold_right (fun t cc -> fun x -> transform (fun y -> cc (app2app x [y])) t) ts c1 in
              funs := k::!funs;
              Let(Nonrecursive, k, [r], c (Var r), transform cc t1)
          else
            let ts1,ts2 = take2 ts n in
            let typ' = app_typ t1.typ_cps ts1 in
              transform c {t_cps=AppCPS({t_cps=AppCPS(t1,ts1);typ_cps=typ'},ts2); typ_cps=typ}
    | IfCPS(t1, t2, t3) ->
        let k = new_var' "k" in
        let x = new_var' "b" in
        let t2' = transform (fun y -> App(Var k, [y])) t2 in
        let t3' = transform (fun y -> App(Var k, [y])) t3 in
        let c' y = Let(Nonrecursive, k, [x], c (Var x), If(y, t2', t3')) in
          funs := k::!funs;
          transform c' t1
    | BranchCPS(t1, t2) ->
        let k = new_var' "k" in
        let x = new_var' "b" in
        let t1' = transform (fun y -> App(Var k, [y])) t1 in
        let t2' = transform (fun y -> App(Var k, [y])) t2 in
          funs := k::!funs;
          Let(Nonrecursive, k, [x], c (Var x), Branch(t1', t2'))
    | LetCPS(flag, f, xs, t1, t2) ->
        if xs = []
        then
          let c' t = subst f.id_cps t (transform c t2) in
            transform c' t1
        else
          let n = get_arg_num f.id_typ in
            if n = List.length xs
            then
              let k = new_var' "k" in
              let f' = f.id_cps in
              let xs' = List.map (fun x -> x.id_cps) xs in
              let t1' = transform (fun y -> App(Var k, [y])) t1 in
              let t2' = transform c t2 in
                Let(flag, f', xs'@[k], t1', t2')
            else
              let xs1,xs2 = take2 xs n in
              let typ_g = app_typ f.id_typ xs1 in
              let g = {id_cps=new_var' f.id_cps.name; id_typ=typ_g} in
              let t1' = {t_cps=LetCPS(Nonrecursive, g, xs2, t1, {t_cps=VarCPS g;typ_cps=typ_g}); typ_cps=typ_g} in
                transform c {t_cps=LetCPS(flag,f,xs1,t1',t2); typ_cps=typ}
    | BinOpCPS(op, t1, t2) ->
        let c1 t1' t2' = c (BinOp(op, t1', t2')) in
        let c2 y1 = transform (fun y2 -> c1 y1 y2) t2 in
          transform c2 t1
    | NotCPS t ->
        let c' t1 = c (Not t1) in
          transform c' t
    | FailCPS -> c (Fail)
    | UnknownCPS -> c Unknown
    | EventCPS s -> c (Event s)
    | RecordCPS(b,[]) -> c (Record(b,[]))
    | RecordCPS(b,fields) ->
        let k = new_var' "k" in
        let r = new_var' "r" in
        let aux t1 s f t2 =
          match t1 with
              Record(b,fields) -> Record(b, fields @ [s,(f,t2)])
            | _ -> assert false
        in
        let c1 x = App(Var k, [x]) in
        let cc = List.fold_right (fun (s,(f,t)) cc -> fun x -> transform (fun y -> cc (aux x s f y)) t) fields c1 in
          funs := k::!funs;
          Let(Nonrecursive, k, [r], c (Var r), transform cc {t_cps=RecordCPS(b,[]);typ_cps=typ})
    | ProjCPS(n,i,s,f,t) ->
        let c' t1 = c (Proj(n,i,s,f,t1)) in
          transform c' t
    | NilCPS -> c Nil
    | ConsCPS(t1,t2) ->
        let c1 t1' t2' = c (Cons(t1', t2')) in
        let c2 y1 = transform (fun y2 -> c1 y1 y2) t2 in
          transform c2 t1
    | ConstrCPS(cstr,[]) -> c (Constr(cstr,[]))
    | ConstrCPS(cstr,ts) ->
        let k = new_var' "k" in
        let r = new_var' "r" in
        let aux t1 t2 =
          match t1 with
              Constr(cstr,ts) -> Constr(cstr, ts @ [t2])
            | _ -> assert false
        in
        let c1 x = App(Var k, [x]) in
        let cc = List.fold_right (fun t cc -> fun x -> transform (fun y -> cc (aux x y)) t) ts c1 in
          funs := k::!funs;
          Let(Nonrecursive, k, [r], c (Var r), transform cc {t_cps=ConstrCPS(cstr,[]);typ_cps=typ})
    | MatchCPS(t1,t2,x,y,t3) ->
        let k = new_var' "k" in
        let r = new_var' "x" in
        let t2' = transform (fun z -> App(Var k, [z])) t2 in
        let t3' = transform (fun z -> App(Var k, [z])) t3 in
        let c' z = Let(Nonrecursive, k, [r], c (Var r), Match(z, t2', x.id_cps, y.id_cps, t3')) in
          funs := k::!funs;
          transform c' t1
    | Match_CPS(t,pats) ->
        let k = new_var' "k" in
        let x = new_var' "x" in
        let aux (pat,cond,t) =
          pattern_of_typed_pat pat, transform (fun y -> App(Var k, [y])) t; assert false
        in
        let pats' = List.map aux pats in
        let c' y = Let(Nonrecursive, k, [x], c (Var x), Match_(y, pats')) in
          funs := k::!funs;
          transform c' t
    | Type_declCPS(decls,t) ->
        Type_decl(decls, transform c t)
    | t -> (Format.printf "%a@." print_t_cps t; assert false)
let transform = transform (fun x -> x)









let rec inlining funs defs = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | RandInt None -> RandInt None
  | RandInt (Some t) -> RandInt (Some (inlining funs defs t))
  | Var x -> Var x
  | Fun _ -> assert false
  | App(Var f, ts) ->
      if List.exists (fun g -> f.id = g.id) funs && List.length (get_args f.typ) = List.length ts
      then
        let xs,t = assoc_id f defs in
          List.fold_right2 subst xs ts t
      else App(Var f, ts)
  | App(Fail, ts) -> App(Fail, ts)
  | App(Event s, ts) -> App(Event s, ts)
  | App _ -> assert false
  | If(t1, t2, t3) ->
      let t2' = inlining funs defs t2 in
      let t3' = inlining funs defs t3 in
        If(t1, t2', t3')
  | Branch(t1, t2) ->
      let t1' = inlining funs defs t1 in
      let t2' = inlining funs defs t2 in
        Branch(t1', t2')
  | Let(flag, f, xs, t1, t2) ->
      if flag = Nonrecursive
      then
        let t1' = inlining funs defs t1 in
        let t2' = inlining funs ((f,(xs,t1'))::defs) t2 in
          Let(flag, f, xs, t1', t2')
      else
        let t1' = inlining funs defs t1 in
        let t2' = inlining funs defs t2 in
          Let(flag, f, xs, t1', t2')
  | BinOp(op, t1, t2) ->
      let t1' = inlining funs defs t1 in
      let t2' = inlining funs defs t2 in
        BinOp(op, t1', t2')
  | Not t ->
      let t' = inlining funs defs t in
        Not t'
  | Fail -> Fail
  | Label _ -> assert false
  | Event s -> Event s
  | Nil -> Nil
  | Cons(t1,t2) -> Cons(inlining funs defs t1, inlining funs defs t2)
  | Match(t1,t2,x,y,t3) ->
      Match(inlining funs defs t1, inlining funs defs t2, x, y, inlining funs defs t3)
  | Constr(c,ts) -> Constr(c, List.map (inlining funs defs) ts)
  | Match_(t,pats) ->
      let aux (pat,cond,t) = pat, apply_opt (inlining funs defs) cond, inlining funs defs t in
        Match_(inlining funs defs t, List.map aux pats)
  | Type_decl(decls,t) -> Type_decl(decls, inlining funs defs t)


let rec normalize = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | RandInt None -> RandInt None
  | RandInt (Some t) -> RandInt (Some (normalize t))
  | Var x -> Var x
  | Fun _ -> assert false
  | App(Fail, [t1;t2]) -> App(Fail, [normalize t1])
  | App(Fail, _) -> assert false
  | App(Event s, [t1;t2]) ->
      let t1' = normalize t1 in
      let t2' = normalize t2 in
        App(Event s, [App(t2', [t1'])])
  | App(Event s, _) -> assert false
  | App(f, ts) ->
      let ts' = List.map normalize ts in
      let f' = normalize f in
        App(f', ts')
  | If(t1, t2, t3) ->
      let t1' = normalize t1 in
      let t2' = normalize t2 in
      let t3' = normalize t3 in
        If(t1', t2', t3')
  | Branch(t1, t2) ->
      let t1' = normalize t1 in
      let t2' = normalize t2 in
        Branch(t1', t2')
  | Let(flag, f, xs, t1, t2) ->
      let t1' = normalize t1 in
      let t2' = normalize t2 in
        Let(flag, f, xs, t1', t2')
(*
  | Let(flag, bindings, t) ->
      let bindings' = List.map (fun (f,xs,t) -> f,xs,normalize t) bindings in
      let t' = normalize t in
        Let(flag, bindings', t')
*)
  | BinOp(op, t1, t2) ->
      let t1' = normalize t1 in
      let t2' = normalize t2 in
        BinOp(op, t1', t2')
  | Not t -> Not (normalize t)
  | Fail -> Fail
  | Label(b,t) -> Label(b,normalize t)
  | Event s -> assert false
  | Record(b,fields) -> Record(b, List.map (fun (s,(f,t)) -> s,(f,normalize t)) fields)
  | Proj(n,i,s,f,t) -> Proj(n, i, s, f, normalize t)
  | Nil -> Nil
  | Cons(t1,t2) -> Cons(normalize t1, normalize t2)
  | Match(t1,t2,x,y,t3) -> Match(normalize t1, normalize t2, x, y, normalize t3)
  | Constr(c,ts) -> Constr(c, List.map normalize ts)
  | Match_(t,pats) ->
      let aux (pat,cond,t) = pat, apply_opt normalize cond, normalize t in
        Match_(normalize t, List.map aux pats)
  | Type_decl(decls,t) -> Type_decl(decls, normalize t)




let rec extract_records env = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | RandInt None -> RandInt None
  | RandInt (Some t) -> RandInt (Some (extract_records env t))
  | Var x -> Var x
  | Fun _ -> assert false
  | App(f, ts) ->
      let rec aux = function
          Var x ->
            begin
              match x.typ with
                  TRecord _ -> List.map (fun x -> Var x) (List.assoc x env)
                | _ -> [Var x]
            end
        | Record(_,fields) -> List.flatten (List.map (fun (_,(_,t)) -> aux t) fields)
        | t -> [extract_records env t]
      in
        App(extract_records env f, List.flatten (List.map aux ts))
  | If(t1, t2, t3) -> If(extract_records env t1, extract_records env t2, extract_records env t3)
  | Branch(t1, t2) -> Branch(extract_records env t1, extract_records env t2)
  | Let(flag, f, xs, t1, t2) ->
      let aux x (xs,env) =
        match x.typ with
            TRecord(_,typs) ->
              let xs' = List.map (fun (s,(f,_)) -> new_var' (x.name^"_"^s)) typs in
                xs'@xs, (x,xs')::env
          | _ -> x::xs, env
      in
      let xs',env' = List.fold_right aux xs ([],env) in
      let t1' = extract_records env' t1 in
      let t2' = extract_records env t2 in
        Let(flag, f, xs', t1', t2')
  | BinOp(op, t1, t2) -> BinOp(op, extract_records env t1, extract_records env t2)
  | Not t -> Not (extract_records env t)
  | Fail -> Fail
  | Label(b,t) -> Label(b,extract_records env t)
  | Event s -> Event s
  | Record(b,fields) -> assert false
  | Proj(_,i,_,_,Var x) -> Var (List.nth (List.assoc x env) i)
  | Proj _ -> assert false
  | Nil -> Nil
  | Cons(t1,t2) -> Cons(extract_records env t1, extract_records env t2)
  | Match(t1,t2,x,y,t3) -> Match(extract_records env t1, extract_records env t2, x, y, extract_records env t3)
  | Constr(c,ts) -> Constr(c, List.map (extract_records env) ts)
  | Match_(t,pats) ->
      let aux (pat,cond,t) = pat, apply_opt (extract_records env) cond, extract_records env t in
        Match_(extract_records env t, List.map aux pats)
  | Type_decl(decls,t) -> Type_decl(decls, extract_records env t)
let extract_records t = extract_records [] t





let rec trans_simpl c t =
  match t with
    Unit -> c Unit
  | True -> c True
  | False -> c False
  | Int n -> c (Int n)
  | NInt x -> c (NInt x)
  | RandInt None ->
      let r = {(new_var' "r") with typ=TInt[]} in
      let k = {(new_var' "k") with typ=TFun((r,r.typ),TUnit)} in
        RandInt (Some (Let(Nonrecursive, k, [r], c (Var r), Var k)))
  | RandInt _ -> assert false
  | Var x -> c (Var x)
  | Fun(x, t) -> assert false
  | App(_, []) -> assert false
  | App(t1, [t2]) ->
      let typ = Typing.get_typ t in
      let r = {(new_var' "r") with typ=typ} in
      let k = {(new_var' "k") with typ=TFun((r,typ),TUnit)} in
      let c1 x = app2app x [Var k] in
      let c2 x = trans_simpl (fun y -> c1 (app2app x [y])) t2 in
        funs := k::!funs;
        Let(Nonrecursive, k, [r], c (Var r), trans_simpl c2 t1)
  | App(t1, t2::ts) -> trans_simpl c (App(App(t1,[t2]), ts))
  | If(t1, t2, t3) ->
      let typ = Typing.get_typ t in
      let x = {(new_var' "x") with typ=typ} in
      let k = {(new_var' "k") with typ=TFun((x,typ),TUnit)} in
      let t2' = trans_simpl (fun y -> App(Var k, [y])) t2 in
      let t3' = trans_simpl (fun y -> App(Var k, [y])) t3 in
      let c' y = Let(Nonrecursive, k, [x], c (Var x), If(y, t2', t3')) in
        funs := k::!funs;
        trans_simpl c' t1
  | Branch(t1, t2) ->
      let typ = Typing.get_typ t in
      let x = {(new_var' "x") with typ=typ} in
      let k = {(new_var' "k") with typ=TFun((x,typ),TUnit)} in
      let t1' = trans_simpl (fun y -> App(Var k, [y])) t1 in
      let t2' = trans_simpl (fun y -> App(Var k, [y])) t2 in
        funs := k::!funs;
        Let(Nonrecursive, k, [x], c (Var x), Branch(t1', t2'))
  | Let(flag, x, [], t1, t2) ->
      let c' t = subst x t (trans_simpl c t2) in
        trans_simpl c' t1
  | Let(flag, f, [x], t1, t2) ->
      let typ = Typing.get_typ t1 in
      let k = {(new_var' "k") with typ=TFun(({dummy_var with typ=typ},typ),TUnit)} in
      let f' = {f with typ=TFun((x,x.typ),TFun((k,k.typ),TUnit))} in
      let t1' = subst f (Var f') (trans_simpl (fun y -> App(Var k, [y])) t1) in
      let t2' = subst f (Var f') (trans_simpl c t2) in
        Let(flag, f', [x;k], t1', t2')
  | Let(flag, f, x::xs, t1, t2) ->
      let typ = match f.typ with TFun(_,typ) -> typ | _ -> assert false in
      let g = {(new_var' f.name) with typ=typ} in
      let t1' = Let(Nonrecursive, g, xs, t1, Var g) in
        trans_simpl c (Let(flag,f,[x],t1',t2))
  | BinOp(op, t1, t2) ->
      let c1 t1' t2' = c (BinOp(op, t1', t2')) in
      let c2 y1 = trans_simpl (fun y2 -> c1 y1 y2) t2 in
        trans_simpl c2 t1
  | Not t ->
      let c' t1 = c (Not t1) in
        trans_simpl c' t
  | Fail -> c (Fail)
  | Unknown -> c Unknown
  | Event s -> c (Event s)
  | Record(b,[]) -> c (Record(b,[]))
  | Record(b,fields) ->
      let typ = Typing.get_typ t in
      let r = {(new_var' "r") with typ=typ} in
      let k = {(new_var' "k") with typ=TFun((r,typ),TUnit)} in
      let aux t1 s f t2 =
        match t1 with
            Record(b,fields) -> Record(b, fields @ [s,(f,t2)])
          | _ -> assert false
      in
      let c1 x = App(Var k, [x]) in
      let cc = List.fold_right (fun (s,(f,t)) cc -> fun x -> trans_simpl (fun y -> cc (aux x s f y)) t) fields c1 in
        funs := k::!funs;
        Let(Nonrecursive, k, [r], c (Var r), trans_simpl cc (Record(b,[])))
  | Proj(n,i,s,f,t) ->
      let c' t1 = c (Proj(n,i,s,f,t1)) in
        trans_simpl c' t
  | Nil -> c Nil
  | Cons(t1,t2) ->
      let c1 t1' t2' = c (Cons(t1', t2')) in
      let c2 y1 = trans_simpl (fun y2 -> c1 y1 y2) t2 in
        trans_simpl c2 t1
  | Constr(cstr,[]) -> c (Constr(cstr,[]))
  | Constr(cstr,ts) ->
      let typ = Typing.get_typ t in
      let r = {(new_var' "r") with typ=typ} in
      let k = {(new_var' "k") with typ=TFun((r,typ),TUnit)} in
      let aux t1 t2 =
        match t1 with
            Constr(cstr,ts) -> Constr(cstr, ts @ [t2])
          | _ -> assert false
      in
      let c1 x = App(Var k, [x]) in
      let cc = List.fold_right (fun t cc -> fun x -> trans_simpl (fun y -> cc (aux x y)) t) ts c1 in
        funs := k::!funs;
        Let(Nonrecursive, k, [r], c (Var r), trans_simpl cc (Constr(cstr,[])))
  | Match(t1,t2,x,y,t3) ->
      let k = new_var' "k" in
      let r = new_var' "x" in
      let t2' = trans_simpl (fun z -> App(Var k, [z])) t2 in
      let t3' = trans_simpl (fun z -> App(Var k, [z])) t3 in
      let c' z = Let(Nonrecursive, k, [r], c (Var r), Match(z, t2', x, y, t3')) in
        funs := k::!funs;
        trans_simpl c' t1
  | Match_(t,pats) ->
      let k = new_var' "k" in
      let x = new_var' "x" in
      let aux (pat,cond,t) =
        pat, trans_simpl (fun y -> App(Var k, [y])) t; assert false
      in
      let pats' = List.map aux pats in
      let c' y = Let(Nonrecursive, k, [x], c (Var x), Match_(y, pats')) in
        funs := k::!funs;
        trans_simpl c' t
  | Type_decl(decls,t) ->
      Type_decl(decls, trans_simpl c t)
  | t -> (Format.printf "%a@." pp_print_term t; assert false)
let trans_simpl = trans_simpl (fun x -> x)



(*
let trans t =
  let cps_pre = infer_cont_pos [] t in
  let () = if true then Format.printf "CPS_infer_cont_pos:@.%a@." print_t_cps cps_pre.t_cps in
  let cps = transform cps_pre in
  let () = if true then Format.printf "CPS:@.%a@." pp_print_term cps in
  let typed = Typing.typing cps in
  let extracted = extract_records typed in
  let () = if false then Format.printf "EXTRACTED:@.%a@." pp_print_term extracted in
  let normalized = normalize extracted in
  let typed = Typing.typing normalized in
  let inlined = inlining !funs [] typed in
(*
  let removed = remove_unused inlined in
*)
    part_eval inlined
*)

let trans t =
  let cps = trans_simpl t in
  let () = if true then Format.printf "CPS:@.%a@." pp_print_term cps in
  let typed = Typing.typing cps in
  let () = if true then Format.printf "Typed CPS:@.%a@." pp_print_term typed in
  let extracted = extract_records typed in
  let () = if false then Format.printf "EXTRACTED:@.%a@." pp_print_term extracted in
  let normalized = normalize extracted in
  let typed = Typing.typing normalized in
  let inlined = inlining !funs [] typed in
    part_eval inlined


