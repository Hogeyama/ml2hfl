
open Util
open Syntax
open Type


module PredSet =
  Set.Make(
    struct
      type t = Syntax.typed_term * Syntax.typed_term
      let compare = compare
    end)
module PredSetSet =
  Set.Make(
    struct
      type t = PredSet.t
      let compare = PredSet.compare
    end)



let hd = function
    [x] -> x
  | _ -> assert false



let get_preds = function
    TInt ps -> ps
  | TList(_,ps) -> ps
  | _ -> assert false



let rec abstract_mutable t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | NInt x -> NInt x
      | RandInt None -> RandInt None
      | RandInt (Some t) -> RandInt (Some (abstract_mutable t))
      | Var x -> Var x
      | Fun(x, t) -> Fun(x, abstract_mutable t)
      | App(t, ts) -> App(abstract_mutable t, List.map abstract_mutable ts)
      | If(t1, t2, t3) -> If(abstract_mutable t1, abstract_mutable t2, abstract_mutable t3)
      | Branch(t1, t2) -> Branch(abstract_mutable t1, abstract_mutable t2)
      | Let(flag, f, xs, t1, t2) -> Let(flag, f, xs, abstract_mutable t1, abstract_mutable t2)
      | BinOp(op, t1, t2) -> BinOp(op, abstract_mutable t1, abstract_mutable t2)
      | Not t -> Not (abstract_mutable t)
      | Fail -> Fail
      | Label(b, t) -> Label(b, abstract_mutable t)
      | Event s -> Event s
      | Record(b,fields) -> Record(b, List.map (fun (f,(s,t)) -> f,(s,abstract_mutable t)) fields)
      | Proj(n,i,s,Flag.Immutable,t) -> Proj(n, i, s, Flag.Immutable, abstract_mutable t)
      | Proj(n,i,s,Flag.Mutable,t) ->
          let u = Id.new_var "u" t.typ in
            Let(Flag.Nonrecursive, u, [], abstract_mutable t, {desc=RandInt None;typ=TInt[]})
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(abstract_mutable t1, abstract_mutable t2)
      | Constr(s,ts) -> Constr(s, List.map abstract_mutable ts)
      | Match(t1,t2,x,y,t3) -> Match(abstract_mutable t1, abstract_mutable t2, x, y, abstract_mutable t3)
      | Match_(t,pats) ->
          let aux (pat,cond,t) = pat,apply_opt abstract_mutable cond, abstract_mutable t in
            Match_(abstract_mutable t, List.map aux pats)
  in
    {desc=desc; typ=t.typ}


let rec get_abst_val _ = assert false (*function
    TUnit -> unit_term
  | TBool -> {desc=BinOp(Eq, {desc=Int 0;typ=TInt[]}, {desc=RandInt None;typ=TInt[]});typ=TBool}
  | TInt _ -> {desc=RandInt None;typ=TInt[]}
  | TFun(x,typ2) as typ ->
      let typs = List.map Id.typ (get_args (Id.typ x)) in
      let ts = List.map get_abst_val typs in
      let x' = Id.new_var_id x in
      let f = Id.new_var "f" typ in
      let u = Id.new_var "u" TUnit in
      let y = Id.new_var "y" typ2 in
      let t1 = {desc=Let(Flag.Nonrecursive, u, [], app2app (make_var x') ts, make_var y);typ=typ} in
      let t2 = make_var y in
      let t = {desc=Let(Flag.Nonrecursive, y, [], get_abst_val typ2, {desc=Branch(t1, t2); typ=typ}); typ=typ} in
        {desc=Let(Flag.Nonrecursive, f, [x'], t, make_var f); typ=typ}
  | TList(typ,_) -> assert false (*
      let u = Id.new_var "u" TUnit in
      let f = Id.new_var "f" (TFun(u,typ)) in
      let t = If(get_abst_val TBool, {desc=Nil;typ=TList, Cons(get_abst_val typ, App(make_var f, [Unit]))) in
        Let(Recursive, f, [u], t, App(make_var f, [Unit]))*)
  | TRecord(b,typs) ->
      let fields = List.map (fun (s,(f,typ)) -> s,(f,get_abst_val typ)) typs in
        Record(b,fields)
  | TVariant _ as typ ->
      let stypss = Typing.get_constrs_from_type typ in
      let aux (s,typs) = Constr(s, List.map get_abst_val typs) in
        List.fold_left (fun t styps -> If(Unknown, t, aux styps)) (aux (List.hd stypss)) (List.tl stypss)
  | TVar x -> assert false
  | TConstr(s,true) -> assert false
  | TConstr(s,false) -> RandValue(TConstr(s,false), None)
  | TUnknown -> Unit
  | typ -> print_typ Format.std_formatter typ; assert false
*)
let rec abst_ext_funs t =
  let desc =
    match t.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int n -> Int n
      | Var x ->
          if is_external x
          then
            let x' = Id.new_var_id x in
              Let(Flag.Nonrecursive, x', [], get_abst_val (Id.typ x), make_var x')
          else Var x
      | NInt x -> NInt x
      | RandInt None -> RandInt None
      | RandInt (Some t) -> RandInt (Some (abst_ext_funs t))
      | RandValue(typ,None) -> RandValue(typ,None)
      | RandValue(typ,Some t) -> RandValue(typ,Some (abst_ext_funs t))
      | Fun(x,t) -> Fun(x, abst_ext_funs t)
      | App(t, ts) -> App(abst_ext_funs t, List.map abst_ext_funs ts)
      | If(t1, t2, t3) -> If(abst_ext_funs t1, abst_ext_funs t2, abst_ext_funs t3)
      | Branch(t1, t2) -> Branch(abst_ext_funs t1, abst_ext_funs t2)
      | Let(flag, f, xs, t1, t2) -> Let(flag, f, xs, abst_ext_funs t1, abst_ext_funs t2)
      | BinOp(op, t1, t2) -> BinOp(op, abst_ext_funs t1, abst_ext_funs t2)
      | Not t -> Not (abst_ext_funs t)
      | Fail -> Fail
      | Label(b, t) -> Label(b, abst_ext_funs t)
      | Event s -> Event s
      | Record(b,fields) -> Record(b, List.map (fun (f,(s,t)) -> f,(s,abst_ext_funs t)) fields)
      | Proj(n,i,s,f,t) -> Proj(n,i,s,f,abst_ext_funs t)
      | SetField(n,i,s,f,t1,t2) -> SetField(n,i,s,f,abst_ext_funs t1,abst_ext_funs t2)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(abst_ext_funs t1, abst_ext_funs t2)
      | Constr(s,ts) -> Constr(s, List.map abst_ext_funs ts)
      | Match(t1,t2,x,y,t3) -> Match(abst_ext_funs t1, abst_ext_funs t2, x, y, abst_ext_funs t3)
      | Match_(t,pats) ->
          let aux (pat,cond,t) = pat, apply_opt abst_ext_funs cond, abst_ext_funs t in
            Match_(abst_ext_funs t, List.map aux pats)
      | TryWith(t,pats) ->
          let aux (pat,cond,t) = pat, apply_opt abst_ext_funs cond, abst_ext_funs t in
            TryWith(abst_ext_funs t, List.map aux pats)
  in
    {desc=desc; typ=t.typ}













open CEGAR_const
open CEGAR_syntax
open CEGAR_type



let check env cond pbs p =
  let ps,_ = List.split pbs in
    Wrapper.check env (cond@@ps) p

let make_conj pbs =
  match pbs with
      [] -> Const True
    | (_,b)::pbs -> List.fold_left (fun t (_,b) -> make_and t b) b pbs

let make_dnf pbss =
  match pbss with
      [] -> Const False
    | pbs::pbss' -> List.fold_left (fun t pbs -> make_or t (make_conj pbs)) (make_conj pbs) pbss'


let mapi f xs =
  let rec aux i xs =
    match xs with
        [] -> []
      | x::xs' -> (f i x) ::(aux (i + 1) xs')
  in
    aux 1 xs


let weakest env (cond:CEGAR_syntax.t list) ds p =
  let fvp = get_fv p in
  let ts = ((cond @@ List.map fst ds):CEGAR_syntax.t list) in
  let ds =
    let rec fixp xs =
      let xs' =
        Util.uniq compare
          (xs @
             (List.flatten
                (List.map
                   (fun p ->
                      let fv = get_fv p in
                        if Util.inter fv xs = []
                        then []
                        else fv)
                   ts)))
      in
        if List.length xs = List.length xs'
        then xs
        else fixp xs'
    in
    let fv = fixp fvp in
      List.filter (fun (p, _) -> subset (get_fv p) fv) ds
  in
  let nds = List.map (fun (p, b) -> make_not p, make_not b) ds in

  let f pbs =
    List.map
      (fun i ->
         if i > 0 then
           List.nth ds (i - 1)
         else
           List.nth nds (-i - 1))
      pbs
  in
  let pbss =
    if Flag.use_neg_pred
    then mapi (fun i _ -> [i]) ds @ mapi (fun i _ -> [-i]) ds
    else mapi (fun i _ -> [i]) ds
  in
  let rec loop xs' nxs' ys' pbss =
    let xs, qs = List.partition
      (fun pbs ->
         let pbs = f pbs in
         let fvs =
           List.flatten
             (List.map
                (fun (p, _) ->
                   get_fv p)
                pbs)
         in
           if Util.inter fvp fvs = [] && Util.inter (Util.rev_map_flatten get_fv cond) fvs = [] then
             false
           else
             check env cond pbs p)
      pbss
    in
    let nxs, ys = List.partition
      (fun pbs ->
         let pbs = f pbs in
         let fvs =
           List.flatten
             (List.map
                (fun (p, _) ->
                   get_fv p)
                pbs)
         in
           if Util.inter fvp fvs = [] && Util.inter (Util.rev_map_flatten get_fv cond) fvs = [] then
             false
           else
             check env cond pbs (make_not p))
      pbss
    in
    let xs = Util.uniq compare (xs' @ xs) in
    let nxs = Util.uniq compare (nxs' @ nxs) in
    let ys = Util.uniq compare (ys' @ ys) in
    let ws = 
      Util.uniq compare
        (List.flatten
           (List.map
              (fun y1 ->
                 List.map
                   (fun y2 ->
                      List.sort compare
                        (Util.uniq compare (y1 @ y2)))
                   ys)
              ys))
    in
    let ws =
      let ok w =
        if List.length w > !Flag.wp_max_num then
          false
        else
          let rec ok w =
            match w with
                [] -> true
              | a::b -> not (List.mem (-a) b) && ok b
          in
            ok w
      in
        List.filter ok ws
    in
    let ws =
      if false then
        Util.diff ws ys
      else
        List.filter (fun w -> not (List.exists (fun x -> Util.diff w x = []) ys)) ws
    in
    let ws = List.filter
      (fun w -> not (List.exists (fun x -> Util.diff x w = []) xs) &&
         not (List.exists (fun x -> Util.diff x w = []) nxs)) ws in
    let ws =
      let rec aux xs =
        match xs with
            [] -> []
          | x::xs' ->
              if List.exists (fun y -> Util.diff y x = []) xs' then aux xs' else x::(aux xs')
      in
        aux ws
    in
      if ws = [] then xs, nxs else loop xs nxs ys ws
  in
  let xs, nxs = loop [] [] [] pbss in
  let pbss = List.map f xs in
  let pbss = List.filter (fun pbs -> not (check env cond pbs (Const False))) pbss in
  let npbss = List.map f nxs in
  let npbss = List.filter (fun pbs -> not (check env cond pbs (Const False))) npbss in
    make_dnf pbss, make_dnf npbss



let abst env cond pbs p =
  let tt, ff = weakest env cond pbs p in
    if tt = make_not (Const False) || make_not (Const True) = ff
    then Const True
    else make_temp_if tt (Const True) (make_temp_if ff (Const False) (App(Const RandBool, Const Unit)))



let assume env cond pbs t =
  let _,ff = weakest env cond pbs t in
  let f = new_id "f" in
  let x = new_id "b" in
  let xs = get_fv t in
  let def1 = f, x::xs, Var x, loop_term in
  let def2 = f, x::xs, make_not (Var x), t in
    [def1;def2], App(List.fold_left (fun t x -> App(t,Var x)) (Var f) xs, ff)

    


let abst_arg x typ =
  let ps =
    match typ with
        TBase(_,ps) -> ps (Var x)
      | _ -> []
  in
    Util.mapi (fun i p -> p, App(Const (Proj i), Var x)) ps

let rec coerce env cond pts typ1 typ2 =
  match typ1,typ2 with
      TBase(_,ps1),TBase(_,ps2) ->
        let x = new_id "x" in
        let env' = (x,typ1)::env in
        let pts' = abst_arg x typ1 @@ pts in
        let ts = List.map (abst env' cond pts') (ps2 (Var x)) in
        let t = List.fold_left (fun t1 t2 -> App(t1,t2)) (Const (Tuple (List.length ts))) ts in
          make_fun [x] t
    | TFun typ1, TFun typ2 ->
        let f = new_id "f" in
        let x = new_id "x" in
        let typ11,typ12 = typ1 (Var x) in
        let typ21,typ22 = typ2 (Var x) in
        let env' = (x,typ11)::env in
        let defs1,f1 = coerce env' cond pts typ12 typ22 in
        let defs2,f2 = coerce env' cond pts typ21 typ11 in
        let defs,f' = make_fun [f;x] (App(Var f1, App(Var f, App(Var f2, Var x)))) in
          defs@@defs1@@defs2, f'
    | TBase(TBottom,_), _ ->
        let x = new_id "x" in
          make_fun [x] (Var x)
    | _ -> assert false


let rec abstract_term env cond pbs t typ =
  match t with
      Const c ->
        let defs,f = coerce env cond pbs (get_const_typ c) typ in
          defs, App(Var f, t)
    | Var x ->
        let defs,f = coerce env cond pbs (List.assoc x env) typ in
          defs, App(Var f, t)
    | App(t1, t2) ->
        let typ' = get_typ env t1 in
          Format.printf "%a: %a@." CEGAR_print.print_term t CEGAR_print.print_typ typ';
        let typ1,typ2 =
          match typ' with
              TFun typ -> typ t2
            | TBase(TBottom,_) -> TBase(TBottom,fun _ -> []), TBase(TBottom,fun _ -> [])
            | _ -> assert false
        in
        let defs1,t1' = abstract_term env cond pbs t1 typ' in
        let defs2,t2' = abstract_term env cond pbs t2 typ1 in
          Format.printf "COERCE: %a  ===>  %a@." CEGAR_print.print_typ typ2 CEGAR_print.print_typ typ;
        let defs,f = coerce env cond pbs typ2 typ in
          defs@@defs1@@defs2, App(Var f, App(t1',t2'))
    | Let(x,t1,t2) ->
        let typ' = get_typ env t1 in
        let defs1,t1' = abstract_term env cond pbs t1 typ' in
        let env' = (x,typ')::env in
        let pbs' = abst_arg x typ' in
        let defs2,t2' = abstract_term env' cond pbs' t2 typ in
          defs1@@defs2, Let(x,t1',t2')


let abstract_def env (f,xs,t1,t2) =
  let rec aux typ xs env =
    match xs with
        [] -> typ, env
      | x::xs' ->
          let typ1,typ2 =
            match typ with
                TFun typ -> typ (Var x)
              | _ -> assert false
          in
          let env' = (x,typ1)::env in
            aux typ2 xs' env'
  in
  let typ,env' = aux (List.assoc f env) xs env in
  let pbs = rev_flatten_map (fun (x,typ) -> abst_arg x typ) env' in
  let defs1,t1' = assume env' [] pbs t1 in
  let defs2,t2' = abstract_term env' [t1] pbs t2 typ in
    (f, xs, t1', t2')::defs1@@defs2




let rec make_arg_let = function
    Const c -> Const c
  | Var x -> Var x
  | App(t1,t2) ->
      let t1' = make_arg_let t1 in
      let t2' = make_arg_let t2 in
      let x = new_id "a" in
        Let(x,t2',App(t1',Var x))
  | Let _ -> assert false
let make_arg_let defs = List.map (apply_body_def make_arg_let) defs



let abstract (env,defs,main) =
  let defs' = make_arg_let defs in
  let defs'' = rev_flatten_map (abstract_def env) defs' in
(*
  let defs = extract_temp_if defs in
*)
    env, defs'', main
