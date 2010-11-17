
open Util
open Syntax



module PredSet =
  Set.Make(
    struct
      type t = Syntax.t * Syntax.t
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









(* ??? *)
let add_to_cond t cond =
  let fv = get_fv t in
    if List.exists (fun x -> x.typ = TBool) fv
    then cond
    else t::cond





let get_preds = function
    TInt ps -> ps
  | _ -> assert false



let make_conj pbs =
  match pbs with
      [] -> True
    | (_,b)::pbs -> List.fold_left (fun t (_,b) -> BinOp(And, t, b)) b pbs

let make_dnf pbss =
  match pbss with
      [] -> False
    | pbs::pbss' -> List.fold_left (fun t pbs -> BinOp(Or, t, make_conj pbs)) (make_conj pbs) pbss'

let check cond pbs p =
  let ps,_ = List.split pbs in

(*
print_string "\n";
List.iter (Syntax.print_term_break Syntax.ML false) (cond@@ps);
print_string "=>";
Syntax.print_term_break Syntax.ML false p;
*)
    let b = Wrapper.check (cond@@ps) p in
(*
if b then print_string "True" else print_string "False";
print_string "\n";
*)
    b


let mapi f xs =
  let rec aux i xs =
    match xs with
        [] -> []
      | x::xs' -> (f i x) ::(aux (i + 1) xs')
  in
    aux 1 xs

let weakest cond ds p =
(*
print_string "\n";
List.iter (Syntax.print_term_break Syntax.ML false) (cond);
print_string ":";
List.iter (fun (p , b) -> Syntax.print_term_break Syntax.ML false p) (ds);
print_string "=>";
Syntax.print_term_break Syntax.ML false p;
*)
  (*This breaks progress mc91.ml if n <= 100 ...
    let nds = List.map (fun (p, b) -> Not p, Not b) ds in
    try
      let b = List.assoc p ds in
        b, Not b
    with _ ->
      try
        match List.assoc p nds with
            Not b -> Not b, b
          | _ -> assert false
      with _ ->*)
        if check cond [] p then (*???*)
          True, False
        else if check cond [] (Not p) then (*???*)
          False, True
        else
          let ss ids = List.map (fun id -> id.id) ids in
          let fvp = ss (Util.uniq (get_fv2(*???*) p)) in
          let ts = cond @@ List.map fst ds in
          let ds =
            let rec fixp xs =
              let xs' =
                Util.uniq
                  (xs @
                     (List.flatten
                        (List.map
                           (fun p ->
                              let fv = ss (get_fv2(*???*) p) in
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
              List.filter (fun (p, _) -> subset (ss (get_fv2(*???*) p)) fv) ds
          in
          let nds = List.map (fun (p, b) -> Not p, Not b) ds in

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
                           ss (get_fv2(*???*) p))
                        pbs)
                 in
                   if Util.inter fvp fvs = [] && Util.inter (Util.rev_map_flatten (fun t -> ss (get_fv2 t)) cond) fvs = [] then
                     false
                   else
                     check cond pbs p)
              pbss
            in
            let nxs, ys = List.partition
              (fun pbs ->
                 let pbs = f pbs in
                 let fvs =
                   List.flatten
                     (List.map
                        (fun (p, _) ->
                           ss (get_fv2(*???*) p))
                        pbs)
                 in
                   if Util.inter fvp fvs = [] && Util.inter (Util.rev_map_flatten (fun t -> ss (get_fv2 t)) cond) fvs = [] then
                     false
                   else
                     check cond pbs (Not p))
              pbss
            in
            let xs = Util.uniq (xs' @ xs) in
            let nxs = Util.uniq (nxs' @ nxs) in
            let ys = Util.uniq (ys' @ ys) in
            let ws = 
              Util.uniq
                (List.flatten
                   (List.map
                      (fun y1 ->
                         List.map
                           (fun y2 ->
                              List.sort compare
                                (Util.uniq (y1 @ y2)))
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
          let pbss = List.filter (fun pbs -> not (check cond pbs False)) pbss in
          let npbss = List.map f nxs in
          let npbss = List.filter (fun pbs -> not (check cond pbs False)) npbss in
            make_dnf pbss, make_dnf npbss



let rec contradict_aux cond pbsetset1 pbsetset2 p =
  if PredSetSet.is_empty pbsetset1
  then
    pbsetset2
  else
    let checkset pbset =
      let ps = List.map fst (PredSet.elements pbset) in
        Wrapper.check (cond @@ ps) p
    in
    let aux pbset =
      let f p pbsetset =
        let pbset' = PredSet.remove p pbset in
          if checkset pbset'
          then PredSetSet.add pbset' pbsetset
          else pbsetset
      in
        PredSet.fold f pbset PredSetSet.empty
    in
    let aux2 pbset (pbsetset1,pbsetset2) =
      let pbsetset = aux pbset in
        if PredSetSet.is_empty pbsetset
        then pbsetset1, PredSetSet.add pbset pbsetset2
        else PredSetSet.union pbsetset pbsetset1, pbsetset2
    in
    let pbsetset1', pbsetset2' = PredSetSet.fold aux2 pbsetset1 (PredSetSet.empty,pbsetset2) in
      contradict_aux cond pbsetset1' pbsetset2' p
let contradict cond pbs =
  let p = False in
  let conj pbset =
    let pbs = PredSet.elements pbset in
      match pbs with
        [] -> True
      | (_,b)::pbs ->
          List.fold_left (fun t (_,b) -> BinOp(And, t, b)) b pbs
  in
  let make_dnf pbsetset =
    let pbsets = PredSetSet.elements pbsetset in
      match pbsets with
        [] -> False
      | pbset::pbsets' -> List.fold_left (fun t pbset -> BinOp(Or, t, conj pbset)) (conj pbset) pbsets'
  in
  let f pbset pb = PredSet.add pb pbset in
  let pbset = List.fold_left f PredSet.empty pbs in
  let pbsetset = PredSetSet.singleton pbset in
  let ps,_ = List.split pbs in
    if Wrapper.check (cond@@ps) p
    then
      let pbsetset' = contradict_aux cond pbsetset PredSetSet.empty p in
        make_dnf pbsetset'
    else False






let abst cond pbs p =
  let tru, fls = weakest cond pbs p in
    if tru = Not fls || Not tru = fls then tru
    else If(tru, True, If(fls, False, Unknown, Unknown), Unknown)







let inst_var t p =
  subst abst_var t p









let abst_arg x (xs,pbs) =
  match x.typ with
      TInt ps ->
        let xs' = List.map (fun _ -> {(new_var' "b") with typ=TAbsBool}) ps in
        let pbs' = List.map2 (fun p b -> inst_var (Var x) p, Var b) ps xs' in
          xs'@xs, pbs'@pbs
    | _ -> x::xs, pbs



let filter_fail cond pbs t =
  let p = contradict cond pbs in
  let bot = new_var' "bot" in
    If(p, Letrec(bot, [], Var bot, Var bot), t, Unit)




let rec coerce cond pbs typ1 typ2 t =
  match typ1,typ2 with
      TUnit,TUnit -> filter_fail cond pbs t
    | TBool, TBool -> t
    | TFun((x1,TInt ps1),typ12), TFun((x2,TInt ps2),typ22) ->
        let xs,pbs' = abst_arg x2 ([],pbs) in
        let cond' = BinOp(Eq, Var x1, Var x2)::cond in
        let ts = List.map (fun p -> abst cond' pbs' (inst_var (Var x2) p)) ps1 in
        let t' = coerce cond' pbs' typ12 typ22 (App(t, ts)) in
          List.fold_right (fun x t -> Fun(x, t)) xs t'

    | TFun((x1,TRInt p),typ12), TFun((x2,TInt ps2),typ22) ->
        let xs,pbs' = abst_arg x2 ([],pbs) in
        let cond' = BinOp(Eq, Var x1, Var x2)::cond in
        let t' = coerce cond' pbs' typ12 typ22 t in
   					let f = new_var' "f" in
   					let x = new_var' "x" in
								let tru,_ = weakest cond' pbs' (inst_var (Var x2) p) in
        let t'' = If(tru, t', App(Fail, [Unit; Let(f, [x], Var x, Var f)]), Unit) in
          List.fold_right (fun x t -> Fun(x, t)) xs t''

    | TFun((x1,TInt ps1),typ12), TFun((x2,TRInt p),typ22) ->
        let cond' = (inst_var (Var x2) p)::BinOp(Eq, Var x1, Var x2)::cond in
        let ts = List.map (fun p -> abst cond' pbs (inst_var (Var x2) p)) ps1 in
        coerce cond' pbs typ12 typ22 (App(t, ts))

    | TFun((x1,typ11),typ12), TFun((x2,typ21),typ22) ->
        let x = new_var' "x" in
(*x cannot be depended because x is function? *)
          Fun(x, coerce cond pbs typ12 typ22 (App(t, [coerce cond pbs typ21 typ11 (Var x)])))
    | TUnknown,_ -> t
    | _,TUnknown -> t
    | _,_ -> Format.printf "coerce:%a,%a@." (Syntax.print_typ Syntax.ML) typ1 (Syntax.print_typ Syntax.ML) typ2; assert false

let rec abstract cond pbs = function
    Unit, TUnit -> [Unit]
  | True, TBool -> [True]
  | False, TBool -> [False]
  | Unknown, TBool -> [Unknown]
  | t, (TRInt p) -> []
  | t, (TInt _ as typ) ->
(*
let ps = List.map (fun p -> inst_var t p) (get_preds typ) in
print_string "\n";
List.iter (Syntax.print_term_break Syntax.ML false) ps;
Syntax.print_term_break Syntax.ML false t;
List.iter (Syntax.print_term_break Syntax.ML false) cond;
List.iter (fun (p, t) -> Syntax.print_term_break Syntax.ML false p) pbs;
*)
      let at = List.map (fun p -> abst cond pbs (inst_var t p)) (get_preds typ) in
(*List.iter (Syntax.print_term_break Syntax.ML false) at;*)
      at
  | Var x, typ when Wrapper.congruent cond x.typ typ -> [Var x]
  | Var x, typ -> abstract cond pbs (App(Var x, []), typ)
  | Fun _, _ -> assert false
  | App(Let(f, xs, t1, t2), ts), typ ->
      abstract cond pbs (Let(f, xs, t1, App(t2, ts)), typ)
  | App(App(t, ts1), ts2), typ ->
      abstract cond pbs (App(t, ts1 @ ts2), typ)
  | App(Fail, _), _ -> [App(Fail, [Unit])]
  | App(Event s, [t1]), _ ->
      let t1' = abstract cond pbs (t1,TUnit) in
        [App(Event s, t1')]
  | App(Var f, ts), typ ->
      let ttyps,typ' =
        let rec aux = function
            [], typ -> [], typ
          | t::ts, TFun((x,typ1),typ2) ->
              let typ2' = subst_type x t typ2 in
              let ttyps,typ = aux (ts, typ2') in
                (t,typ1)::ttyps, typ
          | ts, typ -> Format.printf "Abstract.abstract:@.%n:%a:%a@." (List.length ts) (Syntax.print_term_fm Syntax.ML false) (Var f) (print_typ ML) typ; assert false
        in
          aux (ts, f.typ)
      in
      let ps =
		      Util.rev_flatten_map
		        (fun (t, typ) ->
		          match typ with
		            TRInt(p) -> [inst_var t p]
		          | _ -> [])
		        ttyps
      in
      let ts' =
        let rec aux = function
            [] -> []
          | (t,typ)::ttyps ->
              let t' = abstract cond pbs (t,typ) in
                t' @ aux ttyps
        in
          aux ttyps
      in
   					let f' = new_var' "f" in
   					let x = new_var' "x" in
								let tru,_ = weakest cond pbs (and_list ps) in
        [If(tru, coerce cond pbs typ' typ (App(Var f, ts')), App(Fail, [Unit; Let(f', [x], Var x, Var f')]), Unit)]
  | BinOp(op, t1, t2) as t, typ ->
      begin
        match op with
            Eq
          | Lt
          | Gt
          | Leq
          | Geq
          | And
          | Or ->
              [abst cond pbs t]
          | Add
          | Sub
          | Mult -> assert false
      end
  | If(t1, t2, t3, _), typ ->
      let x1 = new_var' "x" in
      let x2 = new_var' "x" in
      let cond2 = add_to_cond t1 cond in
      let cond3 = add_to_cond (Not t1) cond in
      let t1' = hd (abstract cond pbs (t1,TBool)) in
      let t2' = hd (abstract cond2 pbs (t2,typ)) in
      let t3' = hd (abstract cond3 pbs (t3,typ)) in
      let t2'' = Label(true, Var x1) in
      let t3'' = Label(false, Var x2) in
        [Let(x1,[],t2',Let(x2,[],t3',If(t1', t2'', t3'', Unit)))]
  | Branch(t1, t2), typ ->
      let t1' = hd (abstract cond pbs (t1,typ)) in
      let t2' = hd (abstract cond pbs (t2,typ)) in
        [Branch(t1', t2')]
  | Let(f, xs, t1, t2), typ ->
      let xs',pbs' = List.fold_right abst_arg xs ([],pbs) in
      let () = assert (List.length (get_args f.typ) = List.length xs) in
      let t1' = filter_fail cond pbs' (hd (abstract cond pbs' (t1,TUnit))) in
      let t2' = hd (abstract cond pbs (t2,typ)) in
        [Let(f, xs', t1', t2')]
  | Letrec(f, xs, t1, t2), typ ->
      let xs',pbs' = List.fold_right abst_arg xs ([],pbs) in
      let () = assert (List.length (get_args f.typ) = List.length xs) in
      let t1' = filter_fail cond pbs' (hd (abstract cond pbs' (t1,TUnit))) in
      let t2' = hd (abstract cond pbs (t2,typ)) in
        [Letrec(f, xs', t1', t2')]
  | Not t, _ ->
      let t' = hd (abstract cond pbs (t,TBool)) in
        [Not t']
  | Fail, _ -> [Fail]
  | Event s, _ -> [Event s]
  | App(t, []), typ -> abstract cond pbs (t, typ)
  | t, typ -> (Format.printf "Abstract.abstract:@.%a:%a@." (Syntax.print_term_fm Syntax.ML false) t (Syntax.print_typ Syntax.ML) typ; assert false)





let rec eager_unknown = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | Var x -> Var x
  | Fun(x, t) ->
      let t' = eager_unknown t in
        Fun(x, t')
  | App(t, ts) ->
      let t' = eager_unknown t in
      let ts' = List.map eager_unknown ts in
        App(t', ts')
  | If(t1, t2, t3, t4) ->
      let t1' = eager_unknown t1 in
      let t2' = eager_unknown t2 in
      let t3' = eager_unknown t3 in
      let t4' = eager_unknown t4 in
        If(t1', t2', t3', t4')
  | Branch(t1, t2) ->
      let t1' = eager_unknown t1 in
      let t2' = eager_unknown t2 in
        Branch(t1', t2')
  | Let(f, xs, t1, t2) ->
      let t1' = eager_unknown t1 in
      let t2' = eager_unknown t2 in
      let ys = get_fv t1 in
      let check_mult x =
        if x.typ = TAbsBool
        then
          let n = List.fold_left (fun n y -> if x.id=y.id then n+1 else n) 0 ys in
            n >= 2
        else false
      in
      let xs' = List.filter check_mult xs in
      let aux x t =
        let y = new_var' "be" in
        let g = new_var' "fe" in
          Let(g, [y], subst x (Var y) t, Branch(App(Var g, [True]), App(Var g, [False])))
      in
      let t1'' = List.fold_right aux xs' t1' in
        Let(f, xs, t1'', t2')
  | Letrec(f, xs, t1, t2) ->
      let t1' = eager_unknown t1 in
      let t2' = eager_unknown t2 in
      let ys = get_fv t1 in
      let check_mult x =
        if x.typ = TAbsBool
        then
          let n = List.fold_left (fun n y -> if x.id=y.id then n+1 else n) 0 ys in
            n >= 2
        else false
      in
      let xs' = List.filter check_mult xs in
      let aux x t =
        let y = new_var' "b" in
        let g = new_var' "f" in
          Let(g, [y], subst x (Var y) t, Branch(App(Var g, [True]), App(Var g, [False])))
      in
      let t1'' = List.fold_right aux xs' t1' in
        Letrec(f, xs, t1'', t2')
  | BinOp(op, t1, t2) ->
      let t1' = eager_unknown t1 in
      let t2' = eager_unknown t2 in
        BinOp(op, t1', t2')
  | Not t ->
      let t' = eager_unknown t in
        Not t'
  | Fail -> Fail
  | Label(b, t) ->
      let t' = eager_unknown t in
        Label(b, t')





let rec trans_eager_bool f = function
    True
  | False
  | Var _
  | Unknown as t -> App(Var f, [t])
  | BinOp(Or, t1, t2) ->
      let x = new_var' "b" in
      let y = new_var' "b" in
      let f' = new_var' "f" in
      let f'' = new_var' "f" in
      let t1' = trans_eager_bool f' t1 in
      let t2' = trans_eager_bool f t2 in
      let t2'' = trans_eager_bool f'' t2 in
        Let(f', [x], If(Var x, App(Var f, [True]), t2', Let(f'', [y], If(Var y, App(Var f, [True]), App(Var f, [Unknown]), App(Var f, [Unknown])), t2'')), t1')
  | BinOp(And, t1, t2) ->
      let x = new_var' "b" in
      let y = new_var' "b" in
      let f' = new_var' "f" in
      let f'' = new_var' "f" in
      let t1' = trans_eager_bool f' t1 in
      let t2' = trans_eager_bool f t2 in
      let t2'' = trans_eager_bool f'' t2 in
        Let(f', [x], If(Var x, t2', App(Var f, [False]), Let(f'', [y], If(Var y, App(Var f, [Unknown]), App(Var f, [Unknown]), App(Var f, [Unknown])), t2'')), t1')
  | Not t ->
      let x = new_var' "b" in
      let f' = new_var' "f" in
      let t' = trans_eager_bool f' t in
        Let(f', [x], If(Var x, App(Var f, [False]), App(Var f, [True]), App(Var f, [Unknown])), t')
  | If(t1, t2, t3, t4) ->
      let x = new_var' "b" in
      let f' = new_var' "f" in
      let t1' = trans_eager_bool f' t1 in
      let t2' = trans_eager_bool f t2 in
      let t3' = trans_eager_bool f t3 in
      let t4' = trans_eager_bool f t4 in
        Let(f', [x], If(Var x, t2', t3', t4'), t1')
  | _ -> assert false
let rec trans_eager c = function
    Unit -> c [Unit]
  | True -> c [True]
  | False -> c [False]
  | Unknown -> c [Unknown]
  | Int n -> c [Int n]
  | NInt x -> c [NInt x]
  | Var x -> c [Var x]
  | Fun(x,t) ->
      let t' = trans_eager hd t in
        c [Fun(x,t')]
  | App(t1, ts) ->
      let c' xs = App(trans_eager c t1, xs) in
      let c'' =
        let aux c t xs =
          let c' xs' = c ((hd xs')::xs) in
            trans_eager c' t
        in
          List.fold_left aux c' ts
      in
        c'' []
  | If(t1, Label(_, t2), Label(_, t3), t4) ->
      let x = new_var' "b" in
      let f = new_var' "f" in
      let t1' = trans_eager_bool f t1 in
      let t2' = trans_eager hd t2 in
      let t3' = trans_eager hd t3 in
      let t4' = trans_eager hd t4 in
        c [Let(f, [x], If(Var x, t2', t3', t4'), t1')]
  | Branch(t1, t2) ->
      let t1' = trans_eager hd t1 in
      let t2' = trans_eager hd t2 in
        c [Branch(t1', t2')]
  | Let(f, xs, t1, t2) ->
      let t1' = trans_eager hd t1 in
      let t2' = trans_eager hd t2 in
        c [Let(f, xs, t1', t2')]
  | Letrec(f, xs, t1, t2) ->
      let t1' = trans_eager hd t1 in
      let t2' = trans_eager hd t2 in
        c [Letrec(f, xs, t1', t2')]
  | BinOp _
  | Not _
  | If _ as t ->
      let x = new_var' "b" in
      let f = new_var' "f" in
        Let(f, [x], c [Var x], trans_eager_bool f t)
  | Fail -> c [Fail]
  | Label(b, t) ->
      let t' = trans_eager hd t in
        c [Label(b, t')]
let trans_eager = trans_eager hd



let rec trans_eager_bool2 f = function
    True
  | False
  | Var _ as t -> App(Var f, [t])
  | Unknown ->
      Branch(App(Var f, [True]), App(Var f, [False]))
  | BinOp(Or, t1, t2) ->
      let x = new_var' "b" in
      let f' = new_var' "f" in
      let t1' = trans_eager_bool2 f' t1 in
      let t2' = trans_eager_bool2 f t2 in
        Let(f', [x], If(Var x, App(Var f, [True]), t2', Unit), t1')
  | BinOp(And, t1, t2) ->
      let x = new_var' "b" in
      let f' = new_var' "f" in
      let t1' = trans_eager_bool2 f' t1 in
      let t2' = trans_eager_bool2 f t2 in
        Let(f', [x], If(Var x, t2', App(Var f, [False]), Unit), t1')
  | Not t ->
      let x = new_var' "b" in
      let f' = new_var' "f" in
      let t' = trans_eager_bool2 f' t in
        Let(f', [x], If(Var x, App(Var f, [False]), App(Var f, [True]), App(Var f, [Unknown])), t')
  | If(t1, t2, t3, t4) ->
      let x = new_var' "b" in
      let f' = new_var' "f" in
      let t1' = trans_eager_bool2 f' t1 in
      let t2' = trans_eager_bool2 f t2 in
      let t3' = trans_eager_bool2 f t3 in
      let t4' = trans_eager_bool2 f t4 in
        Let(f', [x], If(Var x, t2', t3', t4'), t1')
  | t -> Syntax.print_term Syntax.ML false t; assert false
let rec trans_eager2 c = function
    Unit -> c [Unit]
  | True -> c [True]
  | False -> c [False]
  | Int n -> c [Int n]
  | NInt x -> c [NInt x]
  | Var x -> c [Var x]
  | Fun(x,t) ->
      let t' = trans_eager2 hd t in
        c [Fun(x,t')]
  | App(t1, ts) ->
      let c' xs = App(trans_eager2 hd t1, xs) in
      let aux c'' t xs =
        let c' xs' = c'' ((hd xs')::xs) in
          trans_eager2 c' t
      in
        c [List.fold_left aux c' ts []]
  | If(t1, t2, t3, Unit) ->
      let x = new_var' "b" in
      let f = new_var' "f" in
      let t1' = trans_eager_bool2 f t1 in
      let t2' = trans_eager2 hd t2 in
      let t3' = trans_eager2 hd t3 in
        c [Let(f, [x], If(Var x, t2', t3', Unit), t1')]
  | Branch(t1, t2) ->
      let t1' = trans_eager2 hd t1 in
      let t2' = trans_eager2 hd t2 in
        c [Branch(t1', t2')]
  | Let(f, xs, t1, t2) ->
      let t1' = trans_eager2 hd t1 in
      let t2' = trans_eager2 hd t2 in
        c [Let(f, xs, t1', t2')]
  | Letrec(f, xs, t1, t2) ->
      let t1' = trans_eager2 hd t1 in
      let t2' = trans_eager2 hd t2 in
        c [Letrec(f, xs, t1', t2')]
  | Unknown
  | BinOp _
  | Not _
  | If _ as t ->
      let x = new_var' "b" in
      let f = new_var' "f" in
        Let(f, [x], c [Var x], trans_eager_bool2 f t)
  | Fail -> c [Fail]
  | Label(b, t) ->
      let t' = trans_eager2 hd t in
        c [Label(b, t')]
  | Event s -> c [Event s]
let trans_eager2 = trans_eager2 hd







let abstract t =
  let t1 = hd (abstract [] [] (t,TUnit)) in
  let t2 = if Flag.use_part_eval then part_eval t1 else t1 in
  let () = if Flag.print_abst then Format.printf "Abstracted Program:@.%a@.@." (Syntax.print_term_fm Syntax.ML false) t2 in
  let t3 = if !Flag.use_unknown then trans_eager t2 else trans_eager2 t2 in
  let t4 = (*part_eval*) t3 in
    t4





















