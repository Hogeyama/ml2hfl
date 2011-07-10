
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









(* ??? *)
let add_to_cond t cond =
  let fv = get_fv t in
    if List.exists (fun x -> Id.typ x = TBool) fv
    then cond
    else t::cond





let get_preds = function
    TInt ps -> ps
  | TList(_,ps) -> ps
  | _ -> assert false



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



(*
let weakest env cond ds p =
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
        if check env cond [] p then (*???*)
          true_term, false_term
        else if check env cond [] (make_not p) then (*???*)
          false_term, true_term
        else
          let ss ids = List.map Id.id ids in
          let fvp = ss (Util.uniq compare (get_fv2(*???*) p)) in
          let ts = cond @@ List.map fst ds in
          let ds =
            let rec fixp xs =
              let xs' =
                Util.uniq compare
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
          let nds = List.map (fun (p, b) -> {desc=Not p;typ=TBool}, {desc=Not b;typ=TBool}) ds in

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
                     check cond pbs {desc=Not p;typ=TBool})
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
          let pbss = List.filter (fun pbs -> not (check cond pbs false_term)) pbss in
          let npbss = List.map f nxs in
          let npbss = List.filter (fun pbs -> not (check cond pbs false_term)) npbss in
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
  let conj pbset =
    let pbs = PredSet.elements pbset in
      match pbs with
        [] -> true_term
      | (_,b)::pbs ->
          List.fold_left (fun t (_,b) -> {desc=BinOp(And, t, b);typ=TBool}) b pbs
  in
  let make_dnf pbsetset =
    let pbsets = PredSetSet.elements pbsetset in
      match pbsets with
        [] -> false_term
      | pbset::pbsets' -> List.fold_left (fun t pbset -> {desc=BinOp(Or, t, conj pbset);typ=TBool}) (conj pbset) pbsets'
  in
  let f pbset pb = PredSet.add pb pbset in
  let pbset = List.fold_left f PredSet.empty pbs in
  let pbsetset = PredSetSet.singleton pbset in
  let ps,_ = List.split pbs in
    if Wrapper.check (cond@@ps) false_term
    then
      let pbsetset' = contradict_aux cond pbsetset PredSetSet.empty false_term in
        make_dnf pbsetset'
    else false_term





let abst cond pbs p =
  let tt, ff = weakest cond pbs p in
    if tt.desc = Not false_term || Not true_term = ff.desc
    then true_term
    else {desc=If(tt, true_term, {desc=If(ff, false_term, {desc=Unknown;typ=TBool});typ=TBool}); typ=TBool}







let inst_var t p =
  subst abst_var t p









let rec abstract_typ = function
    TUnit -> TUnit
  | TBool -> TBool
  | TAbsBool -> assert false
  | TInt _ -> TUnknown
  | TRInt _ -> assert false
  | TVar _ -> assert false
  | TFun({Id.typ=TInt ps} as x,typ) ->
      let rec aux = function
          [] -> abstract_typ typ
        | _::ps -> TFun(Id.set_typ x TAbsBool, aux ps)
      in
        aux ps
  | TFun(x,typ) -> TFun(Id.set_typ x (abstract_typ (Id.typ x)), abstract_typ typ)
  | TList _ -> assert false
  | TConstr(x,y) -> TConstr(x,y)
  | TVariant _ -> assert false
  | TRecord _ -> assert false
  | TUnknown -> assert false


let abst_arg x (xs,pbs) =
  match Id.typ x with
      TInt ps
    | TList(_,ps) ->
        let xs' = List.map (fun _ -> Id.new_var "b" TAbsBool) ps in
        let pbs' = List.map2 (fun p b -> inst_var (make_var x) p, make_var b) ps xs' in
          xs'@xs, pbs'@pbs
    | _ -> (Id.set_typ x (abstract_typ (Id.typ x)))::xs, pbs



let filter_fail cond pbs t =
  let p = contradict cond pbs in
  let bot = Id.new_var "bot" TUnit in
  let bot' = {desc=Var bot; typ=TUnit} in
    {desc=If(p, {desc=Let(Flag.Recursive, bot, [], bot', bot');typ=t.typ}, t); typ=t.typ}




let make_new_fun x t =
  let typ = t.typ in
  let f = Id.new_var "f" (TFun(x,typ)) in
    {desc=Let(Flag.Nonrecursive, f, [x], t, {desc=Var f;typ=Id.typ f}); typ=TFun(x,typ)}



let rec coerce cond pbs typ1 typ2 t =
          Format.printf "COERCE: %a -> %a, %a:%a@." print_typ typ1 print_typ typ2 pp_print_term t print_typ t.typ;
  match typ1,typ2 with
      TUnit,TUnit -> filter_fail cond pbs t
    | TBool, TBool -> t
    | TVariant ctypss1, TVariant ctypss2 when ctypss1=ctypss2 -> t
    | TFun({Id.typ=TList(_,ps1)} as x1,typ12), TFun({Id.typ=TList(_,ps2)} as x2,typ22) ->
        let xs,pbs' = abst_arg x2 ([],pbs) in
        let cond' = {desc=BinOp(Eq, make_var x1, {desc=Var x2;typ=Id.typ x2});typ=TBool}::cond in
        let ts = List.map (fun p -> abst cond' pbs' (inst_var {desc=Var x2;typ=Id.typ x2} p)) ps1 in
        let t' = coerce cond' pbs' typ12 typ22 {desc=App(t, ts);typ=typ12} in
          List.fold_right make_new_fun xs t'

    | TFun({Id.typ=TInt ps1} as x1,typ12), TFun({Id.typ=TInt ps2} as x2,typ22) ->
        let xs,pbs' = abst_arg x2 ([],pbs) in
        let cond' = {desc=BinOp(Eq, make_var x1, make_var x2);typ=TBool}::cond in
        let ts = List.map (fun p -> abst cond' pbs' (inst_var (make_var x2) p)) ps1 in
        let t' = coerce cond' pbs' typ12 typ22 {desc=App(t, ts);typ=typ12} in
          List.fold_right make_new_fun xs t'

    | TFun({Id.typ=TRInt p} as x1,typ12), TFun({Id.typ=TInt ps2} as x2,typ22) ->
        let xs,pbs' = abst_arg x2 ([],pbs) in
        let cond' = {desc=BinOp(Eq, make_var x1, make_var x2);typ=TBool}::cond in
        let t' = coerce cond' pbs' typ12 typ22 t in
        let x = Id.new_var "x" TUnit in
        let f = Id.new_var "f" (TFun(x,TUnit)) in
        let tru,_ = weakest cond' pbs' (inst_var (make_var x2) p) in
        let t'' = {desc=If(tru, t', {desc=App(fail_term, [unit_term]);typ=TBottom});typ=typ12} in
          List.fold_right make_new_fun xs t''

    | TFun({Id.typ=TInt ps1} as x1,typ12), TFun({Id.typ=TRInt p} as x2,typ22) ->
        let cond' = (inst_var (make_var x2) p)::{desc=BinOp(Eq, make_var x1, make_var x2);typ=TBool}::cond in
        let ts = List.map (fun p -> abst cond' pbs (inst_var (make_var x2) p)) ps1 in
          coerce cond' pbs typ12 typ22 {desc=App(t, ts);typ=typ12}

    | TFun(x1,typ12), TFun(x2,typ22) ->
        let x = Id.new_var "x" (Id.typ x1) in
          (* x cannot be depended because x is function? *)
          make_new_fun x (coerce cond pbs typ12 typ22 {desc=App(t, [coerce cond pbs (Id.typ x2) (Id.typ x1) (make_var x)]);typ=typ12})
    | TUnknown,_ -> t
    | _,TUnknown -> t
    | _,_ -> Format.printf "coerce:%a,%a@." print_typ typ1 print_typ typ2; assert false

let rec abstract_list t = assert false
(*
    Var x -> make_var x
  | Nil -> Nil
  | Cons(_,t2) -> BinOp(Add, Int 1, abstract_list t2)
  | _ -> assert false
*)


let rec abstract cond pbs (t,typ) =
  let typ' = abstract_typ typ in
    match t.desc,typ with
        Unit, TUnit -> [{desc=Unit;typ=TUnit}]
      | True, TBool -> [{desc=True;typ=TBool}]
      | False, TBool -> [{desc=False;typ=TBool}]
      | Unknown, TBool -> [{desc=Unknown;typ=TBool}]
      | _, TRInt p -> []
      | _, (TInt _ | TList _) ->
          List.map (fun p -> abst cond pbs (inst_var t p)) (get_preds t.typ)
      | Var x, typ when Wrapper.congruent cond (Id.typ x) typ ->
          Format.printf "ABS_VAR1: %a@." Id.print x;
          [make_var (Id.set_typ x typ')]
      | Var x, typ ->
          Format.printf "ABS_VAR2: %a, %a@." Id.print x print_typ typ;
          abstract cond pbs ({desc=App(make_var x, []);typ=Id.typ x}, typ)
      | RandInt None, _ -> assert false
      | RandInt (Some t), typ ->
          let x = Id.new_var "" (TInt[]) in
            abstract cond pbs (t,TFun(x,typ))
      | Fun _, _ -> assert false
      | App({desc=Let(Flag.Nonrecursive, f, xs, t1, t2)}, ts), typ ->
          abstract cond pbs ({desc=Let(Flag.Nonrecursive, f, xs, t1, {desc=App(t2, ts);typ=t.typ});typ=t.typ}, typ)
      | App({desc=App(t, ts1)}, ts2), typ ->
          abstract cond pbs ({desc=App(t, ts1 @ ts2);typ=t.typ}, typ)
      | App({desc=Fail}, _), _ -> [{desc=App(fail_term, [unit_term]);typ=TBottom}]
      | App({desc=Event s}, [t1]), _ ->
          let t1' = abstract cond pbs (t1,TUnit) in
            [{desc=App(event_term s, t1'); typ=typ'}]
      | App({desc=Var f}, ts), typ ->
          Format.printf "ABS_APP: %a, %a@." print_id_typ f print_typ typ;
          let ttyps,typ'' =
            let rec aux = function
                [], typ -> [], typ
              | t::ts, TFun(x,typ2) ->
                  let typ2' = subst_type x t typ2 in
                    Format.printf "%a, %a@." pp_print_term t print_typ (Id.typ x);
                  let ttyps,typ = aux (ts, typ2') in
                    (t,Id.typ x)::ttyps, typ
              | ts, typ ->
                  Format.printf "Abstract.abstract:@.%n:%a:%a@."
                    (List.length ts) (Syntax.print_term_fm Syntax.ML false) (make_var f) print_typ typ;
                  assert false
            in
              aux (ts, Id.typ f)
          in
          let ps = Util.rev_flatten_map (function (t, TRInt(p)) -> [inst_var t p] | _ -> []) ttyps in
          let ts' = List.flatten (List.map (abstract cond pbs) ttyps) in
          let f_typ =
            let aux {typ=typ1} typ2 =
              let x = Id.new_var "x" typ1 in
                TFun(x,typ2)
            in
              List.fold_right aux ts' typ'
          in
          let tru,_ = weakest cond pbs (and_list ps) in
          let typ''' = abstract_typ typ'' in
          let t2 = coerce cond pbs typ'' typ {desc=App(make_var (Id.set_typ f (abstract_typ (Id.typ f))), ts');typ=typ'''} in
          let t3 = {desc=App(fail_term, [unit_term]);typ=TBottom} in
            [{desc=If(tru, t2, t3); typ=typ'}]
      | BinOp(op, t1, t2), typ ->
          begin
            match op with
                Eq
              | Lt
              | Gt
              | Leq
              | Geq
              | And
              | Or -> [abst cond pbs t]
              | Add
              | Sub
              | Mult -> assert false
          end
      | If(t1, t2, t3), typ ->
          let x1 = Id.new_var "x" typ' in
          let x2 = Id.new_var "x" typ' in
          let cond2 = add_to_cond t1 cond in
          let cond3 = add_to_cond {desc=Not t1;typ=TBool} cond in
          let t1' = hd (abstract cond pbs (t1,TBool)) in
          let t2' = hd (abstract cond2 pbs (t2,typ)) in
          let t3' = hd (abstract cond3 pbs (t3,typ)) in
          let t2'' = {desc=Label(true, make_var x1); typ=typ'} in
          let t3'' = {desc=Label(false, make_var x2); typ=typ'} in
            [{desc=Let(Flag.Nonrecursive, x1,[],t2', {desc=Let(Flag.Nonrecursive, x2,[],t3', {desc=If(t1', t2'', t3'');typ=typ'});typ=typ'}); typ=typ'}]
      | Branch(t1, t2), typ ->
          let t1' = hd (abstract cond pbs (t1,typ)) in
          let t2' = hd (abstract cond pbs (t2,typ)) in
            [{desc=Branch(t1', t2');typ=typ'}]
      | Let(flag, f, xs, t1, t2), typ ->
          let f' = Id.set_typ f (abstract_typ (Id.typ f)) in
            if flag = Flag.Nonrecursive
            then
              let xs',pbs' = List.fold_right abst_arg xs ([],pbs) in
              let () = assert (List.length (get_args (Id.typ f)) = List.length xs) in
              let t1' = filter_fail cond pbs' (hd (abstract cond pbs' (t1,TUnit))) in
              let t2' = hd (abstract cond pbs (t2,typ)) in
                [{desc=Let(flag, f', xs', t1', t2');typ=typ'}]
            else
              let xs',pbs' = List.fold_right abst_arg xs ([],pbs) in
              let () = assert (List.length (get_args (Id.typ f)) = List.length xs) in
              let t1' = filter_fail cond pbs' (hd (abstract cond pbs' (t1,TUnit))) in
              let t2' = hd (abstract cond pbs (t2,typ)) in
                [{desc=Let(flag, f', xs', t1', t2');typ=typ'}]
      | Not t, _ ->
          let t' = hd (abstract cond pbs (t,TBool)) in
            [{desc=Not t';typ=typ'}]
      | Fail, _ -> [fail_term]
      | Event s, _ -> [event_term s]
      | App(t, []), typ -> abstract cond pbs (t, typ)
      | Match(t1,t2,x,y,t3), typ -> assert false(*
                                                  let t1' = hd (abstract cond pbs (t1,TList(TUnknown,[{desc=BinOp(Eq,make_var abst_var,{desc=Nil;typ=TList(TUnknown,[]));typ=TBool}]))) in
                                                  let t2' = hd (abstract cond pbs (t2,typ)) in
                                                  let t3' = hd (abstract cond pbs (t3,typ)) in
                                                  [If(t1', Label(true,t2'), Label(false,t3'))]*)
      | Constr(c,ts), typ -> [unit_term]
      | Match_(_,pats), typ -> assert false(*
                                             let aux (pat,cnd,t1) (t2,c) = (***)
                                             let t1' = hd (abstract cond pbs (t1,typ)) in
                                             Branch(LabelInt(c,t1'),t2), c-1
                                             in
                                             let t,_ = List.fold_right aux pats (Unit,List.length pats-1) in
                                             [t]*)
      | _, typ -> (Format.printf "Abstract.abstract:@.%a:%a@." (print_term_fm ML false) t print_typ typ; assert false)
*)




(*
let rec eager_unknown t =
  let desc =
  match t.desc with
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
  | If(t1, t2, t3) ->
      let t1' = eager_unknown t1 in
      let t2' = eager_unknown t2 in
      let t3' = eager_unknown t3 in
        If(t1', t2', t3')
  | Branch(t1, t2) ->
      let t1' = eager_unknown t1 in
      let t2' = eager_unknown t2 in
        Branch(t1', t2')
  | Let(Flag.Nonrecursive, f, xs, t1, t2) ->
      let t1' = eager_unknown t1 in
      let t2' = eager_unknown t2 in
      let ys = get_fv t1 in
      let check_mult x =
        if x.typ = TAbsBool
        then
          let n = List.fold_left (fun n y -> if Id.same x y then n+1 else n) 0 ys in
            n >= 2
        else false
      in
      let xs' = List.filter check_mult xs in
      let aux x t =
        let y = Id.new_var "be" TAbsBool in
        let g = Id.new_var "fe" (TFun((y,y.typ),TUnit)) in
          Let(Flag.Nonrecursive, g, [y], subst x (make_var y) t, {desc=Branch(App(make_var g, [True]), App(make_var g, [False]));typ=TBool})
      in
      let t1'' = List.fold_right aux xs' t1' in
        Let(Nonrecursive, f, xs, t1'', t2')
  | Let(Flag.Recursive, f, xs, t1, t2) ->
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
        let y = Id.new_var "be" TAbsBool in
        let g = Id.new_var "fe" (TFun((y,y.typ),TUnit)) in
          Let(Nonrecursive, g, [y], subst x (make_var y) t, Branch(App(make_var g, [True]), App(make_var g, [False])))
      in
      let t1'' = List.fold_right aux xs' t1' in
        Let(Recursive, f, xs, t1'', t2')
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
  | Event s -> Event s
  in
  {desc=desc; typ=t.typ}
*)



(*
let rec trans_eager_bool f t =
    True
  | False
  | Var _
  | Unknown as t -> App(make_var f, [t])
  | BinOp(Or, t1, t2) ->
      let x = Id.new_var "b" TBool in
      let f' = Id.new_var "f" (TFun((x,x.typ),TUnit)) in
      let t1' = trans_eager_bool f' t1 in
      let t2' = trans_eager_bool f t2 in
        Let(Nonrecursive, f', [x], If(make_var x, App(make_var f, [True]), t2'), t1')
  | BinOp(And, t1, t2) ->
      let x = Id.new_var "b" TBool in
      let f' = Id.new_var "f" (TFun((x,x.typ),TUnit)) in
      let t1' = trans_eager_bool f' t1 in
      let t2' = trans_eager_bool f t2 in
        Let(Nonrecursive, f', [x], If(make_var x, t2', App(make_var f, [False])), t1')
  | Not t ->
      let x = Id.new_var "b" TBool in
      let f' = Id.new_var "f" (TFun((x,x.typ),TUnit)) in
      let t' = trans_eager_bool f' t in
        Let(Nonrecursive, f', [x], If(make_var x, App(make_var f, [False]), App(make_var f, [True])), t')
  | If(t1, t2, t3) ->
      let x = Id.new_var "b" TBool in
      let f' = Id.new_var "f" (TFun((x,x.typ),TUnit)) in
      let t1' = trans_eager_bool f' t1 in
      let t2' = trans_eager_bool f t2 in
      let t3' = trans_eager_bool f t3 in
        Let(Nonrecursive, f', [x], If(make_var x, t2', t3'), t1')
  | _ -> assert false
let rec trans_eager c = function
    Unit -> c [Unit]
  | True -> c [True]
  | False -> c [False]
  | Unknown -> c [Unknown]
  | Int n -> c [Int n]
  | NInt x -> c [NInt x]
  | Var x -> c [make_var x]
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
  | If(t1, Label(_, t2), Label(_, t3)) ->
      let x = Id.new_var "b" TBool in
      let f = Id.new_var "f" (TFun((x,x.typ),TUnit)) in
      let t1' = trans_eager_bool f t1 in
      let t2' = trans_eager hd t2 in
      let t3' = trans_eager hd t3 in
        c [Let(Nonrecursive, f, [x], If(make_var x, t2', t3'), t1')]
  | Branch(t1, t2) ->
      let t1' = trans_eager hd t1 in
      let t2' = trans_eager hd t2 in
        c [Branch(t1', t2')]
  | Let _ -> Format.printf "Not implemented"; assert false
(*
  | Let(f, xs, t1, t2) ->
      let t1' = trans_eager hd t1 in
      let t2' = trans_eager hd t2 in
        c [Let(f, xs, t1', t2')]
  | Letrec(f, xs, t1, t2) ->
      let t1' = trans_eager hd t1 in
      let t2' = trans_eager hd t2 in
        c [Letrec(f, xs, t1', t2')]
*)
  | BinOp _
  | Not _
  | If _ as t ->
      let x = Id.new_var "b" TBool in
      let f = Id.new_var "f" (TFun((x,x.typ),TUnit)) in
        Let(Nonrecursive, f, [x], c [make_var x], trans_eager_bool f t)
  | Fail -> c [Fail]
  | Label(b, t) ->
      let t' = trans_eager hd t in
        c [Label(b, t')]
  | Event s -> assert false
let trans_eager = trans_eager hd
*)


let rec trans_eager_bool2 f t =
  let desc =
    match t.desc with
        True
      | False
      | Var _ -> App(make_var f, [t])
      | Unknown ->
          Branch({desc=App(make_var f, [true_term]);typ=TUnit}, {desc=App(make_var f, [false_term]);typ=TUnit})
      | BinOp(Or, t1, t2) ->
          let x = Id.new_var "b" TBool in
          let f' = Id.new_var "f" (TFun(x,TUnit)) in
          let t1' = trans_eager_bool2 f' t1 in
          let t2' = trans_eager_bool2 f t2 in
            Let(Flag.Nonrecursive, f', [x], {desc=If(make_var x, {desc=App(make_var f, [true_term]);typ=TUnit}, t2');typ=TUnit}, t1')
      | BinOp(And, t1, t2) ->
          let x = Id.new_var "b" TBool in
          let f' = Id.new_var "f" (TFun(x,TUnit)) in
          let t1' = trans_eager_bool2 f' t1 in
          let t2' = trans_eager_bool2 f t2 in
            Let(Flag.Nonrecursive, f', [x], {desc=If(make_var x, t2', {desc=App(make_var f, [false_term]);typ=TUnit});typ=TUnit}, t1')
      | Not t ->
          let x = Id.new_var "b" TBool in
          let f' = Id.new_var "f" (TFun(x,TUnit)) in
          let t' = trans_eager_bool2 f' t in
          let t1 = {desc=App(make_var f, [false_term]); typ=TUnit} in
          let t2 = {desc=App(make_var f, [true_term]); typ=TUnit} in
            Let(Flag.Nonrecursive, f', [x], {desc=If(make_var x, t1, t2);typ=TUnit}, t')
      | If(t1, t2, t3) ->
          let x = Id.new_var "b" TBool in
          let f' = Id.new_var "f" (TFun(x,TUnit)) in
          let t1' = trans_eager_bool2 f' t1 in
          let t2' = trans_eager_bool2 f t2 in
          let t3' = trans_eager_bool2 f t3 in
            Let(Flag.Nonrecursive, f', [x], {desc=If(make_var x, t2', t3');typ=TUnit}, t1')
      | _ -> assert false
  in
    {desc=desc; typ=TUnit}

let is_bool_const t =
  match t.desc with
      True | False | Unknown -> true
    | _ -> false

let rec trans_eager2 c t =
  match t.desc with
      Unit -> c [unit_term]
    | True -> c [true_term]
    | False -> c [false_term]
    | Int n -> assert false
    | NInt x -> assert false
    | RandInt None -> assert false
    | RandInt (Some t) -> assert false
    | Var x -> c [make_var x]
    | Fun(x,t) ->
        let t' = trans_eager2 hd t in
          c [{desc=Fun(x,t');typ=TFun(x,t'.typ)}]
    | App(t1, ts) ->
        let c' xs = {desc=App(trans_eager2 hd t1, xs); typ=t.typ} in
        let aux c'' t xs =
          let c' xs' = c'' ((hd xs')::xs) in
            trans_eager2 c' t
        in
          c [List.fold_left aux c' ts []]
    | If(t1, t2, t3) when not (is_bool_const t2) && not (is_bool_const t3) ->
        let () = Format.printf "If: t2 = %a, t3 = %a@." pp_print_term t2 pp_print_term t3 in
        let x = Id.new_var "b" TBool in
        let f = Id.new_var "f" (TFun(x,TUnit)) in
        let t1' = trans_eager_bool2 f t1 in
        let t2' = trans_eager2 hd t2 in
        let t3' = trans_eager2 hd t3 in
          c [{desc=Let(Flag.Nonrecursive, f, [x], {desc=If(make_var x, t2', t3');typ=t2'.typ}, t1');typ=t1'.typ}]
    | Branch(t1, t2) ->
        let t1' = trans_eager2 hd t1 in
        let t2' = trans_eager2 hd t2 in
          c [{desc=Branch(t1', t2');typ=t1'.typ}]
    | Let(flag, f, xs, t1, t2) ->
        if flag = Flag.Nonrecursive
        then
          let t1' = trans_eager2 hd t1 in
          let t2' = trans_eager2 hd t2 in
            c [{desc=Let(flag, f, xs, t1', t2');typ=t2'.typ}]
        else
          let t1' = trans_eager2 hd t1 in
          let t2' = trans_eager2 hd t2 in
            c [{desc=Let(flag, f, xs, t1', t2');typ=t2'.typ}]
    | Unknown
    | BinOp _
    | Not _
    | If _ ->
        let x = Id.new_var "b" TBool in
        let f = Id.new_var "f" (TFun(x,TUnit)) in
        let t2 = c [make_var x] in
        let t3 = trans_eager_bool2 f t in
          {desc=Let(Flag.Nonrecursive, f, [x], t2, t3); typ=t2.typ}
    | Fail -> c [fail_term]
    | Label(b, t) ->
        let t' = trans_eager2 hd t in
          c [{desc=Label(b, t');typ=t'.typ}]
    | LabelInt(n, t) ->
        let t' = trans_eager2 hd t in
          c [{desc=LabelInt(n, t');typ=t'.typ}]
    | Event s -> c [event_term s]
let trans_eager2 = trans_eager2 hd

(*
let rec lift = function
    Unit -> [], Unit
  | True -> [], True
  | False -> [], False
  | Unknown -> [], Unknown
  | Int n -> [], Int n
  | NInt x -> [], NInt x
  | RandInt None -> [], RandInt None
  | RandInt (Some t) ->
      let defs,t' = lift t in
        defs, RandInt (Some t')
  | make_var x -> [], make_var x
  | Fun _ -> Format.printf "Not implemented@."; assert false
  | App(t, ts) ->
      let defs,t' = lift t in
      let defss,ts' = List.split (List.map lift ts) in
        defs @ (List.flatten defss), App(t', ts')
  | If(t1,t2,t3) ->
      let defs1,t1' = lift t1 in
      let defs2,t2' = lift t2 in
      let defs3,t3' = lift t3 in
        defs1 @ defs2 @ defs3, If(t1',t2',t3')
  | Branch(t1,t2) ->
      let defs1,t1' = lift t1 in
      let defs2,t2' = lift t2 in
        defs1 @ defs2, Branch(t1',t2')
  | Let(Nonrecursive,f,ys,t1,t2) ->
      let defs1,t1' = lift t1 in
      let defs2,t2' = lift t2 in
        defs1 @ [(f,(ys,t1'))] @ defs2, t2'
  | Let(Recursive,f,ys,t1,t2) ->
      let defs1,t1' = lift t1 in
      let defs2,t2' = lift t2 in
        defs1 @ [(f,(ys,t1'))] @ defs2, t2'
  | BinOp(op,t1,t2) ->
      let defs1,t1' = lift t1 in
      let defs2,t2' = lift t2 in
        defs1 @ defs2, BinOp(op,t1',t2')
  | Not t ->
      let defs,t' = lift t in
        defs, Not t'
  | Fail -> [], Fail
  | Label(b,t) ->
      let defs,t' = lift t in
        defs, Label(b,t')
  | Event s -> [], Event s
  | Record(b,fields) ->
      let aux (s,(f,t)) =
        let defs,t' = lift t in
          defs, (s,(f,t'))
      in
      let defss,fields' = List.split (List.map aux fields) in
        List.flatten defss, Record(b,fields')
  | Proj(n,i,s,f,t) ->
      let defs,t' = lift t in
        defs, Proj(n,i,s,f,t')
  | Nil -> [], Nil
  | Cons(t1,t2) ->
      let defs1,t1' = lift t1 in
      let defs2,t2' = lift t2 in
        defs1 @ defs2, Cons(t1',t2')
  | Match(t1,t2,x,y,t3) ->
      let defs1,t1' = lift t1 in
      let defs2,t2' = lift t2 in
      let defs3,t3' = lift t3 in
        defs1 @ defs2 @ defs3, Match(t1',t2',x,y,t3')
  | Constr(c,ts) ->
      let defss,ts' = List.split (List.map lift ts) in
        List.flatten defss, Constr(c,ts')
  | Match_(t,pats) ->
      let defs,t' = lift t in
      let aux (pat,cond,t) (defs,pats) =
        let defs',cond' =
          match cond with
              None -> [], None
            | Some t ->
                let defs',t' = lift t in
                  defs', Some t'
        in
        let defs'',t' = lift t in
          defs''@defs'@defs, (pat,cond',t')::pats
      in
      let defs',pats' = List.fold_right aux pats (defs,[]) in
        defs', Match_(t',pats')
  | Type_decl(decls,t) ->
      let defs,t' = lift t in
        defs, Type_decl(decls,t')
*)

(*
let abstract t =
  let t1 = hd (abstract [] [] (t,TUnit)) in
  let () = if Flag.print_abst then Format.printf "Abstracted Program:@.%a@.@." (Syntax.print_term' Syntax.ML 0 false) t1 in
  let () = Type_check.check t1 in
  let t2 = if Flag.use_part_eval then part_eval t1 else t1 in
  let () = if Flag.print_abst then Format.printf "Abstracted Program:@.%a@.@." (Syntax.print_term' Syntax.ML 0 false) t2 in
  let () = Type_check.check t2 in
  let t3 = (*if !Flag.use_unknown then trans_eager t2 else*) trans_eager2 t2 in
  let () = if Flag.print_abst then Format.printf "trans_eager:@.%a@.@." (Syntax.print_term' Syntax.ML 0 false) t3 in
  let () = if Flag.print_abst then Format.printf "trans_eager:@.%a@.@." pp_print_term t3 in
  let () = Type_check.check t3 in
    lift t3
*)







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


let weakest env (cond:CEGAR_syntax.t list) ds p =
(*
  if check env cond [] p then (*???*)
    Const True, Const False
  else if check env cond [] (make_not p) then (*???*)
    Const False, Const True
  else
*)
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
    else make_temp_if tt (Const True) (make_temp_if ff (Const False) (Const Unknown))



let assume env cond pbs t =
  let _,ff = weakest env cond pbs t in
    make_if ff loop_term t
    


let abst_arg x ps = Util.mapi (fun i p -> p, App(Const (Proj i), Var x)) (ps (Var x))

(*
let filter_fail cond pbs t =
  let p = contradict cond pbs in
  let bot = Id.new_var "bot" TUnit in
  let bot' = {desc=Var bot; typ=TUnit} in
    {desc=If(p, {desc=Let(Flag.Recursive, bot, [], bot', bot');typ=t.typ}, t); typ=t.typ}
*)


let rec coerce env cond pts typ1 typ2 :          (string * string list * CEGAR_syntax.t * CEGAR_syntax.t) list *
         string
=
  match typ1,typ2 with
      TBase(_,ps1),TBase(_,ps2) ->
        let x = new_id "x" in
        let pts' = abst_arg x ps1 @@ pts in
        let ts = List.map (abst env cond pts') (ps2 (Var x)) in
        let t = List.fold_left (fun t1 t2 -> App(t1,t2)) (Const (Tuple (List.length ts))) ts in
          make_fun [x] t
    | TFun typ1, TFun typ2 ->
        let f = new_id "f" in
        let x = new_id "x" in
        let typ11,typ12 = typ1 (Var x) in
        let typ21,typ22 = typ2 (Var x) in
        let defs1,f1 = coerce env cond pts typ12 typ22 in
        let defs2,f2 = coerce env cond pts typ21 typ11 in
        let defs,f' = make_fun [f;x] (App(Var f1, App(Var f, App(Var f2, Var x)))) in
          defs@@defs1@@defs2, f'
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
        let typ = get_typ env t1 in
        let typ1,typ2 =
          match typ with
              TFun typ -> typ t2
            | _ -> assert false
        in
        let defs1,t1' = abstract_term env cond pbs t typ in
        let defs2,t2' = abstract_term env cond pbs t typ1 in
        let defs,f = coerce env cond pbs typ2 typ in
          defs@@defs1@@defs2, App(Var f, App(t1',t2'))


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
          let env' =
            match typ1 with
                TBase _ -> (x,typ1)::env
              | _ -> env
          in
            aux typ2 xs' env'
  in
  let typ,arg_env = aux (List.assoc f env) xs [] in
  let pbs = rev_flatten_map (function (x,TBase(_,ps)) -> abst_arg x ps | _ -> assert false) arg_env in
  let defs1,t1' = assume env [] pbs t1 in
  let defs2,t2' = abstract_term arg_env [t1] pbs t2 typ in
    (f, xs, t1', t2')::defs1@@defs2

let abstract env (defs,main) =
  rev_flatten_map (abstract_def env) defs, main
