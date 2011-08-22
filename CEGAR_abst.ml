open Utilities
open CEGAR_const
open CEGAR_syntax
open CEGAR_type
open CEGAR_print



let check env cond pbs p =
  let ps,_ = List.split pbs in
    Wrapper2.check env (cond@@ps) p

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
  if check env cond [] p then (*???*)
    Const True, Const False
  else if check env cond [] (make_not p) then (*???*)
    Const False, Const True
  else
    let fvp = get_fv p in
    let ts = ((cond @@ List.map fst ds):CEGAR_syntax.t list) in
    let ds =
      let rec fixp xs =
        let xs' =
          uniq compare
            (xs @
               (List.flatten
                  (List.map
                     (fun p ->
                        let fv = get_fv p in
                          if inter fv xs = []
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
             if inter fvp fvs = [] && inter (rev_map_flatten get_fv cond) fvs = [] then
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
             if inter fvp fvs = [] && inter (rev_map_flatten get_fv cond) fvs = [] then
               false
             else
               check env cond pbs (make_not p))
        pbss
      in
      let xs = uniq compare (xs' @ xs) in
      let nxs = uniq compare (nxs' @ nxs) in
      let ys = uniq compare (ys' @ ys) in
      let ws = 
        uniq compare
          (List.flatten
             (List.map
                (fun y1 ->
                   List.map
                     (fun y2 ->
                        List.sort compare
                          (uniq compare (y1 @ y2)))
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
          diff ws ys
        else
          List.filter (fun w -> not (List.exists (fun x -> diff w x = []) ys)) ws
      in
      let ws = List.filter
        (fun w -> not (List.exists (fun x -> diff x w = []) xs) &&
           not (List.exists (fun x -> diff x w = []) nxs)) ws in
      let ws =
        let rec aux xs =
          match xs with
              [] -> []
            | x::xs' ->
                if List.exists (fun y -> diff y x = []) xs' then aux xs' else x::(aux xs')
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
    else make_if tt (Const True) (make_if ff (Const False) (make_br (Const True) (Const False)))



let assume env cond pbs t1 t2 =
  let _,ff = weakest env cond pbs t1 in
    if ff = Const False
    then t2
    else make_if ff (Const Bottom) t2

(* TODO: equiv *)
let rec congruent env cond typ1 typ2 =
  match typ1,typ2 with
      TBase(TBottom,_), _ -> true
    | TBase(b1,ps1), TBase(b2,ps2) ->
        assert (b1=b2);
        let x = new_id "x" in
(*
          List.for_all2 (equiv env cond) (ps1 (Var x)) (ps2 (Var x))
*)
        let ps1' = ps1 (Var x) in
        let ps2' = ps2 (Var x) in
          List.length ps1' = List.length ps2' && List.for_all2 (=) ps1' ps2'
    | TFun typ1, TFun typ2 ->
        let x = new_id "x" in
        let typ11,typ12 = typ1 (Var x) in
        let typ21,typ22 = typ2 (Var x) in
          congruent env cond typ11 typ21 && congruent env cond typ12 typ22


let abst_arg x typ =
  let ps =
    match typ with
        TBase(_,ps) -> ps (Var x)
      | _ -> []
  in
  let n = List.length ps in
    Utilities.mapi (fun i p -> p, App(Const (Proj(n,i)), Var x)) ps

let rec coerce env cond pts typ1 typ2 t =
  match typ1,typ2 with
      _ when congruent env cond typ1 typ2 -> t
    | TBase(_,ps1),TBase(_,ps2) ->
        let x = new_id "x" in
        let env' = (x,typ1)::env in
        let pts' = abst_arg x typ1 @@ pts in
        let ts = List.map (abst env' cond pts') (ps2 (Var x)) in
          Let(x, t, make_app (Const (Tuple (List.length ts))) ts)
    | TFun typ1, TFun typ2 ->
        let x = new_id "x" in
        let typ11,typ12 = typ1 (Var x) in
        let typ21,typ22 = typ2 (Var x) in
        let env' = (x,typ11)::env in
        let t1 = coerce env' cond pts typ21 typ11 (Var x) in
        let t2 = coerce env' cond pts typ12 typ22 (App(t, t1)) in
          Fun(x, t2)
    | _ -> Format.printf "COERCE: %a, %a@." print_typ typ1 print_typ typ2; assert false




let rec is_base_term env = function
    Const (Unit | True | False | Int _ | RandInt) -> true
  | Const _ -> false
  | Var x ->
      begin
        match List.assoc x env with
            TBase _ -> true
          | _ -> false
      end
  | App(App(Const (And|Or|Lt|Gt|Leq|Geq|Eq|Add|Sub|Mul),t1),t2) ->
      assert (is_base_term env t1);
      assert (is_base_term env t2);
      true
  | App(Const Not,t) -> assert (is_base_term env t); true
  | App _ -> false
  | Let _ -> false

let rec abstract_term env cond pbs t typ =
  match t with
      Const RandInt ->
        let typ' = TBase(TInt, fun x -> []) in
          coerce env cond pbs typ' typ (Const (Tuple 0))
    | t when is_base_term env t ->
        let typ' =
          match get_typ env t with
              TBase(b,_) -> TBase(b, fun x -> [make_eq x t])
            | _ -> assert false
        in
          coerce env cond pbs typ' typ (App(Const (Tuple 1), Const True))
    | Const c -> coerce env cond pbs (get_const_typ c) typ t
    | Var x -> coerce env cond pbs (List.assoc x env) typ t
    | App(Const (Event s), t) -> App(Const (Event s), abstract_term env cond pbs t typ)
    | App(Const (Label n), t) -> App(Const (Label n), abstract_term env cond pbs t typ)
    | App(t1, t2) ->
        let typ' = get_typ env t1 in
        let typ1,typ2 =
          match typ' with
              TFun typ -> typ t2
            | TBase(TBottom,_) -> TBase(TBottom,fun _ -> []), TBase(TBottom,fun _ -> [])
            | _ -> assert false
        in
        let t1' = abstract_term env cond pbs t1 typ' in
        let t2' = abstract_term env cond pbs t2 typ1 in
          if false then Format.printf "COERCE: %a  ===>  %a@." CEGAR_print.print_typ typ2 CEGAR_print.print_typ typ;
          coerce env cond pbs typ2 typ (App(t1',t2'))
    | Let(x,t1,t2) ->
        let typ' = get_typ env t1 in
        let t1' = abstract_term env cond pbs t1 typ' in
        let env' = (x,typ')::env in
        let pbs' = abst_arg x typ' in
        let t2' = abstract_term env' cond pbs' t2 typ in
          Let(x,t1',t2')


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
  let t2' = abstract_term env' [t1] pbs t2 typ in
  let t = assume env' [] pbs t1 t2' in
    [f, xs, Const True, t]




let rec make_arg_let = function
    Const c -> Const c
  | Var x -> Var x
  | App _ as t ->
      let t',ts = decomp_app t in
      let ts' = List.map make_arg_let ts in
      let xs = List.map (fun _ -> new_id "a") ts in
      let t'' = List.fold_left (fun t x -> App(t, Var x)) t' xs in
        List.fold_left2 (fun t' x t -> Let(x, t, t')) t'' xs ts'
  | Let _ -> assert false
let rec reduce_let = function
    Const c -> Const c
  | Var x -> Var x
  | App(t1,t2) -> App(reduce_let t1, reduce_let t2)
  | Let(x,(Var _ | Const _ as t1),t2) -> subst x t1 (reduce_let t2)
  | Let(x,t1,t2) -> Let(x, reduce_let t1, reduce_let t2)
let make_arg_let defs = List.map (apply_body_def (fun t -> reduce_let(make_arg_let t))) defs



let add_label_term = function
    App(App(App(Const If, t1), t2), t3) ->
      make_if t1 (App(Const (Label 0), t2)) (App(Const (Label 1), t3))
  | t -> t

let rec add_label prog =
  let aux (env,defs,main) =
    env, List.map (fun (f,xs,t1,t2) -> f,xs,t1,add_label_term t2) defs, main
  in
  let prog = to_if_exp prog in
  let prog = aux prog in
    of_if_exp prog
    



let abstract (env,defs,main) =
  let defs = make_arg_let defs in
  let () = if false then Format.printf "MAKE_ARG_LET:\n%a@." CEGAR_print.print_prog (env,defs,main) in
  let (env,defs,main) = add_label (env,defs,main) in
  let _ = Typing.infer (env,defs,main) in
  let defs = rev_flatten_map (abstract_def env) defs in
    [], defs, main



