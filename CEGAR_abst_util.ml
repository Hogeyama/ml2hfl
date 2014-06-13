
open Util
open CEGAR_syntax
open CEGAR_type
open CEGAR_print
open CEGAR_util


let debug = false

let hd = function
  | [] -> assert false
  | [x] -> x
  | _ -> assert false

let check_aux env cond p =
  let cond' = List.map FpatInterface.conv_formula cond in
  let p' = FpatInterface.conv_formula p in
  Fpat.SMTProver.implies_dyn cond' [p']

let check env cond pbs p =
  let ps,_ = List.split pbs in
  check_aux env (cond@@@ps) p

let equiv env cond t1 t2 =
  check_aux env (t1::cond) t2 && check_aux env (t2::cond) t1

let make_conj pbs =
  match pbs with
  | [] -> Const True
  | (_,b)::pbs -> List.fold_left (fun t (_,b) -> make_and t b) b pbs

let make_dnf pbss =
  match pbss with
  | [] -> Const False
  | pbs::pbss' -> List.fold_left (fun t pbs -> make_or t (make_conj pbs)) (make_conj pbs) pbss'


let weakest_aux env cond ds p =
  let fvp = get_fv p in
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
    if !Flag.use_neg_pred
    then List.mapi (fun i _ -> [i+1]) ds @ List.mapi (fun i _ -> [-i-1]) ds
    else List.mapi (fun i _ -> [i+1]) ds
  in
  let rec loop xs' nxs' ys' pbss =
    let xs, qs =
      List.partition
        (fun pbs ->
         let pbs = f pbs in
         let fvs = List.flatten @@ List.map (fun (p, _) -> get_fv p) pbs in
         if inter fvp fvs = [] && inter (List.rev_map_flatten get_fv cond) fvs = [] then
           false
         else
           check env cond pbs p)
        pbss
    in
    let nxs, ys =
      List.partition
        (fun pbs ->
         let pbs' = f pbs in
         let fvs = List.flatten @@ List.map (fun (p, _) -> get_fv p) pbs' in
         if inter fvp fvs = [] && inter (List.rev_map_flatten get_fv cond) fvs = [] then
           false
         else
           check env cond pbs' (make_not p))
        pbss
    in
    let xs = List.unique (xs' @ xs) in
    let nxs = List.unique (nxs' @ nxs) in
    let ys = List.unique (ys' @ ys) in
    let ws =
      List.unique @@
        List.flatten @@
          List.map (fun y1 -> List.map (fun y2 -> List.sort (List.unique (y1 @ y2))) ys) ys
    in
    let ws =
      let ok w =
        if List.length w > !Flag.wp_max_num then
          false
        else
          let rec ok w =
            match w with
            | [] -> true
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
    let ws =
      List.filter
        (fun w -> not (List.exists (fun x -> diff x w = []) xs) &&
                  not (List.exists (fun x -> diff x w = []) nxs)) ws in
    let ws =
      let rec aux xs =
        match xs with
        | [] -> []
        | x::xs' ->
            if List.exists (fun y -> diff y x = []) xs' then aux xs' else x::(aux xs')
      in
      aux ws
    in
    if ws = [] then xs, nxs else loop xs nxs ys ws
  in
  let xs, nxs = loop [] [] [] pbss in
  let pbss = List.map f xs in
  let npbss = List.map f nxs in
  pbss, npbss


let weakest env cond ds p =
  if check env cond [] p then (*???*)
    Const True, Const False
  else if check env cond [] (make_not p) then (*???*)
    Const False, Const True
  else
    let fvp = get_fv p in
    let ts = cond @@@ List.map fst ds in
    let ds =
      let rec fixp xs =
        let aux p =
          let fv = get_fv p in
          if inter fv xs = []
          then []
          else fv
        in
        let xs' = List.unique (xs @ (List.flatten @@ List.map aux ts)) in
        if List.length xs = List.length xs'
        then xs
        else fixp xs'
      in
      let fv = fixp fvp in
      List.filter (fun (p, _) -> subset (get_fv p) fv) ds
    in
    let pbss,npbss = weakest_aux env cond ds p in
    let pbss = List.filter (fun pbs -> not @@ check env cond pbs (Const False)) pbss in
    let npbss = List.filter (fun pbs -> not @@ check env cond pbs (Const False)) npbss in
    make_dnf pbss, make_dnf npbss



let filter env cond pbs must t =
  let pbss,_ = weakest_aux env cond pbs (Const False) in
  make_if (make_dnf pbss) (Const Bottom) t



let print_pb fm (p,b) =
  Format.fprintf fm "%a := %a" CEGAR_print.term b CEGAR_print.term p

let print_pbs fm pbs =
  print_list print_pb ";@\n" fm pbs


let abst env cond pbs p =
  if debug then Format.printf "pbs: @[<hv>%a@]@.p:%a@." print_pbs pbs CEGAR_print.term p;
  if has_bottom p
  then Const Bottom
  else
    let tt, ff = weakest env cond pbs p in
    if debug then Format.printf "tt:%a@.ff:%a@.@." CEGAR_print.term tt CEGAR_print.term ff;
    if make_not tt = ff || tt = make_not ff
    then tt
    else make_if tt (Const True) (make_if ff (Const False) (make_br (Const True) (Const False)))


let assume env cond pbs t1 t2 =
  if t1 = Const True
  then t2
  else
    let ff = snd (weakest env cond pbs t1) in
      make_if ff (Const Bottom) t2


let rec congruent env cond typ1 typ2 =
  match typ1,typ2 with
      TBase(b1,ps1), TBase(b2,ps2) when b1=b2 ->
        let x = new_id "x_abst" in
        let env' = (x,typ1)::env in
        let ps1' = ps1 (Var x) in
        let ps2' = ps2 (Var x) in
          List.length ps1' = List.length ps2' && List.for_all2 (equiv env' cond) ps1' ps2'
    | TFun(typ11,typ12), TFun(typ21,typ22) ->
        let x = new_id "x_abst" in
        let typ12 = typ12 (Var x) in
        let typ22 = typ22 (Var x) in
        let env' = (x,typ11)::env in
          congruent env cond typ11 typ21 && congruent env' cond typ12 typ22
    | _ -> Format.printf "CONGRUENT: %a,%a@." CEGAR_print.typ typ1 CEGAR_print.typ typ2; assert false


let decomp_tbase = function
    TBase(b, ps) -> b, ps
  | _ -> raise (Invalid_argument "CEGAR_abst_util.decomp_tbase")

let rec is_base_term env = function
    Const (Unit | True | False | Int _ | RandInt | Char _ | String _ | Float _ | Int32 _ | Int64 _ | Nativeint _ | RandVal _) -> true
  | Const _ -> false
  | Var x ->
      let typ =
        try
          List.assoc x env
        with Not_found -> Format.printf "Not found: %s@." x; assert false
      in
        begin
          match typ with
              TBase _ -> true
            | _ -> false
        end
  | App(App(Const (And|Or|Lt|Gt|Leq|Geq|EqUnit|EqInt|EqBool|CmpPoly _|Add|Sub|Mul),t1),t2) ->
      assert (is_base_term env t1);
      assert (is_base_term env t2);
      true
  | App(Const Not,t) -> is_base_term env t
  | App _ -> false
  | Let _ -> false
  | Fun _ -> false



let rec make_arg_let_term = function
    Const c -> [], Const c
  | Var x -> [], Var x
  | App _ as t ->
      let t',ts = decomp_app t in
      let aux t (bind,ts) =
        let bind',t' = make_arg_let_term t in
          bind'@bind, t'::ts
      in
      let bind,ts' = List.fold_right aux ts ([],[]) in
      let xs = List.map (fun _ -> new_id "a") ts in
      let t'' = List.fold_left (fun t x -> App(t, Var x)) t' xs in
        bind @ List.combine xs ts', t''
  | Let _ -> assert false
  | Fun _ -> assert false
let make_arg_let_term t =
  let bind,t' = make_arg_let_term t in
    List.fold_right (fun (x,t) t' -> Let(x, t, t')) bind t'

let rec reduce_let env = function
  | Const c -> Const c
  | Var x -> Var x
  | App(t1,t2) -> App(reduce_let env t1, reduce_let env t2)
  | Fun _ -> assert false
  | Let(x,t1,t2) ->
      match t1,get_typ env t1 with
      | Var _, _
      | Const _, _
      | _, TFun _ -> reduce_let env (subst x t1 t2)
      | _, (TBase _ as typ) -> Let(x, reduce_let env t1, reduce_let ((x,typ)::env) t2)
      | _ -> assert false

let make_arg_let_def env (f,xs,t1,e,t2) =
    f, xs, t1, e, reduce_let (get_arg_env (List.assoc f env) xs @@@ env) (make_arg_let_term t2)

let make_arg_let prog =
  {prog with defs = List.map (make_arg_let_def prog.env) prog.defs}



let rec add_label {env=env;defs=defs;main=main} =
  let merge = function
    | [f,xs,t1,e,t2] -> assert (t1 = Const True); [f, xs, t1, e, t2]
    | [f1,xs1,t11,e1,t12; f2,xs2,t21,e2,t22] when f1=f2 && xs1=xs2 && t11=make_not t21 ->
        [f1,xs1,t11,e1, make_label 1 t12; f2,xs2,t21,e2,make_label 0 t22]
    | [f1,xs1,t11,e1,t12; f2,xs2,t21,e2,t22] when f1=f2 && xs1=xs2 && make_not t11=t21 ->
        [f1,xs1,t11,e1,make_label 0 t12; f2,xs2,t21,e2,make_label 1 t22]
    | [f1,xs1,t11,e1,t12; f2,xs2,t21,e2,t22] ->
        CEGAR_print.prog Format.std_formatter{env=[];defs=[f1,xs1,t11,e1,t12; f2,xs2,t21,e2,t22];main=""};assert false
    | defs -> raise (Fatal ("Not implemented (CEGAR_abst_util.add_label) " ^ string_of_int (List.length defs)))
  in
  let rec aux = function
    | [] -> []
    | (f,xs,t1,e,t2)::defs ->
        let defs1,defs2 = List.partition (fun (g,_,_,_,_) -> f = g) defs in
        let defs' = merge ((f,xs,t1,e,t2)::defs1) in
        defs' @ aux defs2
  in
  let defs' = aux defs in
  let labeled = List.unique @@ List.rev_flatten_map (function (f,_,_,_,App(Const (Label _),_)) -> [f] | _ -> []) defs' in
  labeled, {env=env;defs=defs';main=main}



let rec use_arg x typ t =
  match typ with
  | TBase _ -> t
  | TFun(typ1,typ2) ->
      let u = new_id "u" in
      let t' = make_br (Const Unit) (App(Var x, make_ext_fun typ1)) in
      App(Fun(u, None, t), t')
  | _ -> assert false

and make_ext_fun = function
  | TBase(TUnit, _) -> Const Unit
  | TBase(TBool, _) -> make_br (Const True) (Const False)
  | TFun(typ1,typ2) ->
      let x = new_id "x" in
      Fun(x, None, use_arg x typ1 (make_ext_fun (typ2 (Const Unit))))
  | _ -> assert false


let add_ext_funs prog =
  let env = get_ext_fun_env prog in
  let defs = List.map (fun (f,typ) -> f, [], Const True, [], make_ext_fun typ) env in
  let defs' = defs@prog.defs in
  let _ = Typing.infer {env=[]; defs=defs'; main=prog.main} in
  {prog with defs=defs'}
