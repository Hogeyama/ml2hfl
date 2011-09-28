open ExtList
open ExtString

(* for any Fun(t1, pre, t2), pre is true if t2 is not a base value *)
(* only argument side can have intersection types *)
type s =
  Unit of Var.t
| Bool of Var.t
| Int of Var.t
| Fun of (s * Term.t * s) list
and t = { ty: s; cond: Term.t }

let make ty cond = { ty = ty; cond = cond }

let is_base ty =
  match ty with
    Unit(_) | Bool(_) | Int(_) -> true
  | Fun(_) -> false

let rec pr_shape ppf ty =
  match ty with
    Unit(x) ->
      Format.fprintf ppf "unit[%a]" Var.pr x
  | Bool(x) ->
      Format.fprintf ppf "bool[%a]" Var.pr x
  | Int(x) ->
      Format.fprintf ppf "int[%a]" Var.pr x
  | Fun(xs) ->
      let pr_aux ppf (ty1, pre, ty2) =
        let _ = Format.fprintf ppf "@[<hv>" in
        let _ =
          if is_base ty1 then
            Format.fprintf ppf "%a " pr_shape ty1
          else
            Format.fprintf ppf "(%a) " pr_shape ty1
        in
        let _ =
          match pre with
            Term.Const(_, Const.True) ->
              Format.fprintf ppf "->@ "
          | _ ->
              Format.fprintf ppf "-{%a}->@ "
                Term.pr pre
        in
        Format.fprintf ppf "%a@]" pr_shape ty2
      in
      let _ =
        match xs with
          [] ->
            Format.fprintf ppf "T"
        | _ ->
            Format.fprintf ppf "@[<hv>%a@]" (Util.pr_list pr_aux " /\\@ ") xs
      in
      ()

let rec pr ppf sty =
  match sty.cond with
    Term.Const(_, Const.True) ->
      Format.fprintf ppf "%a" pr_shape sty.ty
  | _ ->
      Format.fprintf ppf "@[<hov>{%a |@ %a}@]" pr_shape sty.ty Term.pr sty.cond

let pr_fun_bind ppf (f, sty) = Format.fprintf ppf "%a: %a" Var.pr f pr sty
let pr_fun_env ppf env = Format.fprintf ppf "@[<v>%a@]" (Util.pr_list pr_fun_bind "@ ") env
let pr_var_bind ppf (x, ty) = Format.fprintf ppf "%a: %a" Var.pr x pr_shape ty
let pr_var_env ppf env = Format.fprintf ppf "@[<v>%a@]" (Util.pr_list pr_var_bind "@ ") env
  
let rec envs ty =
  match ty with
    Unit(x) ->
      [x, SimType.Unit]
  | Bool(x) ->
      [x, SimType.Bool]
  | Int(x) ->
      [x, SimType.Int]
  | Fun(xs) ->
      Util.concat_map
        (fun (ty1, _, ty2) -> envs ty1 @ envs ty2)
        xs
  
let rec rename_shape sub ty =
  match ty with
    Unit(x) ->
      Unit(try sub x with Not_found -> x)
  | Bool(x) ->
      Bool(try sub x with Not_found -> x)
  | Int(x) ->
      Int(try sub x with Not_found -> x)
  | Fun(xs) ->
      Fun
        (List.map
          (fun (ty1, pre, ty2) ->
            rename_shape sub ty1,
            Term.subst
              (fun x -> Term.make_var2 (sub x))
              pre,
            rename_shape sub ty2)
          xs)

let rename sub ty cond =
  { ty = rename_shape sub ty;
    cond =
      Term.subst
        (fun x -> Term.make_var2 (sub x))
        cond }

let rec sub_from_tys new_var tys =
  match List.hd tys with
    Unit(_) ->
      let y = new_var () in
      List.map (function Unit(x) -> x, y | _ -> assert false) tys
  | Bool(_) ->
      let y = new_var () in
      List.map (function Bool(x) -> x, y | _ -> assert false) tys
  | Int(_) ->
      let y = new_var () in
      List.map (function Int(x)  -> x, y | _ -> assert false) tys
  | Fun(xs) ->
      let tys1, tys2 = List.split (List.map (fun (ty1, _, ty2) -> ty1, ty2) xs) in
      let sub1 = sub_from_tys new_var tys1 in
      let sub2 = sub_from_tys new_var tys2 in
      sub1 @ sub2

let canonize sty =
  let new_var =
    let cnt = ref 0 in
    fun () -> cnt := !cnt + 1; Var.V("v" ^ (string_of_int !cnt))
  in
  let sub = sub_from_tys new_var [sty.ty] in
  rename (fun x -> List.assoc x sub) sty.ty sty.cond

let alpha sty =
  let sub = sub_from_tys Var.new_var [sty.ty] in
  rename (fun x -> List.assoc x sub) sty.ty sty.cond

let of_simple_type sty =
		let rec aux sty =
		  match sty with
		    SimType.Unit -> Unit(Var.make (Idnt.new_id ()))
		  | SimType.Bool -> Bool(Var.make (Idnt.new_id ()))
		  | SimType.Int -> Int(Var.make (Idnt.new_id ()))
		  | SimType.Fun(sty1, sty2) ->
		      Fun([aux sty1, Term.ttrue, aux sty2])
  in
  make (aux sty) Term.ttrue

let merge_tys tys =
  match tys with
    Fun(_)::_ ->
        Fun
          (Util.concat_map
            (function (Fun(xs)) -> xs | _ -> assert false)
            tys)
  | [ty] ->
      ty
  | _ -> assert false

let merge stys =
  match stys with
    [sty] -> sty
  | _ ->
      let tys, posts =
        List.split
          (List.map
            (fun sty ->
              sty.ty, sty.cond)
            stys)
      in
      let post = Term.band posts in
      match List.hd tys with
        Fun(_) ->
          make
            (Fun
              (Util.concat_map
                (function (Fun(xs)) -> xs | _ -> assert false)
                tys))
            post
      | _ ->
          let sub = sub_from_tys Var.new_var tys in
          make
            (rename_shape (fun x -> List.assoc x sub) (List.hd tys))
            (Term.subst (fun x -> Term.make_var2 (List.assoc x sub)) post)

let merge_env env =
  List.map
    (fun f_stys -> fst (List.hd f_stys), merge (List.map snd f_stys))
    (Util.classify (fun (x, _) (y, _) -> Var.equiv x y) env)

let make_fun tys pre ty =
  let n = List.length tys in
  List.fold_right
    (fun (i, ty) retty ->
      Fun
        [ty,
        (if i = n - 1 then pre else Term.ttrue),
        retty])
    (List.mapi (fun i ty -> i, ty) tys)
    ty

(* fcs is set *)
let of_summaries env fcs sums =
  let rec type_of x =
    match env x with
      SimType.Unit -> [Unit(x), Term.ttrue]
    | SimType.Bool -> [Bool(x), Term.ttrue]
    | SimType.Int -> [Int(x), Term.ttrue]
    | SimType.Fun(_, _) as ty ->
        let n = SimType.arity ty in
        List.map
          (fun uid ->
            let pres =
              List.unique
	               (Util.filter_map
	                 (function
	                   `Pre(x_uid, pre) ->
	                     if x_uid = (x, uid) then Some(pre) else None
	                 | _ -> None)
	                 sums)
            in
            let pre =
              if Var.is_top x then
                Term.band pres
              else if Var.is_neg x then
                if pres = [] then Term.ttrue else Term.bor pres
              else
(*
                let _ = Format.printf "%a:%a@." Var.pr x (Util.pr_list Term.pr ",") pres in
*)
                let _ = assert (pres = []) in
                Term.ttrue
            in
            let posts =
              List.unique
                (Util.filter_map
                  (function
                    `Post(x_uid, post) ->
                      if x_uid = (x, uid) then Some(post) else None
                  | _ -> None)
                  sums)
            in
            let post =
              if Var.is_top x then
                if posts = [] then Term.ttrue else Term.bor posts
              else
                let _ = assert (posts = []) in
                Term.ttrue
              in

            let [retty, retcond] = type_of (Var.T(x, uid, n)) in
            let _ = assert (retcond = Term.ttrue) in

            make_fun
              (List.init n
                (fun i ->
                  let argtys, argconds = List.split (type_of (Var.T(x, uid, i))) in
                  let argty = merge_tys argtys in
                  let _ = assert (CsisatInterface.iff (Term.band argconds) Term.ttrue) in
                  argty))
              pre
              retty,
            post)
          (List.filter_map (fun (y, uid) -> if x = y then Some(uid) else None) fcs)
  in

  let top_level_functions = List.unique
    (List.filter_map
      (fun (x, _) -> if Var.is_top x then Some(x) else None)
      fcs)
  in
  List.unique
    (Util.concat_map
      (fun f ->
        List.map
          (fun (ty, cond) ->
            f, canonize (make ty cond))
          (type_of f))
      top_level_functions)

let type_of_const c =
  (*
  Format.printf "%a@ " Const.pr c;
    *)
  match c with
    Const.Event(id) when id = Ctree.event_fail ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
		    make
		      (Fun([Unit(x1), Term.tfalse, Unit(x2)]))
		      Term.ttrue
  | Const.Event(_) ->
      assert false
  | Const.Unit ->
      let x = Var.new_var () in
      make (Unit(x)) (Term.eqUnit (Term.make_var2 x) (Term.tunit))
  | Const.True ->
      let x = Var.new_var () in
      make (Bool(x)) (Term.iff (Term.make_var2 x) (Term.ttrue))
  | Const.False ->
      let x = Var.new_var () in
      make (Bool(x)) (Term.iff (Term.make_var2 x) (Term.tfalse))
  | Const.And ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (Fun([Bool(x1), Term.ttrue, Fun([Bool(x2), Term.ttrue, Bool(x3)])]))
        (Term.iff (Term.make_var2 x3) (Term.band [Term.make_var2 x1; Term.make_var2 x2]))
  | Const.Or ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (Fun([Bool(x1), Term.ttrue, Fun([Bool(x2), Term.ttrue, Bool(x3)])]))
        (Term.iff (Term.make_var2 x3) (Term.bor [Term.make_var2 x1; Term.make_var2 x2]))
  | Const.Imply ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (Fun([Bool(x1), Term.ttrue, Fun([Bool(x2), Term.ttrue, Bool(x3)])]))
        (Term.iff (Term.make_var2 x3) (Term.imply (Term.make_var2 x1) (Term.make_var2 x2)))
  | Const.Iff ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (Fun([Bool(x1), Term.ttrue, Fun([Bool(x2), Term.ttrue, Bool(x3)])]))
        (Term.iff (Term.make_var2 x3) (Term.iff (Term.make_var2 x1) (Term.make_var2 x2)))
  | Const.Not ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      make
        (Fun([Bool(x1), Term.ttrue, Bool(x2)]))
        (Term.iff (Term.make_var2 x2) (Term.bnot (Term.make_var2 x1)))
  | Const.Lt ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (Fun([Int(x1), Term.ttrue, Fun([Int(x2), Term.ttrue, Bool(x3)])]))
        (Term.iff (Term.make_var2 x3) (Term.lt (Term.make_var2 x1) (Term.make_var2 x2)))
  | Const.Gt ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (Fun([Int(x1), Term.ttrue, Fun([Int(x2), Term.ttrue, Bool(x3)])]))
        (Term.iff (Term.make_var2 x3) (Term.gt (Term.make_var2 x1) (Term.make_var2 x2)))
  | Const.Leq ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (Fun([Int(x1), Term.ttrue, Fun([Int(x2), Term.ttrue, Bool(x3)])]))
        (Term.iff (Term.make_var2 x3) (Term.leq (Term.make_var2 x1) (Term.make_var2 x2)))
  | Const.Geq ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (Fun([Int(x1), Term.ttrue, Fun([Int(x2), Term.ttrue, Bool(x3)])]))
        (Term.iff (Term.make_var2 x3) (Term.geq (Term.make_var2 x1) (Term.make_var2 x2)))
  | Const.EqBool ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (Fun([Bool(x1), Term.ttrue, Fun([Bool(x2), Term.ttrue, Bool(x3)])]))
        (Term.iff (Term.make_var2 x3) (Term.eqBool (Term.make_var2 x1) (Term.make_var2 x2)))
  | Const.NeqBool ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (Fun([Bool(x1), Term.ttrue, Fun([Bool(x2), Term.ttrue, Bool(x3)])]))
        (Term.iff (Term.make_var2 x3) (Term.neqBool (Term.make_var2 x1) (Term.make_var2 x2)))
  | Const.EqUnit ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (Fun([Unit(x1), Term.ttrue, Fun([Unit(x2), Term.ttrue, Bool(x3)])]))
        (Term.iff (Term.make_var2 x3) (Term.eqUnit (Term.make_var2 x1) (Term.make_var2 x2)))
  | Const.NeqUnit ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (Fun([Unit(x1), Term.ttrue, Fun([Unit(x2), Term.ttrue, Bool(x3)])]))
        (Term.iff (Term.make_var2 x3) (Term.neqUnit (Term.make_var2 x1) (Term.make_var2 x2)))
  | Const.EqInt ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (Fun([Int(x1), Term.ttrue, Fun([Int(x2), Term.ttrue, Bool(x3)])]))
        (Term.iff (Term.make_var2 x3) (Term.eqInt (Term.make_var2 x1) (Term.make_var2 x2)))
  | Const.NeqInt ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (Fun([Int(x1), Term.ttrue, Fun([Int(x2), Term.ttrue, Bool(x3)])]))
        (Term.iff (Term.make_var2 x3) (Term.neqInt (Term.make_var2 x1) (Term.make_var2 x2)))
  | Const.Int(n) ->
      let x = Var.new_var () in
      make (Int(x)) (Term.eqInt (Term.make_var2 x) (Term.tint n))
  | Const.RandInt ->
      let x = Var.new_var () in
      make (Int(x)) Term.ttrue
(*
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      make
        (Fun([Unit(x1), Term.ttrue, Int(x2)]))
        Term.ttrue
*)
  | Const.Add ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (Fun([Int(x1), Term.ttrue, Fun([Int(x2), Term.ttrue, Int(x3)])]))
        (Term.eqInt (Term.make_var2 x3) (Term.add (Term.make_var2 x1) (Term.make_var2 x2)))
  | Const.Sub ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (Fun([Int(x1), Term.ttrue, Fun([Int(x2), Term.ttrue, Int(x3)])]))
        (Term.eqInt (Term.make_var2 x3) (Term.sub (Term.make_var2 x1) (Term.make_var2 x2)))
  | Const.Mul ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (Fun([Int(x1), Term.ttrue, Fun([Int(x2), Term.ttrue, Int(x3)])]))
        (Term.eqInt (Term.make_var2 x3) (Term.mul (Term.make_var2 x1) (Term.make_var2 x2)))
  | Const.Minus ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      make
        (Fun([Int(x1), Term.ttrue, Int(x2)]))
        (Term.eqInt (Term.make_var2 x2) (Term.minus (Term.make_var2 x1)))
    
let rec subtype_same ty1 ty2 =
  match ty1, ty2 with
    Unit(x), Unit(y)
  | Bool(x), Bool(y)
  | Int(x), Int(y) when Var.equiv x y ->
    Term.ttrue
  | Fun(xs), Fun(ys) ->
      let _ = assert (List.length xs = List.length ys) in
      Term.band
        (try
		        List.map2
		          (fun (ty1, pre, ty2) (ty1', pre', ty2') ->
		            Term.band
		              [subtype_same ty1' ty1;
		              Term.imply pre' (Term.band [pre; subtype_same ty2 ty2'])])
		          xs
		          ys
				    with List.Different_list_size(_) ->
				      assert false)
  | _, _ ->
      assert false

let rec merge_same cov tys =
  match List.hd tys with
    Fun(_) ->
      let xss = Util.transpose_list (List.map (function (Fun(xs)) -> xs | _ -> assert false) tys) in
      Fun
        (List.map
          (fun xs ->
            merge_same (not cov) (List.map (fun (ty, _, _) -> ty) xs),
            (if cov then Term.bor else Term.band) (List.map (fun (_, pre, _) -> pre) xs),
            merge_same cov (List.map (fun (_, _, ty) -> ty) xs))
          xss)
  | _ ->
      (*any element in tys is the same*)
      List.hd tys
    
let rec canonize_fun cov subs0 (ty1, pre, ty1') (ty2, _, ty2') =
  let ty', subs' = canonize_ag cov subs0 ty1' ty2' in
  let ty, subs = canonize_ag (not cov) (Util.multiply_list subs0 subs') ty1 ty2 in
  let pres =
    List.map
      (fun sub0 ->
         let sub = List.map (fun (x, t, _) -> x, t) sub0 in
         Term.subst (fun x -> Term.make_var2 (List.assoc x sub)) pre)
      (Util.multiply_list subs0 subs)
  in
  let pre = if cov then Term.bor pres else Term.band pres in
  (ty, pre, ty'),
  Util.multiply_list subs subs'
and canonize_ag cov subs0 ty1 ty2 =
  match ty1, ty2 with
    Unit(x), Unit(y) ->
      ty2, [[x, y, SimType.Unit]]
  | Bool(x), Bool(y) ->
      ty2, [[x, y, SimType.Bool]]
  | Int(x), Int(y) ->
      ty2, [[x, y, SimType.Int]]
  | Fun(xs), Fun(ys) ->
      let tmp = List.init (List.length ys) (fun i -> i) in
      let tmp = List.map (fun _ -> tmp) xs in
      let nss = Util.multiply_list_list (List.map (fun x -> [x]) tmp) in
      let nzss, subss =
        List.split
          (List.map
            (fun ns ->
              let zs, subss =
                List.split
                  (try
		                  List.map2
		                    (fun n x ->
		                      let y = List.nth ys n in
		                      canonize_fun cov subs0 x y)
		                    ns
		                    xs
														    with List.Different_list_size(_) ->
														      assert false)
              in
              let subs = List.concat subss in
              List.combine ns zs, subs)
            nss)
      in
      let nzs = List.concat nzss in
      let zs =
        List.mapi
          (fun i _ ->
            let zs =
              List.filter_map
                (fun (n, z) -> if n = i then Some(z) else None)
                nzs
            in
            let Fun([z]) = merge_same cov (List.map (fun z -> Fun([z])) zs) in
            z)
          ys
      in
      let subs = List.concat subss in 
      Fun(zs), subs
  | _, _ ->
      let _ = Format.printf "%a, %a" pr_shape ty1 pr_shape ty2 in
      assert false
  
let subtype sty1 sty2 =
  let ty, subs = canonize_ag true [[]] sty1.ty sty2.ty in
  let cond =
    Term.bor
      (List.map
        (fun sub0 ->
          let sub = List.map (fun (x, t, _) -> x, t) sub0 in
          Term.subst (fun x -> Term.make_var2 (List.assoc x sub)) sty1.cond)
        subs)
  in
  Term.band
    [subtype_same ty sty2.ty;
    Term.imply cond sty2.cond]

let subtype_ty sty1 ty2 =
  let ty1, subs = canonize_ag true [[]] sty1.ty ty2 in
  Term.band
    (subtype_same ty1 ty2::
    (List.map
      (fun sub0 ->
        let sub = List.map (fun (x, t, _) -> x, t) sub0 in
        Term.subst (fun x -> Term.make_var2 (List.assoc x sub)) sty1.cond)
      subs))

let make_open ty =
  let sub = sub_from_tys Var.new_var [ty] in
  let ty' = rename_shape (fun x -> List.assoc x sub) ty in
  let ty', subs = canonize_ag true [[]] ty ty' in
  make
    ty'
    (Term.bor
      (List.map
        (fun sub ->
          Term.band
            (List.map
              (fun (x, y, ty) -> Term.eq_ty ty (Term.make_var2 y) (Term.make_var2 x))
              sub))
        subs))

let apply sty0 stys =
  (*
  let _ = Format.printf "%a %a@ " pr sty0 (Util.pr_list pr " ") stys in
    *)
  let rec aux conds_envs ty post stys =
    match stys with
      [] ->
        Term.ttrue, make ty (Term.forall_imply conds_envs post)
    | sty::stys ->
        match ty with
          Unit(_) | Bool(_) | Int(_) ->
            assert false
        | Fun(xs) ->
            let pres, stys =
              List.split
                (List.map
                  (fun (ty1, pre, ty2) ->
                    let conds_envs = (subtype_ty sty ty1, envs ty1)::conds_envs in
                    let pre', sty = aux conds_envs ty2 post stys in
                    if is_base ty2 then
                      let _ = assert (Term.equiv pre' Term.ttrue) in
                      Term.forall_imply conds_envs pre, sty
                    else
                      let _ = assert (Term.equiv pre Term.ttrue) in
                      pre', sty)
                  xs)
            in
            (*any element in tys is the same*)
            Term.bor pres, merge stys
  in
  aux [] sty0.ty sty0.cond stys

exception Ill_typed

let rec infer_term cenv env t =
  let f, args = Term.fun_args t in
  let sty_f =
    match f with
      Term.Var(_, x) ->
        (try
          List.assoc x cenv
        with Not_found ->
          make_open
            (try
              List.assoc x env
            with Not_found ->
              let _ = Format.printf "\"%a\" not found@ " Var.pr x in
              let _ = Format.printf "function type env: %a@ " pr_fun_env cenv in
              let _ = Format.printf "variable type env: %a@ " pr_var_env env in
              raise Ill_typed))
    | Term.Const(_, c) ->
        type_of_const c
    | Term.App(_, _, _) | Term.Call(_, _, _) | Term.Ret(_, _, _, _) | Term.Error(_) | Term.Forall(_, _, _) ->
        assert false
  in
  let vcs, stys = List.split (List.map (infer_term cenv env) args) in
  let vc, sty = apply (alpha sty_f) stys in
  (*
  let _ = Format.printf "sty_f: %a@ " pr sty_f in
  *)
  let vc = Term.band (vc::vcs) in
  let _ = Format.printf "@[<v>type inference of term: %a@   env: %a@   vc: %a@   type: %a@ @]" Term.pr t pr_var_env env Term.pr vc pr sty in
  vc, sty

let check_fdef cenv fdef sty =
  let ty_args, pre, ty_ret =
    let rec args_pre_ret ty =
      match ty with
        Fun([ty1, pre, ty2]) ->
          let args, pre', ret = args_pre_ret ty2 in
          let pre =
            if is_base ty2 then
              let _ = assert (Term.equiv pre' Term.ttrue) in
              pre
            else
              let _ = assert (Term.equiv pre Term.ttrue) in
              pre'
          in
          ty1::args, pre, ret
      | Unit(_) | Bool(_) | Int(_) ->
          [], Term.ttrue, ty
      | _ -> assert false
    in
    args_pre_ret sty.ty
  in
  assert (is_base ty_ret);
  let env =
    try
		    List.map2
		      (fun x ty -> Var.V(x), ty)
		      fdef.Fdef.args
		      ty_args
    with List.Different_list_size(_) ->
      assert false
  in
  let guard =
    let _(*ttrue*), sty_guard = infer_term cenv env fdef.Fdef.guard in
    match sty_guard.ty with
      Bool(x) -> Term.subst (fun y -> if Var.equiv x y then Term.ttrue else raise Not_found) sty_guard.cond
    | _ -> assert false
  in
  (*
  let _ = Format.printf "guard: %a@ " Term.pr guard in
    *)
  let cond = Term.band [pre; guard] in
  try
    let vc1, sty_ret = infer_term cenv env fdef.Fdef.body in
    let vc2 = subtype (*no need to take env?*) sty_ret (make ty_ret sty.cond) in
    let vc = Term.band [vc1; vc2] in
    let _ = Format.printf "@[<v>type checking@   cond: %a@   vc1: %a@   vc2: %a@ @]" Term.pr cond Term.pr vc1 Term.pr vc2 in
    Cvc3Interface.is_valid (Term.imply cond vc)
    (*CsisatInterface.implies cond vc*)
  with Ill_typed ->
    false

(* cand_env must not include a type with intersection refinement *)
let check_prog cand_env prog =
  let env =
    Util.fixed_point
      (fun env ->
        merge_env
          (List.filter
            (function (Var.V(id), sty) ->
              Format.printf "@[<v>checking if \"%a\" has %a:@   @]" Idnt.pr id pr sty;
              let fdefs = Prog.fdefs_of prog id in
              let res =
                List.for_all
                  (fun fdef -> check_fdef env fdef sty)
                  fdefs
              in
              let _ =
                if res then
                  Format.printf "well-typed@ "
                else
                  Format.printf "ill-typed@ "
              in
              res
            | _ -> assert false)
            cand_env))
      (fun env1 env2 ->
        (*
        Format.printf "%d, %d@ " (List.length env1) (List.length env2);
          *)
        List.length env1 = List.length env2)
      (merge_env cand_env)
  in
  List.exists
    (function (Var.V(id), sty) ->
      id = prog.Prog.main (*&& sty is a subtype of a given type*)
    | _ -> assert false)
    env
