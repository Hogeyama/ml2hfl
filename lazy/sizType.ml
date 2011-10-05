open ExtList
open ExtString

(* only argument side can have intersection types *)
type s =
  Unit of Var.t
| Bool of Var.t
| Int of Var.t
| Fun of (s * s) list
and t = { shape: s; pre: Term.t; post: Term.t }

let make shape pre post = { shape = shape; pre = pre; post = post }

let of_simple_type sty =
		let rec aux sty =
		  match sty with
		    SimType.Unit -> Unit(Var.make (Idnt.new_id ()))
		  | SimType.Bool -> Bool(Var.make (Idnt.new_id ()))
		  | SimType.Int -> Int(Var.make (Idnt.new_id ()))
		  | SimType.Fun(sty1, sty2) ->
		      Fun([aux sty1, aux sty2])
  in
  make (aux sty) Term.ttrue Term.ttrue

let make_fun_shape tys ty =
  let n = List.length tys in
  List.fold_right
    (fun ty retty -> Fun [ty, retty])
    tys
    ty

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
      let pr_aux ppf (ty1, ty2) =
        let _ = Format.fprintf ppf "@[<hv>" in
        let _ =
          if is_base ty1 then
            Format.fprintf ppf "%a " pr_shape ty1
          else
            Format.fprintf ppf "(%a) " pr_shape ty1
        in
        let _ = Format.fprintf ppf "->@ " in
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
  match sty.pre, sty.post with
    Term.Const(_, Const.True), Term.Const(_, Const.True) ->
      Format.fprintf ppf "%a" pr_shape sty.shape
  | _, _ ->
      Format.fprintf ppf "<@[<hov>%a |@ %a |@ %a@]>" Term.pr sty.pre pr_shape sty.shape Term.pr sty.post

let merge_shapes shs =
  match shs with
    Fun(_)::_ ->
        Fun
          (Util.concat_map
            (function (Fun(xs)) -> xs | _ -> assert false)
            shs)
  | [ty] ->
      ty
  | [] -> Fun [](*???*)
  | _ ->
      let _ = Format.printf ":%a:@." (Util.pr_list pr_shape ":") shs in
      assert false

let pr_bind ppf (f, sty) = Format.fprintf ppf "%a: %a" Var.pr f pr sty
let pr_env ppf env = Format.fprintf ppf "@[<v>%a@]" (Util.pr_list pr_bind "@ ") env

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
          (fun (ty1, ty2) ->
            rename_shape sub ty1,
            rename_shape sub ty2)
          xs)

let rename sub sty =
  let subst x = Term.make_var2 (sub x) in
  make
    (rename_shape sub sty.shape)
    (Term.subst subst sty.pre)
    (Term.subst subst sty.post)

let rec sub_from_tys new_var tys =
  if tys = [] then
    []
  else
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
		      let tys1, tys2 = List.split (List.map (fun (ty1, ty2) -> ty1, ty2) xs) in
		      let sub1 = sub_from_tys new_var tys1 in
		      let sub2 = sub_from_tys new_var tys2 in
		      sub1 @ sub2

let canonize sty =
  let new_var =
    let cnt = ref 0 in
    fun () -> cnt := !cnt + 1; Var.V("v" ^ (string_of_int !cnt))
  in
  let sub = sub_from_tys new_var [sty.shape] in
  rename (fun x -> List.assoc x sub) sty



let shape_env_of env fcs =
  let tlfcs =
    List.unique
		    (List.filter
		      (fun (x, _) -> Var.is_top x)
		      fcs)
  in
  let rec shape_of_x_uid (x, uid) =
    match env x with
      SimType.Unit -> Unit(x)
    | SimType.Bool -> Bool(x)
    | SimType.Int -> Int(x)
    | SimType.Fun(_, _) as ty ->
        let n = SimType.arity ty in
        let ret = merge_shapes (shape_of (Var.T(x, uid, n))) in
(*
Format.printf "%a@." Var.pr (Var.T(x, uid, n));
*)
        make_fun_shape
          (List.init n
            (fun i ->
              let args = shape_of (Var.T(x, uid, i)) in
(*
Format.printf "%a@." Var.pr (Var.T(x, uid, i));
*)
              merge_shapes args))
          ret
    | _ ->
        let _ = Format.printf "%a:%d" Var.pr x uid in
        assert false
  and shape_of x =
    match env x with
      SimType.Unit -> [Unit(x)]
    | SimType.Bool -> [Bool(x)]
    | SimType.Int -> [Int(x)]
    | SimType.Fun(_, _) as ty ->
        let res =
		        List.map
		          (fun uid -> shape_of_x_uid (x, uid))
		          (List.filter_map (fun (y, uid) -> if x = y then Some(uid) else None) fcs)
        in
        if res = [] then [(of_simple_type ty).shape] else res
  in
  List.map (fun x_uid -> x_uid, shape_of_x_uid x_uid) tlfcs

let of_summaries senv sums =
  List.map
    (fun (x_uid, sh) ->
      let pres = Util.filter_map (function `Pre(x_uid', t) when x_uid = x_uid' -> Some(t) | _ -> None) sums in
      let posts = Util.filter_map (function `Post(x_uid', t) when x_uid = x_uid' -> Some(t) | _ -> None) sums in
      fst x_uid, canonize (make sh (if pres = [] then (*???*)Term.ttrue else Term.band pres) (if posts = [] then (*???*)Term.ttrue else Term.bor posts)))
    senv


let check_prog env prog = assert false

(*
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


let alpha sty =
  let sub = sub_from_tys Var.new_var [sty.shape] in
  rename (fun x -> List.assoc x sub) sty.shape sty.cond


let merge stys =
  match stys with
    [sty] -> sty
  | _ ->
      let tys, posts =
        List.split
          (List.map
            (fun sty ->
              sty.shape, sty.cond)
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
  let ty, subs = canonize_ag true [[]] sty1.shape sty2.shape in
  let cond =
    Term.bor
      (List.map
        (fun sub0 ->
          let sub = List.map (fun (x, t, _) -> x, t) sub0 in
          Term.subst (fun x -> Term.make_var2 (List.assoc x sub)) sty1.cond)
        subs)
  in
  Term.band
    [subtype_same ty sty2.shape;
    Term.imply cond sty2.cond]

let subtype_ty sty1 ty2 =
  let ty1, subs = canonize_ag true [[]] sty1.shape ty2 in
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
  aux [] sty0.shape sty0.cond stys

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
    args_pre_ret sty.shape
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
    match sty_guard.shape with
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
*)
