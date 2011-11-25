open ExtList

type t = Base of Var.t * b * Term.t | Fun of (Var.t * t * t) list
and b = Unit | Bool | Int

let pr_base ppf bty =
  match bty with
    Unit ->
      Format.fprintf ppf "unit"
  | Bool ->
      Format.fprintf ppf "bool"
  | Int ->
      Format.fprintf ppf "int"

let is_base rty =
  match rty with
    Base(_, _, _) -> true
  | Fun(_) -> false

let rec pr ppf rty =
  match rty with
    Base(x, bty, t) ->
      (match t with
        Term.Const(_, Const.True) ->
          Format.fprintf ppf "%a" pr_base bty
      | _ ->
          Format.fprintf ppf "{@[<hov>%a:%a@ |@ %a@]}" Var.pr x pr_base bty Term.pr t)
  | Fun(xs) ->
      let pr_aux ppf (x, rty1, rty2) =
        let _ = if is_base rty1 then Format.fprintf ppf "@[<hov>%a:%a" Var.pr x pr rty1 else Format.fprintf ppf "@[<hov>%a:(%a)" Var.pr x(*???*) pr rty1 in
        Format.fprintf ppf "@ ->@ %a@]" pr rty2
      in
      let _ = assert (xs <> []) in
      Format.fprintf ppf "@[<hv>%a@]" (Util.pr_list pr_aux " /\\@ ") xs

let pr_bind ppf (f, sty) = Format.fprintf ppf "%a: %a" Var.pr f pr sty
let pr_env ppf env = Format.fprintf ppf "@[<v>%a@]" (Util.pr_list pr_bind "@ ") env

let rec of_simple_type ty =
  match ty with
    SimType.Unit -> Base(Var.new_var(), Unit, Term.ttrue)
  | SimType.Bool -> Base(Var.new_var(), Bool, Term.ttrue)
  | SimType.Int -> Base(Var.new_var(), Int, Term.ttrue)
  | SimType.Fun(ty1, ty2) ->
      Fun([Var.new_var(), of_simple_type ty1, of_simple_type ty2])

(*???*)
let rec of_interaction_type sty =
  match sty.IntType.shape with
    IntType.Unit(x) -> Base(x, Unit, sty.IntType.post)
  | IntType.Bool(x) -> Base(x, Bool, sty.IntType.post)
  | IntType.Int(x) -> Base(x, Int, sty.IntType.post)
  | IntType.Fun(xs) ->
      Fun
        (List.map
          (fun (ty1, ty2) ->
            (match ty1 with IntType.Unit(x) | IntType.Bool(x) | IntType.Int(x) -> x | _ -> Var.new_var()),
            of_interaction_type (IntType.make ty1 Term.ttrue sty.IntType.pre),
            of_interaction_type (IntType.make ty2 Term.ttrue sty.IntType.post))
          xs)

let rec subst sub rty =
  match rty with
    Base(x, bty, t) ->
      let sub y = if Var.equiv x y then raise Not_found else sub y in
      Base(x, bty, Term.subst sub t)
  | Fun(xs) ->
      Fun
        (List.map
		        (fun (x, rty1, rty2) ->
		          x,
		          subst sub rty1,
		          let sub y = if Var.equiv x y then raise Not_found else sub y in
		          subst sub rty2)
		        xs)

let set_base_cond rty t =
  match rty with
    Base(x, bty, _) ->
      Base(x, bty, t)
  | Fun(_) ->
      assert false

let env_of env sums fcs =
  let tlfcs =
    List.unique
		    (List.filter
		      (fun (x, _) -> Var.is_top x)
		      fcs)
  in
		let make_fun_shape x_tys ty =
		  List.fold_right
		    (fun (x, ty) retty -> Fun [x, ty, retty])
		    x_tys
		    ty
		in
		let merge_shapes shs =
			 match shs with
			   Fun(_)::_ ->
			       Fun
			         (Util.concat_map
			           (function (Fun(xs)) -> xs | _ -> assert false)
			           shs)
			 | [ty] ->
			     ty
			 | [] -> assert false (*Fun []*)
			 | _ ->
			     let _ = Format.printf ":%a:@." (Util.pr_list pr ":") shs in
			     assert false
		in
  let rec shape_of (x, uid) =
    match env x with
      SimType.Unit -> Base(x, Unit, Term.ttrue)
    | SimType.Bool -> Base(x, Bool, Term.ttrue)
    | SimType.Int -> Base(x, Int, Term.ttrue)
    | SimType.Fun(_, _) as ty ->
        let n = SimType.arity ty in
        (* ret is of base type *)
        let pres = Util.filter_map (function `Pre(x_uid, t) when x_uid = (x, uid) -> Some(t) | _ -> None) sums in
        let pre = if pres = [] then Term.ttrue(*???*) else Term.bor(*???*) pres in
        let posts = Util.filter_map (function `Post(x_uid, t) when x_uid = (x, uid) -> Some(t) | _ -> None) sums in
        let post = if posts = [] then Term.ttrue(*???*) else Term.band(*???*) posts in
        let ret = set_base_cond (merge_shapes (shapes_of (Var.T(x, uid, n)))) post in
        let j = SimType.find_last_base ty in
        let _ = if j = -1 then ()(*assert (Term.equiv pre Term.ttrue)*) in
								(*
								Format.printf "%a@." Var.pr (Var.T(x, uid, n));
								*)
        make_fun_shape
          (List.init n
            (fun i ->
														(*
														Format.printf "%a@." Var.pr (Var.T(x, uid, i));
														*)
              let y = Var.T(x, uid, i) in
              let rty = merge_shapes (shapes_of y) in
              y, if i = j then set_base_cond rty pre else rty))
          ret
    | _ ->
        let _ = Format.printf "%a:%d" Var.pr x uid in
        assert false
  and shapes_of x =
    match env x with
      SimType.Unit -> [Base(x, Unit, Term.ttrue)]
    | SimType.Bool -> [Base(x, Bool, Term.ttrue)]
    | SimType.Int -> [Base(x, Int, Term.ttrue)]
    | SimType.Fun(_, _) as ty ->
        let res =
		        List.map
		          (fun uid -> shape_of (x, uid))
		          (List.filter_map (fun (y, uid) -> if x = y then Some(uid) else None) fcs)
        in
        if res = [] then [of_simple_type ty] else res
  in
  List.map (fun x_uid -> x_uid, shape_of x_uid) tlfcs

let of_summaries prog sums fcs =
  List.map
    (fun (x_uid, rty) -> fst x_uid, rty)
    (env_of (Prog.type_of prog) sums fcs)
