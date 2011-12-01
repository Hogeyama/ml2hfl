open ExtList

type t = Base of Var.t * b * Term.t | Fun of (t * t) list
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

let bv_of rty =
  match rty with
    Base(bv, _, _) -> bv
  | Fun(_) -> assert false

let rec pr ppf rty =
  match rty with
    Base(x, bty, t) ->
      (match t with
        Term.Const(_, Const.True) ->
          Format.fprintf ppf "%a:%a" Var.pr x pr_base bty
      | _ ->
          Format.fprintf ppf "%a:%a{@[<hov>%a@]}" Var.pr x pr_base bty Term.pr t)
  | Fun(xs) ->
      let pr_aux ppf (rty1, rty2) =
        let _ =
          if is_base rty1 then
            Format.fprintf ppf "@[<hov>%a" pr rty1
          else
            Format.fprintf ppf "@[<hov>(%a)" pr rty1
        in
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
      Fun([of_simple_type ty1, of_simple_type ty2])

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
            of_interaction_type (IntType.make ty1 Term.ttrue sty.IntType.pre),
            of_interaction_type (IntType.make ty2 Term.ttrue sty.IntType.post))
          xs)

let rec subst sub rty =
  match rty with
    Base(x, bty, t) ->
      let sub' y = if Var.equiv x y then raise Not_found else sub y in
      Base(x, bty, Term.subst sub' t)
  | Fun(xs) ->
      Fun
        (List.map
		        (fun (rty1, rty2) ->
		          subst sub rty1,
		          let sub' y = if is_base rty1 && Var.equiv (bv_of rty1) y then raise Not_found else sub y in
		          subst sub' rty2)
		        xs)

let set_base_cond rty t =
  match rty with
    Base(x, bty, _) ->
      Base(x, bty, t)
  | Fun(_) ->
      assert false






let rename sub sty =
  let subst x = Term.make_var2 (sub x) in
		let rec aux sty =
		  match sty with
		    Base(x, bty, t) ->
        Base((try sub x with Not_found -> x), bty, Term.subst subst t)
		  | Fun(xs) ->
		      Fun
		        (List.map
		          (fun (sty1, sty2) ->
		            aux sty1,
		            aux sty2)
		          xs)
  in
  aux sty

let rec sub_from new_var ty =
		match ty with
		  Base(x, bty, t) ->
		    let y = new_var () in
		    [x, y]
		| Fun(xs) ->
		    Util.concat_map 
        (fun (sty1, sty2) ->
          let sub1 = sub_from new_var sty1 in
          let sub2 = sub_from new_var sty2 in
          sub1 @ sub2)
        xs

let canonize sty =
  let new_var =
    let cnt = ref 0 in
    fun () -> cnt := !cnt + 1; Var.V("v" ^ (string_of_int !cnt))
  in
  let sub = sub_from new_var sty in
  rename (fun x -> List.assoc x sub) sty







let env_of env sums fcs =
  let tlfcs =
    List.unique
		    (List.filter
		      (fun (x, _) -> Var.is_top x)
		      fcs)
  in
		let make_fun_shape tys ty =
		  List.fold_right
		    (fun ty retty -> Fun [ty, retty])
		    tys
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
      SimType.Unit | SimType.Bool | SimType.Int ->
        (* pred disc assume that top level functions are with a function type but MoCHi violates *)
        Base(x, (match env x with SimType.Unit -> Unit | SimType.Bool -> Bool | SimType.Int -> Int), Term.ttrue)
(*
        let _ = Format.printf "%a:%d" Var.pr x uid in
        assert false
*)
    | SimType.Fun(_, _) as ty ->
        let n = SimType.arity ty in
        (* ret is of base type *)
        let ret = merge_shapes (shapes_of (Var.T(x, uid, n))) in
								(*
								Format.printf "%a@." Var.pr (Var.T(x, uid, n));
								*)
        make_fun_shape
          (List.init n
            (fun i ->
														(*
														Format.printf "%a@." Var.pr (Var.T(x, uid, i));
														*)
              merge_shapes (shapes_of (Var.T(x, uid, i)))))
          ret
    | _ ->
        let _ = Format.printf "%a:%d" Var.pr x uid in
        assert false
  and shapes_of x =
    match env x with
      SimType.Unit | SimType.Bool | SimType.Int ->
        let ps = Util.filter_map (function `P(y, t) when x = y -> Some(t) | _ -> None) sums in
        let p = (*???*)if ps = [] then Term.ttrue else Term.band ps in
        [Base(x, (match env x with SimType.Unit -> Unit | SimType.Bool -> Bool | SimType.Int -> Int), p)]
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
    (fun (x_uid, rty) -> fst x_uid, canonize rty)
    (env_of (Prog.type_of prog) sums fcs)
