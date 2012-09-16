open ExtList

(** Refinement types *)

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
    SimType.Unit -> Base(Var.new_var(), Unit, Formula.ttrue)
  | SimType.Bool -> Base(Var.new_var(), Bool, Formula.ttrue)
  | SimType.Int -> Base(Var.new_var(), Int, Formula.ttrue)
  | SimType.Fun(ty1, ty2) ->
      Fun([of_simple_type ty1, of_simple_type ty2])

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
  let subst x = Term.make_var (sub x) in
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

let rec fresh_names new_var ty =
  match ty with
    Base(x, bty, t) ->
      let y = new_var () in
      [x, y]
  | Fun(xs) ->
      Util.concat_map 
        (fun (sty1, sty2) ->
          let sub1 = fresh_names new_var sty1 in
          let sub2 = fresh_names new_var sty2 in
          sub1 @ sub2)
        xs

let canonize sty =
  let new_var =
    let cnt = ref 0 in
    fun () -> cnt := !cnt + 1; Var.V(Idnt.make ("v" ^ (string_of_int !cnt)))
  in
  let sub = fresh_names new_var sty in
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
        let _ = Format.printf ":%a:@," (Util.pr_list pr ":") shs in
        assert false
  in
  let conv_base = function SimType.Unit -> Unit | SimType.Bool -> Bool | SimType.Int -> Int | _ -> assert false in
  let rec shape_of (x, uid) =
    match env x with
      SimType.Unit | SimType.Bool | SimType.Int ->
        (* predicate discovery algo. assumes that each top level definition is with a function type but MoCHi violates the condition *)
        Base(x, conv_base (env x), Formula.ttrue)
(*
        let _ = Format.printf "%a:%d" Var.pr x uid in
        assert false
*)
    | SimType.Fun(_, _) as ty ->
        let n = SimType.arity ty in
        (* ret is of base type *)
        let ret = merge_shapes (shapes_of (Var.T(x, uid, n))) in
        (*
        Format.printf "%a@," Var.pr (Var.T(x, uid, n));
        *)
        make_fun_shape
          (List.init n
            (fun i ->
              (*
              Format.printf "%a@," Var.pr (Var.T(x, uid, i));
              *)
              merge_shapes (shapes_of (Var.T(x, uid, i)))))
          ret
(*
    | _ ->
        let _ = Format.printf "%a:%d" Var.pr x uid in
        assert false
*)
  and shapes_of x =
    match env x with
      SimType.Unit | SimType.Bool | SimType.Int ->
        let ps = List.filter_map (function `P(y, t) when x = y -> Some(t) | _ -> None) sums in
        let p = (*???*)if ps = [] then Formula.ttrue else Formula.band ps in
        [Base(x, conv_base (env x), p)]
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




let rec of_interaction_type ty = assert false
(*
  match ty.IntType.shape with
    IntType.Unit(x) -> Base(x, Unit, ty.IntType.post)
  | IntType.Bool(x) -> Base(x, Bool, ty.IntType.post)
  | IntType.Int(x) -> Base(x, Int, ty.IntType.post)
  | IntType.Fun(sh1, sh2) ->
      Fun([of_interaction_type (IntType.make sh1 Formula.ttrue ty.IntType.pre),
           of_interaction_type (IntType.make sh2 Formula.ttrue ty.IntType.post)])
  | IntType.And(shs) ->
      of_interction_type shs
*)


(** require: x is a structured variable *)
let rec visible x y =
  match x with
    Var.V(_) ->
      false
  | Var.T(z, uid, arg) ->
      (match y with
        Var.V(_) -> false
      | Var.T(z', uid', arg') ->
          (z = z' && uid = uid' && arg' <= arg) ||
          (visible z y))

let visible_vars env x =
  let rec aux x =
    match x with
      Var.V(_) ->
        [x]
    | Var.T(x, uid, arg) ->
        aux x @ List.init (arg + 1) (fun i -> Var.T(x, uid, i))
  in
  List.filter_map (fun x -> if SimType.is_base (env x) then Some(x, env x) else None) (aux x)
