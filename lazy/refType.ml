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
        Format.fprintf ppf " @[<hov>%a:%a@ ->@ %a@]" Var.pr x pr rty1 pr rty2
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

let rec of_sized_type sty =(*???*)
  match sty.SizType.shape with
    SizType.Unit(x) -> Base(x, Unit, sty.SizType.post)
  | SizType.Bool(x) -> Base(x, Bool, sty.SizType.post)
  | SizType.Int(x) -> Base(x, Int, sty.SizType.post)
  | SizType.Fun(xs) ->
      Fun
        (List.map
          (fun (ty1, ty2) ->
            (match ty1 with SizType.Unit(x) | SizType.Bool(x) | SizType.Int(x) -> x | _ -> Var.new_var()),
            of_sized_type (SizType.make ty1 Term.ttrue sty.SizType.pre),
            of_sized_type (SizType.make ty2 Term.ttrue sty.SizType.post))
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
