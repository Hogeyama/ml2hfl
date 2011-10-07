open ExtList

type t = Base of b * Var.t * Term.t list | Fun of Var.t * t * t
and b = Unit | Bool | Int

let pr_base ppf bty =
  match bty with
    Unit ->
      Format.fprintf ppf "unit"
  | Bool ->
      Format.fprintf ppf "bool"
  | Int ->
      Format.fprintf ppf "int"

let is_base aty =
  match aty with
    Base(_, _, _) -> true
  | Fun(_, _, _) -> false

let rec pr ppf aty =
  match aty with
    Base(bty, x, ts) ->
      (match ts with
        [] ->
          Format.fprintf ppf "%a" pr_base bty
      | _ ->
          let pr_aux ppf t =
            Format.fprintf ppf "%a -> %a" Var.pr x Term.pr t
          in
          Format.fprintf ppf "%a[@[<hov>%a@]]" pr_base bty (Util.pr_list pr_aux ",@,") ts)
  | Fun(x, aty1, aty2) ->
      let _ = Format.fprintf ppf "@[<hov>%a:" Var.pr x in
      let _ = if is_base aty1 then Format.fprintf ppf "%a" pr aty1 else Format.fprintf ppf "(%a)" pr aty2 in
      Format.fprintf ppf "@ ->@ %a@]" pr aty2

let pr_bind ppf (f, sty) = Format.fprintf ppf "%a: %a" Var.pr f pr sty
let pr_env ppf env = Format.fprintf ppf "@[<v>%a@]" (Util.pr_list pr_bind "@ ") env

let rec subst sub aty =
  match aty with
    Base(bty, x, ts) ->
      let sub' y = if y = x then raise Not_found else sub y in
      Base(bty, x, List.map (Term.subst sub') ts)
  | Fun(x, aty1, aty2) ->
      Fun
		      (x,
		      subst sub aty1,
		      let sub' y = if Var.equiv x y then raise Not_found else sub y in
		      subst sub' aty2)

let rec merge2 aty1 aty2 =
  match aty1, aty2 with
    Base(bty1, x1, ts1), Base(bty2, x2, ts2) ->
      let _ = assert (bty1 = bty2) in
      let x = Var.new_var () in
      let sub1 y = if y = x1 then Term.make_var2 x else raise Not_found in
      let sub2 y = if y = x2 then Term.make_var2 x else raise Not_found in
      Base(bty1, x, List.unique ((List.map (Term.subst sub1) ts1) @ (List.map (Term.subst sub2) ts2)))
  | Fun(x1, aty11, aty12), Fun(x2, aty21, aty22) ->
      let x = Var.new_var () in
      let sub1 y = if y = x1 then Term.make_var2 x else raise Not_found in
      let sub2 y = if y = x2 then Term.make_var2 x else raise Not_found in
      let aty12 = subst sub1 aty12 in
      let aty22 = subst sub2 aty22 in
      Fun(x, merge2 aty11 aty21, merge2 aty12 aty22)

let merge atys =
  match atys with
    [] -> assert false
  | aty::atys ->
      List.fold_left (fun aty1 aty2 -> merge2 aty1 aty2) aty atys

let rec of_refinement_type rty =
  match rty with
    RefType.Base(x, RefType.Unit, t) -> Base(Unit, x, if t = Term.ttrue then [] else [t])
  | RefType.Base(x, RefType.Bool, t) -> Base(Bool, x, if t = Term.ttrue then [] else [t])
  | RefType.Base(x, RefType.Int, t) -> Base(Int, x, if t = Term.ttrue then [] else [t])
  | RefType.Fun(xs) ->
      let res = List.map (fun (x, rty1, rty2) -> x, (of_refinement_type rty1, of_refinement_type rty2)) xs in
      let xs = List.map fst res in
      let atys1 = List.map fst (List.map snd res) in
      let atys2 = List.map snd (List.map snd res) in
      let x = Var.new_var () in
      let atys2 = List.map2 (fun y aty -> let sub z = if z = y then Term.make_var2 x else raise Not_found in subst sub aty) xs atys2 in
      Fun(x, merge atys1, merge atys2)
