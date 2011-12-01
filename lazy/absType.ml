open ExtList

type t = Base of b * Var.t * Term.t list | Fun of t * t
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
  | Fun(_, _) -> false

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
  | Fun(aty1, aty2) ->
      let _ = Format.fprintf ppf "@[<hov>" in
      let _ = if is_base aty1 then Format.fprintf ppf "%a:" Var.pr (bv_of aty1) in
      let _ = if is_base aty1 then Format.fprintf ppf "%a" pr aty1 else Format.fprintf ppf "(%a)" pr aty1 in
      Format.fprintf ppf "@ ->@ %a@]" pr aty2
and bv_of aty =
  match aty with
    Base(_, bv, _) -> bv
  | Fun(_, _) ->
      let _ = Format.printf "%a@." pr aty in
      assert false

let pr_bind ppf (f, sty) = Format.fprintf ppf "%a: %a" Var.pr f pr sty
let pr_env ppf env = Format.fprintf ppf "@[<v>%a@]" (Util.pr_list pr_bind "@ ") env

let rec subst sub aty =
  match aty with
    Base(bty, x, ts) ->
      let sub' y = if Var.equiv x y then raise Not_found else sub y in
      Base(bty, x, List.map (Term.subst sub') ts)
  | Fun(aty1, aty2) ->
      Fun
		      (subst sub aty1,
        let sub' y = if is_base aty1 && Var.equiv (bv_of aty1) y then raise Not_found else sub y in
		      subst sub' aty2)

let rec merge2 aty1 aty2 =
  match aty1, aty2 with
    Base(bty1, x1, ts1), Base(bty2, x2, ts2) ->
      let _ = assert (bty1 = bty2) in
      let x = Var.new_var () in
      let sub1 y = if y = x1 then Term.make_var2 x else raise Not_found in
      let sub2 y = if y = x2 then Term.make_var2 x else raise Not_found in
      Base(bty1, x, List.unique ((List.map (Term.subst sub1) ts1) @ (List.map (Term.subst sub2) ts2)))
  | Fun(aty11, aty12), Fun(aty21, aty22) ->
      let aty1 = merge2 aty11 aty21 in
      let sub1 y = if is_base aty11 && Var.equiv (bv_of aty11) y then Term.make_var2 (bv_of aty1) else raise Not_found in
      let sub2 y = if is_base aty21 && Var.equiv (bv_of aty21) y then Term.make_var2 (bv_of aty1) else raise Not_found in
      let aty12 = subst sub1 aty12 in
      let aty22 = subst sub2 aty22 in
      Fun(aty1, merge2 aty12 aty22)

let merge atys =
  match atys with
    [] -> assert false
  | aty::atys ->
      List.fold_left (fun aty1 aty2 -> merge2 aty1 aty2) aty atys

(*
let of_interaction_type f sty =
  let base_name = function
      IntType.Unit(x) -> x
    | IntType.Bool(x) -> x
    | IntType.Int(x) -> x
    | _ -> assert false
  in
  let get_env_aux tys =
    if IntType.is_base (List.hd tys)
    then
      let xs = List.map base_name tys in
      let x = List.hd xs in
        List.map (fun y -> x,y) xs
    else []
  in
  let rec get_env sty =
    match sty with
        IntType.Unit _ -> []
      | IntType.Bool _ -> []
      | IntType.Int _ -> []
      | IntType.Fun tts ->
          let tys1,tys2 = List.split tts in
          let env1 = get_env_aux tys1 in
          let env2 = get_env_aux tys2 in
            List.rev_append env1 env2
  in
  let rec trans env ps vars sty =
    match sty with
        IntType.Unit x
      | IntType.Bool x
      | IntType.Int x ->
          let vars' = x::vars in
          let ps1,ps2 = List.partition (fun p -> Util.subset (Term.fvs p) vars') ps in
          let b =
            match sty with
                IntType.Unit _ -> Unit
              | IntType.Bool _ -> Bool
              | IntType.Int _ -> Int
              | _ -> assert false
          in
            Base(b, x, ps1), ps2
      | IntType.Fun tts when IntType.is_base (fst (List.hd tts)) ->
          let sty1,sty2 = List.hd tts in
          let x = base_name sty1 in
          let vars' = x::vars in
          let aty1,ps1 = trans env ps vars sty1 in
          let aty2,ps2 = trans env ps1 vars' sty2 in
            Fun(x, aty1, aty2), ps2
      | IntType.Fun tts ->
          let sty1,sty2 = List.hd tts in
          let aty1,ps1 = trans env ps vars sty1 in
          let aty2,ps2 = trans env ps1 vars sty2 in
            Fun(Var.new_var (), aty1, aty2), ps2
  in
  let env = get_env sty.IntType.shape in
  let subst = Term.subst (fun x -> Term.make_var2 (List.assoc x env)) in
  let ps = [subst sty.IntType.pre; subst sty.IntType.post] in
  let ps' = List.filter (function Term.Const(_, Const.True) -> false | _ -> true) ps in
  let aty,ps'' = trans env ps' [] sty.IntType.shape in
    if ps''<>[] then Format.printf "Cannot represent as abstraction type:@.";
    List.iter (Format.printf "%a: %a@." Var.pr f Term.pr) ps'';
    aty
*)

let rec of_refinement_type rty =
  match rty with
    RefType.Base(x, RefType.Unit, t) ->
      Base(Unit, x, if t = Term.ttrue then [] else [t])
  | RefType.Base(x, RefType.Bool, t) ->
      Base(Bool, x, if t = Term.ttrue then [] else [t])
  | RefType.Base(x, RefType.Int, t) ->
      Base(Int, x, if t = Term.ttrue then [] else [t])
  | RefType.Fun(xs) ->
      let _ = assert (xs <> []) in
      let atys = List.map (fun (rty1, rty2) -> Fun(of_refinement_type rty1, of_refinement_type rty2)) xs in
      merge atys
