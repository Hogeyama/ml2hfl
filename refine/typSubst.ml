open ExtList

(** Typed substitutions *)

type t = (Var.t * Term.t * SimType.t) list

let pr_elem ppf (x, t, _) =
  Format.fprintf ppf "%a -> %a" Var.pr x Term.pr t
let pr ppf sub =
  Format.fprintf ppf "%a" (Util.pr_list pr_elem " && ") sub

let fvs_elem (x, t, _) = x :: Term.fvs t
let fvs sub = Util.concat_map fvs_elem sub

let fun_of xttys =
  let sub = List.map (fun (x, t, _) -> x, t) xttys in
  fun x -> List.assoc x sub

let dom xttys = List.map Util.fst3 xttys

(** @todo support other cases *)
let sub_of t =
  match t with
    Term.App(_, Term.App(_, Term.Const(_, Const.EqBool), Term.Var(_, x)), t') when not (List.mem x (Term.fvs t')) ->
      [x, t', SimType.Bool]
  | Term.App(_, Term.App(_, Term.Const(_, Const.EqInt), Term.Var(_, x)), t') when not (List.mem x (Term.fvs t')) ->
      [x, t', SimType.Int]
  | _ ->
      []
