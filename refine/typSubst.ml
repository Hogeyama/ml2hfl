open ExtList

(** Typed substitutions *)

type t = (Var.t * Term.t * SimType.t) list

let pr_elem ppf (x, t, _) =
  Format.fprintf ppf "%a -> %a" Var.pr x Term.pr t
let pr ppf xttys =
  Format.fprintf ppf "%a" (Util.pr_list pr_elem " && ") xttys

let fvs_elem (x, t, _) = x :: Term.fvs t
let fvs xttys = Util.concat_map fvs_elem xttys

let fun_of xttys =
  let xttys' = List.map (fun (x, t, _) -> x, t) xttys in
  fun x -> List.assoc x xttys'

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

let subtract xttys xs =
  List.filter (fun (x, _, _) -> not (List.mem x xs)) xttys

let subtract_fun sub xs =
  fun x -> if List.mem x xs then raise Not_found else sub x

let rec subst sub t =
  match t with
    Term.Var(a, x) ->
      (try sub x with Not_found -> Term.Var(a, x))
  | Term.Const(a, c) ->
      Term.Const(a, c)
  | Term.App(a, t1, t2) ->
      Term.App(a, subst sub t1, subst sub t2)
  | Term.Call(_, _, _) | Term.Ret(_, _, _, _) | Term.Error(_) ->
      assert false
  | Term.Forall(a, env, t) ->
      let xs = List.map fst env in
      Term.Forall(a, env, subst (subtract_fun sub xs) t)
  | Term.Exists(a, env, t) ->
      let xs = List.map fst env in
      Term.Exists(a, env, subst (subtract_fun sub xs) t)

(** @todo compute the fixed-point of sub first *)
let subst_fixed sub t =
  let _ = Global.log_begin ~disable:true "TypSubst.subst_fixed" in
  let _ = Global.log (fun () -> Format.printf "input: %a@," Term.pr t) in
  let t = Util.fixed_point (subst sub) Term.equiv t in
  let _ = Global.log (fun () -> Format.printf "output: %a" Term.pr t) in
  let _ = Global.log_end "TermTypSubst.subst_fixed" in
  t

let rename sub t =
  subst (fun x -> List.assoc x sub) t

(** @param p every variable not satisfying p is renamed *)
let fresh p t =
  let xs = List.filter (fun x -> not (p x)) (Term.fvs t) in
  let sub = List.map (fun x -> x, Term.new_var ()) xs in
  subst (fun x -> List.assoc x sub) t

(** rename given variables to fresh ones
    @param xs variables to be renamed
    @require not (Util.is_dup xs) *)
let fresh_vars xs t =
  let sub = List.map (fun x -> x, Term.new_var ()) xs in
  subst (fun x -> List.assoc x sub) t
