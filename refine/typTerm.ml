open ExtList
open ExtString
open Term

(** Typed term expressions *)

(** {6 The type of terms with types} *)

type t = Term.t * SimType.t

(** {6 Printers} *)

let pr ppf (t, ty) =
  Format.fprintf ppf "%a:%a" Term.pr t SimType.pr ty

(** {6 Basic functions} *)

let fvs (t, _) = Term.fvs t

let equiv (t1, ty1) (t2, ty2) =
  let _ = if !Global.debug then assert (ty1 = ty2) in
  t1 = t2

let rec env_of (t, ty) =
  match fun_args t with
    Var(_, x), [] ->
      if Var.is_coeff x then [] else [x, ty]
  | Const(_, c), [] ->
      []
  | Const(a, Const.Not), [t] -> 
      env_of (t, SimType.Bool)
  | Const(a, Const.Minus), [t] ->
      env_of (t, SimType.Int)
  | Const(a, Const.EqUnit), [t1; t2]
  | Const(a, Const.NeqUnit), [t1; t2] ->
      env_of (t1, SimType.Unit) @ env_of (t2, SimType.Unit)
  | Const(a, Const.EqBool), [t1; t2]
  | Const(a, Const.NeqBool), [t1; t2]
  | Const(a, Const.And), [t1; t2]
  | Const(a, Const.Or), [t1; t2]
  | Const(a, Const.Imply), [t1; t2]
  | Const(a, Const.Iff), [t1; t2] ->
      env_of (t1, SimType.Bool) @ env_of (t2, SimType.Bool)
  | Const(a, Const.EqInt), [t1; t2]
  | Const(a, Const.NeqInt), [t1; t2]
  | Const(a, Const.Lt), [t1; t2]
  | Const(a, Const.Gt), [t1; t2]
  | Const(a, Const.Leq), [t1; t2]
  | Const(a, Const.Geq), [t1; t2]
  | Const(a, Const.Add), [t1; t2]
  | Const(a, Const.Sub), [t1; t2]
  | Const(a, Const.Mul), [t1; t2] ->
      env_of (t1, SimType.Int) @ env_of (t2, SimType.Int)
  | _->
      let _ = Format.printf "@,%a@," pr (t, ty) in
      assert false

let fvs_ty ty tty =
  List.filter_map (fun (x, ty') -> if ty' = ty then Some(x) else None) (env_of tty)

(** {6 Utility functions} *)

(** matching a term t1 with a pattern t2:
    @require Util.inter (Term.fvs t1) xs = []
    @require if imply t1 t2, then t1 implies t2 but if not (imply t1 t2), t1 may imply t2
    @return xttys such that Term.subst (TypSubst.sub_of xttys) t2 = t1
    @ensure Util.subset (TypSubst.dom xttys) (Util.inter (Term.fvs t2) xs) *)
let matches ?(imply = fun ts1 ts2 -> false) env xs (t1, ty1) (t2, ty2) =
  let _ = Global.log_begin ~disable:true "TypTerm.matches" in
  let _ = Global.log (fun () -> Format.printf "t1: %a@,t2: %a@," Term.pr t1 Term.pr t2) in
  let _ = if !Global.debug then assert (ty1 = ty2) in
  let xttys =
    if t1 = t2 then
      []
    else if Util.inter (Term.fvs t2) xs = [] then
      let eq = (*Formula.eq_tty (t1, ty1) (t2, ty2)*)
        match ty1 with
          SimType.Unit ->
            bop Const.EqUnit t1 t2
        | SimType.Bool ->
            bop Const.EqBool t1 t2
        | SimType.Int ->
            bop Const.EqInt t1 t2
        | _ -> assert false
      in
      if imply env [eq] then
        []
      else
        let _ = Global.log_end "TypTerm.matches" in
        raise MayNotMatch
    else
      match t2 with
        Term.Var(_, x) when List.mem x xs ->
          [x, t1, ty1]
      | _ ->
          (try
            LinArith.matches env xs t1 (LinArith.of_term t2)
          with Invalid_argument _ ->
            let _ = Global.log (fun () -> Format.printf "t1: %a@,t2: %a@," Term.pr t1 Term.pr t2) in
            let _ = Global.log_end "TypTerm.matches" in
            raise MayNotMatch
          | MayNotMatch ->
            let _ = Global.log_end "TypTerm.matches" in
            raise MayNotMatch
          | NeverMatch ->
            let _ = Global.log_end "TypTerm.matches" in
            raise NeverMatch)
  in
  let _ = Global.log_end "TypTerm.matches" in
  xttys
