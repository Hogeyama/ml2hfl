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
