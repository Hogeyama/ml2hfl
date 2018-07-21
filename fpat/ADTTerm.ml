open Util
open Combinator
open Term

(** Algebraic data type term expressions *)

(** {6 Auxiliary constructors} *)

(*
val mk_kon : TypEnv.elem -> t list -> t
val mk_list : Type.t -> t list -> t
val let_kon : t -> (Type.t -> Idnt.t -> t list -> 'a) -> 'a
 *)

let mk_kon (x, ty) ts = mk_app (mk_const (Const.Con(ty, x))) ts
let mk_list ty ts =
  List.fold_right
    (fun t1 t2 -> mk_app (mk_const (Const.Cons ty)) [t1; t2])
    ts
    (mk_const (Const.Nil ty))
let mk_accessor ty x i t = mk_app (mk_const (Const.Accessor (ty, x, i))) [t]

(** {6 Auxiliary destructors} *)

let is_kon e =
  match fun_args e with
  | Const (Const.Con(ty, x)), ts -> true
  | _ -> false

let let_kon e f =
  match fun_args e with
  | Const (Const.Con(ty, x)), ts ->
    assert (List.length ts = Type.arity_of ty);
    f ty x ts
  | _ -> assert false

let eval_accessor =
  Term.fold
    (object
      method fvar = Term.mk_var
      method fcon = Term.mk_const
      method fapp t1 t2 =
        match t1, Term.fun_args t2 with
        | Term.Const (Const.Accessor (_, x, i)),
          (Term.Const (Const.Con(_, y)), args) when x = y ->
          List.nth args i
        | _ -> Term.mk_app t1 [t2]
      method fbin = Term.mk_binder
    end)
