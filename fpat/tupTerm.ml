open Util
open Combinator

include Term

(** Tuple term expressions *)

(** {6 Auxiliary constructors} *)

let make tys ts = mk_app (mk_const (Const.Tuple tys)) ts
let mk_proj tys i t = mk_app (mk_const (Const.Proj(tys, i))) [t]
let mk_tuple2 = function
  | [] -> UnitTerm.make
  | [t] -> t
  | ts -> make (List.duplicate Type.mk_top(* @todo *) (List.length ts)) ts

(** {6 Auxiliary destructors} *)

let let_tuple e kok kex =
  match fun_args e with
  | Const(Const.Tuple tys), ts ->
    assert (List.length ts = List.length tys);
    kok tys ts
  | _ -> kex ()

let is_tuple e = let_tuple e (fun _ _ -> true) (fun _ -> false)

let elements_of (t, ty) =
  if Type.is_tuple ty then
    match fun_args t with
    | Const(Const.Tuple tys), ts -> List.combine ts tys
    | _, _ ->
      Type.let_tuple ty
        (fun tys -> List.mapi (fun i tyi -> mk_proj tys i t, tyi) tys)
    | _ -> assert false
  else [t, ty]

let rec get_proj_var atm =
  Term.visit
    (object
      method fvar _ = []
      method fcon _ = []
      method fapp t1 t2 = 
        match t1, t2 with
        | Term.Const (Const.Proj (tys, n)), Term.Var id->
          [id, Type.mk_tuple tys]
        | _ -> get_proj_var t1 @ get_proj_var t2
      method fbin b x t1 = assert false(*get_proj_var t1*)
    end)
    atm

let has_proj =
  Term.fold
    (object
      method fvar x = false
      method fcon = function Const.Proj (_, _) -> true | _ -> false
      method fapp r1 r2 = r1 || r2
      method fbin b x r1 = assert false
    end)

let eval_proj =
  Term.fold
    (object
      method fvar = Term.mk_var
      method fcon = Term.mk_const
      method fapp t1 t2 =
        match t1, Term.fun_args t2 with
        | Term.Const (Const.Proj (tys1, i)),
          (Term.Const (Const.Tuple tys2), args) ->
          (*Logger.debug_assert (fun () -> tys1 = tys2);*)
          List.nth args i
        | _ -> Term.mk_app t1 [t2]
      method fbin = Term.mk_binder
    end)
