open ExtList

(** Simple types *)

(** {6 The type of simple types} *)

type t = Unit | Bool | Int | Fun of t * t

(** {6 Basic functions} *)

let equiv ty1 ty2 = ty1 = ty2

let is_base ty =
  match ty with
    Unit | Bool | Int -> true
  | Fun(_, _) -> false

let is_fun ty =
  match ty with
    Unit | Bool | Int -> false
  | Fun(_, _) -> true

let rec arity ty =
  match ty with
    Unit
  | Bool
  | Int -> 0
  | Fun(_, ty) -> 1 + arity ty

(** {6 Printers} *)

let rec pr ppf ty =
  match ty with
    Unit ->
      Format.fprintf ppf "unit"
  | Bool ->
      Format.fprintf ppf "bool"
  | Int ->
      Format.fprintf ppf "int"
  | Fun(ty1, ty2) ->
      let _ =
        if is_base ty1 then
          Format.fprintf ppf
            "@[<hov>%a" pr ty1
        else
          Format.fprintf ppf
            "@[<hov>(%a)" pr ty1
      in
      Format.fprintf ppf
        "@ ->@ %a@]" pr ty2

let pr_bind ppf (x, ty) =
  Format.fprintf ppf "%a: %a" Var.pr x pr ty

let pr_env ppf env =
  Format.fprintf ppf "@[<v>%a@]" (Util.pr_list pr_bind ",") env

(** {6 Constructors} *)

let make_ftyp tys =
  List.fold_right
    (fun ty1 ty2 -> Fun(ty1, ty2))
    (Util.init tys)
    (List.last tys)

(** {6 Destructors} *)

let rec args_ret ty =
  match ty with
    Fun(ty1, ty2) ->
      let args, ret = args_ret ty2 in
      ty1::args, ret
  | _ ->
      [], ty
