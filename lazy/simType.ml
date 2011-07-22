open ExtList

type t = Unit | Bool | Int | Fun of t * t

let rec arity ty =
  match ty with
    Unit
  | Bool
  | Int -> 0
  | Fun(_, ty) -> 1 + arity ty

let rec args_ret ty =
  match ty with
    Fun(ty1, ty2) ->
      let args, ret = args_ret ty2 in
      ty1::args, ret
  | _ ->
      [], ty
