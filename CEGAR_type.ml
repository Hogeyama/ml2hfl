
type var = string

type base =
    TUnit
  | TInt
  | TBool

type 'a t =
    TBase of base * ('a -> 'a list)
  | TVar of var
  | TApp of 'a t * 'a t
  | TFun of ('a -> 'a t * 'a t)


let is_base_typ = function
    TBase _ -> true
  | _ -> assert false
