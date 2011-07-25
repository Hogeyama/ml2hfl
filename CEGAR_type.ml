
type var = string

type base =
    TUnit
  | TInt
  | TBool
  | TBottom
  | TList
  | TTuple of int
  | TEvent

type 'a t =
    TBase of base * ('a -> 'a list)
  | TAbs of ('a t -> 'a t)
  | TApp of 'a t * 'a t
  | TFun of ('a -> 'a t * 'a t)


let is_base_typ = function
    TBase _ -> true
  | _ -> false


let make_tapp typ typs =
  List.fold_left (fun typ1 typ2 -> TApp(typ1,typ2)) typ typs

let rec decomp_tapp = function
    TApp(typ1,typ2) ->
      let typ,typs = decomp_tapp typ1 in
        typ, typs@[typ2]
  | typ -> typ, []
  
