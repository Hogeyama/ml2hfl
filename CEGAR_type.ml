
type var = string

type base =
    TUnit
  | TInt
  | TBool
  | TList
  | TTuple of int

type 'a t =
    TBase of base * ('a -> 'a list)
  | TAbs of ('a t -> 'a t)
  | TApp of 'a t * 'a t
  | TFun of ('a -> 'a t * 'a t)


let typ_unit = TBase(TUnit, fun _ -> [])
let typ_bool = TBase(TBool, fun x -> [x])
let typ_int = TBase(TInt, fun _ -> [])
let typ_event = TFun(fun _ -> TFun(fun _ -> typ_unit, typ_unit), typ_unit)
let make_tfun typ1 typ2 = TFun(fun _ -> typ1, typ2)

let is_base_typ = function
    TBase _ -> true
  | _ -> false

let get_base = function
    TBase(b, _) -> b
  | _ -> assert false


let make_tapp typ typs =
  List.fold_left (fun typ1 typ2 -> TApp(typ1,typ2)) typ typs

let rec decomp_tapp = function
    TApp(typ1,typ2) ->
      let typ,typs = decomp_tapp typ1 in
        typ, typs@[typ2]
  | typ -> typ, []
  
