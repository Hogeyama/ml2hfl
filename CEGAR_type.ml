
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
  | TFun of 'a t * ('a -> 'a t)


let typ_unit = TBase(TUnit, fun _ -> [])
let typ_bool = TBase(TBool, fun x -> [x])
let typ_int = TBase(TInt, fun _ -> [])
let typ_event = TFun(TFun(typ_unit, fun _ -> typ_unit), fun _ -> typ_unit)
let make_tfun typ1 typ2 = TFun(typ1, fun _ -> typ2)

let is_base = function
    TBase _ -> true
  | _ -> false

let decomp_base = function
    TBase(b, ps) -> b, ps
  | _ -> assert false

let get_base typ = fst (decomp_base typ)

let rec app typ ts =
  match typ,ts with
    | TFun(_,typ2), t::ts' -> app (typ2 t) ts'
    | _, [] -> typ
    | _ -> assert false

let make_tapp typ typs =
  List.fold_left (fun typ1 typ2 -> TApp(typ1,typ2)) typ typs

let rec decomp_tapp = function
    TApp(typ1,typ2) ->
      let typ,typs = decomp_tapp typ1 in
        typ, typs@[typ2]
  | typ -> typ, []


