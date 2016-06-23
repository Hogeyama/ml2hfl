open Util

type var = string

type base =
  | TUnit
  | TInt
  | TBool
  | TList
  | TTuple
  | TAbst of string

type 'a t =
  | TBase of base * ('a -> 'a list)
  | TAbs of ('a t -> 'a t)
  | TApp of 'a t * 'a t
  | TFun of 'a t * ('a -> 'a t)

let _TApp typ1 typ2 = TApp(typ1, typ2)

let typ_result_base = TAbst "X"
let typ_result = TBase(typ_result_base, fun _ -> [])
let typ_unit = TBase(TUnit, fun _ -> [])
let typ_bool_empty = TBase(TBool, fun x -> [])
let typ_bool_id = TBase(TBool, fun x -> [x])
let typ_bool() = if !Flag.bool_init_empty then typ_bool_empty else typ_bool_id
let typ_int = TBase(TInt, fun _ -> [])
let typ_abst s = TBase(TAbst s, fun _ -> [])
let typ_event = TFun(TFun(typ_unit, fun _ -> typ_unit), fun _ -> typ_unit)
let make_tfun typ1 typ2 = TFun(typ1, fun _ -> typ2)
let typ_unknown = TBase(TAbst "?", fun _ -> [])

let is_typ_result = function
  | TBase(b, _) -> b = typ_result_base
  | _ -> false

let is_base = function
  | TBase _ -> true
  | _ -> false

let decomp_base = function
  | TBase(b, ps) -> b, ps
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
  | TApp(typ1,typ2) ->
      let typ,typs = decomp_tapp typ1 in
      typ, typs@[typ2]
  | typ -> typ, []

let make_ttuple typs =
  List.fold_left _TApp (TBase(TTuple, fun _ -> [])) typs

let is_ttuple typ =
  match decomp_tapp typ with
  | TBase(TTuple, _), _ -> true
  | _ -> false

let rec arg_num x typ =
  match typ with
  | TBase _ -> 0
  | TAbs _ -> unsupported "CEGAR_type.arg_num: TAbs"
  | TApp _ -> unsupported "CEGAR_type.arg_num: TAbs"
  | TFun(typ1, typ2) -> 1 + arg_num x (typ2 x)
