
open Util

type 'a t =
    TUnit
  | TBool
  | TAbsBool
  | TInt
  | TRInt of 'a
  | TVar of 'a t option ref
  | TFun of ('a t Id.t) * 'a t
  | TList of 'a t
  | TPair of ('a t Id.t) * 'a t
  | TConstr of string * bool
  | TPred of ('a t Id.t) * 'a list
(*| TLabel of 'a t Id.t * 'a t*)


let typ_unknown = TConstr("???", false)

let is_fun_typ = function
    TFun(_,_) ->
      true
  | _ ->
      false

let rec is_base_typ = function
    TUnit
  | TBool
  | TAbsBool
  | TInt
  | TRInt _ -> true
  | TPred(x,_) -> is_base_typ (Id.typ x)
  | _ -> false

let elim_tpred = function
    TPred(x,_) -> Id.typ x
  | typ -> typ

let rec decomp_tfun = function
    TFun(x,typ) ->
      let xs,typ = decomp_tfun typ in
        x :: xs, typ
  | typ -> [], typ

let rec can_unify typ1 typ2 =
  match typ1,typ2 with
      TVar{contents=Some typ1},typ2
    | typ1,TVar{contents=Some typ2} -> can_unify typ1 typ2
    | TPred(x,_), typ
    | typ, TPred(x,_) -> can_unify (Id.typ x) typ
    | TUnit,TUnit -> true
    | (TBool|TAbsBool),(TBool|TAbsBool) -> true
    | TInt,TInt -> true
    | TRInt _,TRInt _ -> true
    | TFun(x1,typ1),TFun(x2,typ2) -> can_unify (Id.typ x1) (Id.typ x2) && can_unify typ1 typ2
    | TList typ1, TList typ2 -> can_unify typ1 typ2
    | TPair(x1,typ1), TPair(x2,typ2) -> can_unify (Id.typ x1) (Id.typ x2) && can_unify typ1 typ2
    | TConstr("event",_), TFun _ -> true
    | TFun _, TConstr("event",_) -> true
    | TConstr(s1,_),TConstr(s2,_) -> s1 = s2
    | TVar{contents=None}, _ -> true
    | _, TVar{contents=None} -> true
    | _ when typ1 = typ_unknown || typ2 = typ_unknown -> true
    | _ -> false


let rec print ?(occur=fun _ _ -> false) print_pred fm typ =
  let print' = print ~occur print_pred in
  let print_preds ps = print_list print_pred "; " ps in
    match typ with
        TUnit -> Format.fprintf fm "unit"
      | TAbsBool -> Format.fprintf fm "abool"
      | TBool -> Format.fprintf fm "bool"
      | TInt -> Format.fprintf fm "int"
      | TRInt p -> assert false (*Format.fprintf fm "{ %a | %a }" Id.print abst_var print_preds [p]*)
      | TVar{contents=Some typ} -> print' fm typ
      | TVar _ -> Format.fprintf fm "!!!"
      | TFun(x, typ) ->
          if occur x typ
          then Format.fprintf fm "(@[%a:%a@ ->@ %a@])" Id.print x print' (Id.typ x) print' typ
          else Format.fprintf fm "(@[%a@ ->@ %a@])" print' (Id.typ x) print' typ
      | TList typ -> Format.fprintf fm "@[%a list@]" print' typ
      | TPair(x,typ) ->
          if occur x typ
          then Format.fprintf fm "(@[%a:%a@ *@ %a@])" Id.print x print' (Id.typ x) print' typ
          else Format.fprintf fm "(@[%a@ *@ %a@])" print' (Id.typ x) print' typ
      | TConstr(s,_) -> Format.pp_print_string fm s
      | TPred(x,ps) -> Format.fprintf fm "@[%a[\\%a. %a]@]" print' (Id.typ x) Id.print x print_preds ps


let print_typ_init typ = print (fun _ -> assert false) typ


let rec flatten typ =
  match typ with
      TVar{contents = Some typ'} -> flatten typ'
    | _ -> typ

let rec occurs r typ =
  match flatten typ with
      TUnit -> false
    | TBool -> false
    | TAbsBool -> false
    | TInt -> false
    | TRInt p -> assert false
    | TVar({contents=None} as r') -> r == r'
    | TVar{contents=Some typ} -> assert false
    | TFun(x,typ) -> occurs r (Id.typ x) || occurs r typ
    | TList typ -> occurs r typ
    | TPair(x,typ) -> occurs r (Id.typ x) || occurs r typ
    | TConstr(s,b) -> false
    | TPred(x,_) -> occurs r (Id.typ x)

exception CannotUnify

let rec unify typ1 typ2 =
  match flatten typ1, flatten typ2 with
      TUnit, TUnit
    | TBool, TBool
    | TInt, TInt -> ()
    | TRInt _, TRInt _ -> ()
    | TFun(x1, typ1), TFun(x2, typ2) ->
        unify (Id.typ x1) (Id.typ x2);
        unify typ1 typ2
    | TList typ1, TList typ2 -> unify typ1 typ2
    | TPair(x1, typ1), TPair(x2, typ2) ->
        unify (Id.typ x1) (Id.typ x2);
        unify typ1 typ2
    | TVar r1, TVar r2 when r1 == r2 -> ()
    | TVar({contents = None} as r), typ
    | typ, TVar({contents = None} as r) ->
        if occurs r typ then
          (Format.printf "occurs check failure: %a, %a@."
             print_typ_init (flatten typ1) print_typ_init (flatten typ2);
           raise CannotUnify)
        else
          r := Some typ
    | TPred(x,_), typ
    | typ, TPred(x,_) -> unify (Id.typ x) typ
    | TConstr(s1,_), TConstr(s2,_) -> assert (s1 = s2)
    | _ ->
        Format.printf "unification error: %a, %a@."
          print_typ_init (flatten typ1) print_typ_init (flatten typ2);
        raise CannotUnify


let rec same_shape typ1 typ2 =
  match typ1,typ2 with
      TUnit,TUnit -> true
    | TBool,TBool -> true
    | TAbsBool,TAbsBool -> true
    | TInt,TInt -> true
    | TRInt _, TRInt _ -> true
    | TVar{contents=None}, TVar{contents=None} -> true
    | TVar{contents=Some typ1},TVar{contents=Some typ2} -> same_shape typ1 typ2
    | TFun(x1,typ1),TFun(x2,typ2) -> same_shape (Id.typ x1) (Id.typ x2) && same_shape typ1 typ2
    | TList typ1, TList typ2 -> same_shape typ1 typ2
    | TPair(x1,typ1),TPair(x2,typ2) -> same_shape (Id.typ x1) (Id.typ x2) && same_shape typ1 typ2
    | TConstr(s1,_),TConstr(s2,_) -> s1 = s2
    | _ -> Format.printf "%a,%a@.Type.same_shape" print_typ_init typ1 print_typ_init typ2; assert false


let rec is_poly_typ = function
    TUnit -> false
  | TBool -> false
  | TAbsBool -> false
  | TInt -> false
  | TRInt _ -> false
  | TVar{contents=None} -> true
  | TVar{contents=Some typ} -> is_poly_typ typ
  | TFun(x,typ) -> is_poly_typ (Id.typ x) || is_poly_typ typ
  | TList typ -> is_poly_typ typ
  | TPair(x,typ) -> is_poly_typ (Id.typ x) || is_poly_typ typ
  | TConstr _ -> false
  | TPred(x,_) -> is_poly_typ (Id.typ x)


let rec copy = function
    TVar {contents = Some typ} -> copy typ
  | TVar {contents = None} -> TVar (ref None)
  | typ -> typ



let rec app_typ typ typs =
  match typ,typs with
    | TFun(_,typ2), _::typs' -> app_typ typ2 typs'
    | _, [] -> typ
    | _ -> assert false

let fst_typ typ =
  match typ with
    TPair(x, _) -> Id.typ x
  | _ -> assert false

let snd_typ typ =
  match typ with
    TPair(_, typ2) -> typ2
  | _ -> assert false

let rec has_pred = function
    TUnit -> false
  | TBool -> false
  | TAbsBool -> false
  | TInt -> false
  | TRInt _ -> assert false
  | TVar{contents=None} -> false
  | TVar{contents=Some typ} -> has_pred typ
  | TFun(x,typ) -> has_pred (Id.typ x) || has_pred typ
  | TList typ -> has_pred typ
  | TPair(x,typ) -> has_pred (Id.typ x) || has_pred typ
  | TConstr _ -> false
  | TPred(x,ps) -> has_pred (Id.typ x) || ps <> []

let rec to_id_string = function
    TUnit -> "unit"
  | TBool -> "bool"
  | TAbsBool -> assert false
  | TInt -> "int"
  | TRInt _ -> assert false
  | TVar{contents=None} -> "abst"
  | TVar{contents=Some typ} -> to_id_string typ
  | TFun(x,typ) -> to_id_string (Id.typ x) ^ "__" ^ to_id_string typ
  | TList typ -> to_id_string typ ^ "_list"
  | TPair(x,typ) -> to_id_string (Id.typ x) ^ "_x_" ^ to_id_string typ
  | TConstr(s,_) -> s
  | TPred(x,_) -> to_id_string (Id.typ x)
