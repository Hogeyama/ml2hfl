
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
  | TPair of 'a t * 'a t
  | TConstr of string * bool
  | TPred of 'a t * 'a list
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
  | TInt _
  | TRInt _ -> true
  | TPred(typ,_) -> is_base_typ typ
  | _ -> false

let elim_tpred = function
    TPred(typ,_) -> typ
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
    | TPred(typ1,_), typ2
    | typ1, TPred(typ2,_) -> can_unify typ1 typ2
    | TUnit,TUnit -> true
    | (TBool|TAbsBool),(TBool|TAbsBool) -> true
    | TInt _,TInt _ -> true
    | TRInt _,TRInt _ -> true
    | TFun(x1,typ1),TFun(x2,typ2) -> can_unify (Id.typ x1) (Id.typ x2) && can_unify typ1 typ2
    | TList typ1, TList typ2 -> can_unify typ1 typ2
    | TPair(typ11,typ12), TPair(typ21,typ22) -> can_unify typ11 typ21 && can_unify typ12 typ22
    | TConstr("event",_), TFun _ -> true
    | TFun _, TConstr("event",_) -> true
    | TConstr(s1,_),TConstr(s2,_) -> s1 = s2
(*
    | TVariant stypss1,TVariant stypss2 ->
        let aux (s1,typs1) (s2,typs2) = s1=s2 && List.for_all2 can_unify typs1 typs2 in
          List.length stypss1 = List.length stypss2 && List.for_all2 aux stypss1 stypss2
    | TRecord(_,fields1),TRecord(_,fields2) -> List.for_all2 (fun (s1,(_,typ1)) (s2,(_,typ2)) -> s1=s2 && can_unify typ1 typ2) fields1 fields2
*)
    | TVar{contents=None}, _ -> true
    | _, TVar{contents=None} -> true
    | _ when typ1 = typ_unknown || typ2 = typ_unknown -> true
    | _ -> false


let rec print ?(occur=fun _ _ -> false) print_pred fm typ =
  let print' = print ~occur print_pred in
  let print_preds = print_list print_pred ";" false in
    match typ with
        TUnit -> Format.fprintf fm "unit"
      | TAbsBool -> Format.fprintf fm "abool"
      | TBool -> Format.fprintf fm "bool"
      | TInt -> Format.fprintf fm "int"
      | TRInt p -> assert false (*Format.fprintf fm "{ %a | %a }" Id.print abst_var print_preds [p]*)
      | TVar{contents=Some typ} -> print' fm typ
      | TVar _ -> Format.fprintf fm "!!!"
      | TFun(x, typ) ->
          if Id.to_string x = "" || not (occur x typ)
          then Format.fprintf fm "(@[%a@ ->@ %a@])" print' (Id.typ x) print' typ
          else Format.fprintf fm "(@[%a:%a@ ->@ %a@])" Id.print x print' (Id.typ x) print' typ
      | TList typ -> Format.fprintf fm "@[%a list@]" print' typ
      | TPred(TList typ, ps) -> Format.fprintf fm "@[%a list[%a]@]" print' typ print_preds ps
      | TPair(typ1,typ2) -> Format.fprintf fm "(@[%a@ *@ %a@])" print' typ1 print' typ2
      | TConstr(s,_) -> Format.pp_print_string fm s
      | TPred(typ,ps) -> Format.fprintf fm "%a[%a]" print' typ print_preds ps


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
    | TPair(typ1,typ2) -> occurs r typ1 || occurs r typ2
    | TConstr(s,b) -> false
    | TPred(typ,_) -> occurs r typ

exception CannotUnify

let rec unify typ1 typ2 =
  match flatten typ1, flatten typ2 with
      TUnit, TUnit
    | TBool, TBool
    | TInt _, TInt _ -> ()
    | TRInt _, TRInt _ -> ()
    | TFun(x1, typ1), TFun(x2, typ2) ->
        unify (Id.typ x1) (Id.typ x2);
        unify typ1 typ2
    | TList typ1, TList typ2 -> unify typ1 typ2
    | TPair(typ11,typ12), TPair(typ21,typ22) ->
        unify typ11 typ21;
        unify typ12 typ22
    | TVar r1, TVar r2 when r1 == r2 -> ()
    | TVar({contents = None} as r), typ
    | typ, TVar({contents = None} as r) ->
        if occurs r typ then
          (Format.printf "occurs check failure: %a, %a@."
             print_typ_init (flatten typ1) print_typ_init (flatten typ2);
           raise CannotUnify)
        else
          r := Some typ
    | TPred(typ1,_), typ2
    | typ1, TPred(typ2,_) -> unify typ1 typ2
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
    | TInt _,TInt _ -> true
    | TRInt _, TRInt _ -> true
    | TVar{contents=None}, TVar{contents=None} -> true
    | TVar{contents=Some typ1},TVar{contents=Some typ2} -> same_shape typ1 typ2
    | TFun(x1,typ1),TFun(x2,typ2) -> same_shape (Id.typ x1) (Id.typ x2) && same_shape typ1 typ2
    | TList typ1, TList typ2 -> same_shape typ1 typ2
    | TPair(typ11,typ12),TPair(typ21,typ22) -> same_shape typ11 typ21 && same_shape typ12 typ22
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
  | TPair(typ1,typ2) -> is_poly_typ typ1 || is_poly_typ typ2
  | TConstr _ -> false
  | TPred(typ,_) -> is_poly_typ typ


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
    TPair(typ1, _) -> typ1
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
  | TPair(typ1,typ2) -> has_pred typ1 || has_pred typ2
  | TConstr _ -> false
  | TPred(typ,ps) -> has_pred typ || ps <> []

let rec to_id_string = function
    TUnit -> "unit"
  | TBool -> "bool"
  | TAbsBool _ -> assert false
  | TInt -> "int"
  | TRInt _ -> assert false
  | TVar{contents=None} -> "abst"
  | TVar{contents=Some typ} -> to_id_string typ
  | TFun(x,typ) -> to_id_string (Id.typ x) ^ "__" ^ to_id_string typ
  | TList typ -> to_id_string typ ^ "_list"
  | TPair(typ1,typ2) -> to_id_string typ1 ^ "_x_" ^ to_id_string typ2
  | TConstr(s,_) -> s
  | TPred(typ,_) -> to_id_string typ

let rec remove_top_pred = function
    TPred(typ,_) -> typ
  | typ -> typ
