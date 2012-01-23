
open Utilities

type 'a t =
    TUnit
  | TBool
  | TAbsBool
  | TInt of 'a list
  | TRInt of 'a
  | TVar of 'a t option ref
  | TFun of ('a t Id.t) * 'a t
  | TList of 'a t * 'a list
  | TPair of 'a t * 'a t
  | TConstr of string * bool
  | TUnknown
  | TVariant of 'a t
(*
  | TLabel of 'a t Id.t * 'a t
*)
  | TPred of 'a t Id.t * 'a t



let is_base_typ = function
    TUnit
  | TBool
  | TAbsBool
  | TInt _
  | TRInt _ -> true
  | _ -> false


let rec decomp_tfun = function
    TFun(x,typ) ->
      let xs,typ = decomp_tfun typ in
        x :: xs, typ
  | typ -> [], typ

let rec can_unify typ1 typ2 =
  match typ1,typ2 with
      TVar{contents=Some typ1},typ2
    | typ1,TVar{contents=Some typ2} -> can_unify typ1 typ2
    | TUnit,TUnit -> true
    | (TBool|TAbsBool),(TBool|TAbsBool) -> true
    | TInt _,TInt _ -> true
    | TRInt _,TRInt _ -> true
    | TFun(x1,typ1),TFun(x2,typ2) -> can_unify (Id.typ x1) (Id.typ x2) && can_unify typ1 typ2
    | TList(typ1,_), TList(typ2,_) -> can_unify typ1 typ2
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
    | TUnknown, _ -> true
    | _, TUnknown -> true
    | TVar{contents=None}, _ -> true
    | _, TVar{contents=None} -> true
    | _ -> false


let rec print print_pred fm typ =
  let print = print print_pred in
  let print_preds = print_preds print_pred in
    match typ with
        TUnit -> Format.fprintf fm "unit"
      | TAbsBool -> Format.fprintf fm "abool"
      | TBool -> Format.fprintf fm "bool"
      | TInt [] -> Format.fprintf fm "int"
      | TInt ps -> Format.fprintf fm "int[%a]" print_preds ps
      | TRInt p -> assert false (*Format.fprintf fm "{ %a | %a }" Id.print abst_var print_preds [p]*)
      | TVar{contents=Some typ} -> print fm typ
      | TVar _ -> Format.fprintf fm "!!!"
      | TFun(x, typ) ->
          if Id.to_string' x = ""
          then Format.fprintf fm "(%a -> %a)" print (Id.typ x) print typ
          else Format.fprintf fm "(%a:%a -> %a)" Id.print x print (Id.typ x) print typ
      | TUnknown -> Format.fprintf fm "???"
      | TList(typ,[]) -> Format.fprintf fm "%a list" print typ
      | TList(typ,ps) -> Format.fprintf fm "%a list[%a]" print typ print_preds ps
      | TPair(typ1,typ2) -> Format.fprintf fm "(%a * %a)" print typ1 print typ2
      | TVariant _ -> assert false
(*
      | TVariant ctypss ->
          let rec aux fm = function
              [] -> ()
            | (c,typs)::ctypss ->
                let rec aux' fm = function
                    [] -> ()
                  | [typ] -> print fm typ
                  | typ::typs -> Format.fprintf fm "%a * %a" print typ aux' typs
                in
                let s_of = if typs = [] then "" else " of " in
                let bar = if ctypss = [] then "" else " | " in
                  Format.fprintf fm "%s%s%a%s%a" c s_of aux' typs bar aux ctypss
          in
            aux fm ctypss
      | TRecord(b,typs) ->
          let rec aux fm = function
              [] -> ()
            | (s,(f,typ))::fields ->
                match is_int s, fields=[] with
                    true,true -> Format.fprintf fm "%a" print typ
                  | true,false -> Format.fprintf fm "%a * %a" print typ aux fields
                  | false,true -> Format.fprintf fm "%s:%a" s print typ
                  | false,false -> Format.fprintf fm "%s:%a; %a" s print typ aux fields
          in
            if b
            then Format.fprintf fm "(%a)" aux typs
            else Format.fprintf fm "{%a}" aux typs
*)
      | TConstr(s,_) -> Format.pp_print_string fm s
      | TPred(x,typ) -> Format.fprintf fm "(%a|[%a])" print typ Id.print x

and print_preds print_pred = print_list print_pred ";" false


let rec flatten typ =
  match typ with
      TVar{contents = Some typ'} -> flatten typ'
    | _ -> typ

let rec occurs r typ =
  match flatten typ with
      TUnit -> false
    | TBool -> false
    | TAbsBool -> false
    | TInt ps -> assert (ps = []); false
    | TRInt p -> assert false
    | TVar({contents=None} as r') -> r == r'
    | TVar{contents=Some typ} -> assert false
    | TFun(x,typ) -> occurs r (Id.typ x) || occurs r typ
    | TList(typ,ps) -> assert (ps = []); occurs r typ
    | TPair(typ1,typ2) -> occurs r typ1 || occurs r typ2
    | TConstr(s,b) -> false
    | TUnknown -> false
    | TVariant _ -> assert false

exception CannotUnify

let rec unify typ1 typ2 =
  let print_typ = print (fun _ -> assert false) in
    match flatten typ1, flatten typ2 with
        TUnit, TUnit
      | TBool, TBool
      | TInt _, TInt _ -> ()
      | TRInt _, TRInt _ -> ()
      | TFun(x1, typ1), TFun(x2, typ2) ->
          unify (Id.typ x1) (Id.typ x2);
          unify typ1 typ2
      | TList(typ1,_), TList(typ2,_) -> unify typ1 typ2
      | TPair(typ11,typ12), TPair(typ21,typ22) ->
          unify typ11 typ21;
          unify typ12 typ22
      | TVar r1, TVar r2 when r1 == r2 -> ()
      | TVar({contents = None} as r), typ
      | typ, TVar({contents = None} as r) ->
          if occurs r typ then
            (Format.printf "occurs check failure: %a, %a@." print_typ (flatten typ1) print_typ (flatten typ2);
             raise CannotUnify)
          else
            r := Some typ
      | _ ->
          if Flag.debug
          then Format.printf "unification error: %a, %a@." print_typ (flatten typ1) print_typ (flatten typ2);
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
    | TList(typ1,_),TList(typ2,_) -> same_shape typ1 typ2
    | TPair(typ11,typ12),TPair(typ21,typ22) -> same_shape typ11 typ21 && same_shape typ12 typ22
    | TConstr(s1,_),TConstr(s2,_) -> s1 = s2
    | TUnknown, TUnknown -> true
    | TVariant _,_ -> assert false
    | _,TVariant _ -> assert false
    | _ -> assert false


let rec is_poly_typ = function
    TUnit -> false
  | TBool -> false
  | TAbsBool -> false
  | TInt _ -> false
  | TRInt _ -> false
  | TVar{contents=None} -> true
  | TVar{contents=Some typ} -> is_poly_typ typ
  | TFun(x,typ) -> is_poly_typ (Id.typ x) || is_poly_typ typ
  | TList(typ,_) -> is_poly_typ typ
  | TPair(typ1,typ2) -> is_poly_typ typ1 || is_poly_typ typ2
  | TConstr _ -> false
  | TUnknown _ -> assert false
  | TVariant _ -> assert false


let rec copy = function
    TVar {contents = Some typ} -> copy typ
  | TVar {contents = None} -> TVar (ref None)
  | typ -> typ



let rec app_typ typ typs =
  match typ,typs with
    | TFun(_,typ2), _::typs' -> app_typ typ2 typs'
    | _, [] -> typ
    | _ -> assert false
