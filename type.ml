
open Util

type 'a t =
  | TUnit
  | TBool
  | TAbsBool
  | TInt
  | TRInt of 'a
  | TVar of 'a t option ref
  | TFun of 'a t Id.t * 'a t
  | TFuns of 'a t Id.t list * 'a t
  | TList of 'a t
  | TTuple of 'a t Id.t list
  | TData of string * bool
  | TRef of 'a t
  | TOption of 'a t
  | TPred of 'a t Id.t * 'a list
(*| TLabel of 'a t Id.t * 'a t*)

exception CannotUnify

let _TFun x typ = TFun(x, typ)

let typ_unknown = TData("???", false)

let is_fun_typ = function
  | TFun(_,_) -> true
  | TFuns(_,_) -> true
  | _ -> false

let rec is_base_typ = function
  | TUnit
  | TBool
  | TAbsBool
  | TInt
  | TRInt _ -> true
  | TPred(x,_) -> is_base_typ @@ Id.typ x
  | _ -> false

let elim_tpred = function
  | TPred(x,_) -> Id.typ x
  | typ -> typ

let tfuns_to_tfun = function
  | TFuns(xs,typ) -> List.fold_right _TFun xs typ
  | typ -> typ

let rec elim_tpred_all = function
  | TUnit -> TUnit
  | TAbsBool -> TAbsBool
  | TBool -> TBool
  | TInt -> TInt
  | TRInt p -> TRInt p
  | TVar{contents=Some typ} -> elim_tpred_all typ
  | TVar r -> TVar r
  | TFun(x, typ) -> TFun(Id.map_typ elim_tpred_all x, elim_tpred_all typ)
  | TList typ -> TList (elim_tpred_all typ)
  | TTuple xs -> TTuple (List.map (Id.map_typ elim_tpred_all) xs)
  | TData(s,b) -> TData(s,b)
  | TPred(x,_) -> elim_tpred_all @@ Id.typ x
  | TRef typ -> TRef (elim_tpred_all typ)
  | TOption typ -> TOption (elim_tpred_all typ)
  | TFuns _ -> unsupported ""

let rec decomp_tfun = function
  | TFun(x,typ) ->
      let xs,typ = decomp_tfun typ in
      x :: xs, typ
  | typ -> [], typ

let rec decomp_tfuns = function
  | TFuns(xs, typ) -> xs, typ
  | _ -> invalid_arg "decomp_tfuns"

let rec print occur print_pred fm typ =
  let print' = print occur print_pred in
  let print_preds ps = print_list print_pred "; " ps in
  match typ with
  | TUnit -> Format.fprintf fm "unit"
  | TAbsBool -> Format.fprintf fm "abool"
  | TBool -> Format.fprintf fm "bool"
  | TInt -> Format.fprintf fm "int"
  | TRInt p -> assert false (*Format.fprintf fm "{ %a | %a }" Id.print abst_var print_preds [p]*)
  | TVar{contents=Some typ} -> print' fm typ
  | TVar _ -> Format.fprintf fm "!!!"
  | TFun _ ->
      let rec aux fm (xs, typ) =
        match xs with
        | [] -> print' fm typ
        | x::xs' ->
            if occur x typ || List.exists (occur x) (List.map Id.typ xs)
            then Format.fprintf fm "@[<hov 2>%a:%a ->@ %a@]" Id.print x print' (Id.typ x) aux (xs',typ)
            else Format.fprintf fm "@[<hov 2>%a ->@ %a@]" print' (Id.typ x) aux (xs',typ)
      in
      Format.fprintf fm "(%a)" aux @@ decomp_tfun typ
  | TFuns(xs,typ) ->
      let rec aux fm (xs, typ) =
        match xs with
        | [] -> Format.printf "[%a]" print' typ
        | x::xs' ->
            if occur x typ || List.exists (occur x) (List.map Id.typ xs)
            then Format.fprintf fm "@[<hov 2>%a:%a ->@ %a@]" Id.print x print' (Id.typ x) aux (xs',typ)
            else Format.fprintf fm "@[<hov 2>%a ->@ %a@]" print' (Id.typ x) aux (xs',typ)
      in
      Format.fprintf fm "(%a)" aux (xs, typ)
  | TList typ -> Format.fprintf fm "@[%a list@]" print' typ
  | TTuple xs ->
      let pr fm x =
        if occur x typ then Format.fprintf fm "%a:" Id.print x;
        Format.fprintf fm "%a" print' (Id.typ x)
      in
      Format.fprintf fm "(@[<hov 2>%a@])" (print_list pr "@ *@ ") xs
  | TData(s,_) -> Format.pp_print_string fm s
  | TRef typ -> Format.fprintf fm "@[%a ref@]" print' typ
  | TPred(x,ps) -> Format.fprintf fm "@[%a@[<hov 3>[\\%a. %a]@]@]" print' (Id.typ x) Id.print x print_preds ps
  | TOption typ -> Format.fprintf fm "@[%a option@]" print' typ

let print ?(occur=fun _ _ -> false) print_pred fm typ =
  Format.fprintf fm "@[%a@]" (print occur print_pred) typ
let print_typ_init typ = print (fun _ -> assert false) typ

let rec can_unify typ1 typ2 =
  match typ1,typ2 with
  | TVar{contents=Some typ1}, typ2
  | typ1, TVar{contents=Some typ2} -> can_unify typ1 typ2
  | TPred(x,_), typ
  | typ, TPred(x,_) -> can_unify (Id.typ x) typ
  | _ when typ1 = typ_unknown || typ2 = typ_unknown -> true
  | TUnit,TUnit -> true
  | (TBool|TAbsBool),(TBool|TAbsBool) -> true
  | (TInt|TRInt _),(TInt|TRInt _) -> true
  | TFuns([], typ1), typ2 -> can_unify typ1 typ2
  | typ1, TFuns([], typ2) -> can_unify typ1 typ2
  | TFuns(x::xs, typ1), typ2 -> can_unify (TFun(x, TFuns(xs, typ1))) typ2
  | typ1, TFuns(x::xs, typ2) -> can_unify typ1 (TFun(x, TFuns(xs, typ2)))
  | TFun(x1,typ1),TFun(x2,typ2) -> can_unify (Id.typ x1) (Id.typ x2) && can_unify typ1 typ2
  | TList typ1, TList typ2 -> can_unify typ1 typ2
  | TRef typ1, TRef typ2 -> can_unify typ1 typ2
  | TOption typ1, TOption typ2 -> can_unify typ1 typ2
  | TTuple xs1, TTuple xs2 ->
      List.length xs1 = List.length xs2 &&
      List.for_all2 (fun x1 x2 -> can_unify (Id.typ x1) (Id.typ x2)) xs1 xs2
  | TData("event",_), TFun _ -> true
  | TFun _, TData("event",_) -> true
  | TVar{contents=None}, _ -> true
  | _, TVar{contents=None} -> true
  | TData(s1,_),TData(s2,_) -> s1 = s2
  | _ -> false


let rec flatten typ =
  match typ with
      TVar{contents = Some typ'} -> flatten typ'
    | _ -> typ

let rec occurs r typ =
  match flatten typ with
  | TUnit -> false
  | TBool -> false
  | TAbsBool -> false
  | TInt -> false
  | TRInt p -> assert false
  | TVar({contents=None} as r') -> r == r'
  | TVar{contents=Some typ} -> assert false
  | TFun(x,typ) -> occurs r (Id.typ x) || occurs r typ
  | TList typ -> occurs r typ
  | TTuple xs -> List.exists (occurs r -| Id.typ) xs
  | TData(s,b) -> false
  | TPred(x,_) -> occurs r (Id.typ x)
  | TRef typ -> occurs r typ
  | TOption typ -> occurs r typ
  | TFuns _ -> unsupported ""


let rec unify typ1 typ2 =
  match flatten typ1, flatten typ2 with
  | TUnit, TUnit
  | TBool, TBool
  | TInt, TInt -> ()
  | TRInt _, TRInt _ -> ()
  | TFun(x1, typ1), TFun(x2, typ2) ->
      unify (Id.typ x1) (Id.typ x2);
      unify typ1 typ2
  | TList typ1, TList typ2 -> unify typ1 typ2
  | TRef typ1, TRef typ2 -> unify typ1 typ2
  | TOption typ1, TOption typ2 -> unify typ1 typ2
  | TTuple xs1, TTuple xs2 ->
      List.iter2 (fun x1 x2 -> unify (Id.typ x1) (Id.typ x2)) xs1 xs2
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
  | TData(s1,_), TData(s2,_) -> assert (s1 = s2)
  | _ ->
      Format.printf "unification error: %a, %a@."
                    print_typ_init (flatten typ1) print_typ_init (flatten typ2);
      raise CannotUnify


let rec same_shape typ1 typ2 =
  match elim_tpred typ1, elim_tpred typ2 with
  | TUnit,TUnit -> true
  | TBool,TBool -> true
  | TAbsBool,TAbsBool -> true
  | TInt,TInt -> true
  | TRInt _, TRInt _ -> true
  | TVar{contents=None}, TVar{contents=None} -> true
  | TVar{contents=Some typ1},TVar{contents=Some typ2} -> same_shape typ1 typ2
  | TFun(x1,typ1),TFun(x2,typ2) -> same_shape (Id.typ x1) (Id.typ x2) && same_shape typ1 typ2
  | TList typ1, TList typ2 -> same_shape typ1 typ2
  | TTuple xs1, TTuple xs2 ->
      List.length xs1 = List.length xs2
      && List.for_all2 (fun x1 x2 -> same_shape (Id.typ x1) (Id.typ x2)) xs1 xs2
  | TData(s1,_),TData(s2,_) -> s1 = s2
  | _ -> false





let rec copy = function
  | TVar {contents = Some typ} -> copy typ
  | TVar {contents = None} -> TVar (ref None)
  | typ -> typ



let rec app_typ typ typs =
  match typ,typs with
    | TFun(_,typ2), _::typs' -> app_typ typ2 typs'
    | _, [] -> typ
    | _ -> assert false

let tuple_num typ =
  match elim_tpred typ with
  | TTuple xs -> Some (List.length xs)
  | _ -> None

let proj_typ i typ =
  match elim_tpred typ with
  | TTuple xs -> Id.typ @@ List.nth xs i
  | typ when typ = typ_unknown -> typ_unknown
  | _ -> assert false

let fst_typ typ = proj_typ 0 typ
let snd_typ typ = proj_typ 1 typ

let ref_typ typ =
  match elim_tpred typ with
  | TRef typ -> typ
  | typ when typ = typ_unknown -> typ_unknown
  | _ -> assert false

let list_typ typ =
  match elim_tpred typ with
  | TList typ -> typ
  | typ when typ = typ_unknown -> typ_unknown
  | _ -> assert false

let option_typ typ =
  match elim_tpred typ with
  | TOption typ -> typ
  | typ when typ = typ_unknown -> typ_unknown
  | _ -> assert false

let rec has_pred = function
  | TUnit -> false
  | TBool -> false
  | TAbsBool -> false
  | TInt -> false
  | TRInt _ -> assert false
  | TVar{contents=None} -> false
  | TVar{contents=Some typ} -> has_pred typ
  | TFun(x,typ) -> has_pred (Id.typ x) || has_pred typ
  | TList typ -> has_pred typ
  | TTuple xs -> List.exists (has_pred -| Id.typ) xs
  | TData _ -> false
  | TPred(x,ps) -> has_pred (Id.typ x) || ps <> []
  | TRef typ -> has_pred typ
  | TOption typ -> has_pred typ
  | TFuns _ -> unsupported ""

let rec to_id_string = function
  | TUnit -> "unit"
  | TBool -> "bool"
  | TAbsBool -> assert false
  | TInt -> "int"
  | TRInt _ -> assert false
  | TVar{contents=None} -> "abst"
  | TVar{contents=Some typ} -> to_id_string typ
  | TFun(x,typ) -> to_id_string (Id.typ x) ^ "__" ^ to_id_string typ
  | TList typ -> to_id_string typ ^ "_list"
  | TTuple xs ->
      let xs',x = List.decomp_snoc xs in
      let aux x s = to_id_string (Id.typ x) ^ "_x_" ^ s in
      List.fold_right aux xs' @@ to_id_string @@ Id.typ x
  | TData(s,_) -> s
  | TPred(x,_) -> to_id_string (Id.typ x)
  | TRef typ -> to_id_string typ ^ "_ref"
  | TOption typ -> to_id_string typ ^ "_option"
  | TFuns _ -> unsupported ""


(* order of simpl types *)
let rec order typ =
  match typ with
  | TUnit -> 0
  | TBool -> 0
  | TAbsBool -> 0
  | TInt -> 0
  | TRInt _ -> 0
  | TVar{contents=None} -> assert false
  | TVar{contents=Some typ} -> order typ
  | TFun(x,typ) -> max (order (Id.typ x) + 1) (order typ)
  | TTuple xs -> List.fold_left (fun m x -> max m (order @@ Id.typ x)) 0 xs
  | TPred(x,_) -> order @@ Id.typ x
  | _ -> assert false

let arg_var typ =
  match typ with
  | TFun(x,_) -> x
  | _ -> invalid_arg "arg_var"

let result_typ typ =
  match typ with
  | TFun(_,typ') -> typ'
  | _ -> invalid_arg "result_typ"

let decomp_ttuple typ =
  match typ with
  | TTuple xs -> List.map Id.typ xs
  | _ -> invalid_arg "decomp_ttuple"
