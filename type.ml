
open Util

module Debug = Debug.Make(struct let check () = List.mem "Type" !Flag.debug_module end)

type 'a t =
  | TUnit
  | TBool
  | TInt
  | TVar of 'a t option ref
  | TFun of 'a t Id.t * 'a t
  | TFuns of 'a t Id.t list * 'a t
  | TTuple of 'a t Id.t list
  | TData of string
  | TPred of 'a t Id.t * 'a list
  | TVariant of (string * 'a t list) list
  | TRecord of (string * (mutable_flag * 'a t)) list
  | Type of (string * 'a t) list * string
  | TApp of constr * 'a t list
and mutable_flag = Immutable | Mutable
and constr =
  | TList
  | TRef
  | TOption
  | TArray

exception CannotUnify

let _TFun x typ = TFun(x, typ)

let typ_unknown = TData "???"


let var_name_of typ =
  match typ with
  | TUnit -> "u"
  | TBool -> "b"
  | TInt -> "n"
  | TFun _ -> "f"
  | TTuple _ -> "p"
  | TApp(TList,_) -> "xs"
  | _ -> "x"

let new_var typ = Id.new_var ~name:(var_name_of typ) typ

let make_tfun typ1 typ2 =
  TFun(new_var typ1, typ2)


let make_ttuple typs =
  TTuple (List.map new_var typs)

let make_ttuple' typs =
  match typs with
  | [] -> TUnit
  | [typ] -> typ
  | _ -> make_ttuple typs

let make_tpair typ1 typ2 = make_ttuple [typ1; typ2]

let make_tlist typ = TApp(TList, [typ])
let make_tref typ = TApp(TRef, [typ])
let make_toption typ = TApp(TOption, [typ])
let make_tarray typ = TApp(TArray, [typ])


let is_fun_typ = function
  | TFun(_,_) -> true
  | TFuns(_,_) -> true
  | _ -> false

let rec is_base_typ = function
  | TUnit
  | TBool
  | TInt
  | TData "string" -> true
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
  | TBool -> TBool
  | TInt -> TInt
  | TVar{contents=Some typ} -> elim_tpred_all typ
  | TVar r -> TVar r
  | TFun(x, typ) -> TFun(Id.map_typ elim_tpred_all x, elim_tpred_all typ)
  | TApp(c, typs) -> TApp(c, List.map elim_tpred_all typs)
  | TTuple xs -> TTuple (List.map (Id.map_typ elim_tpred_all) xs)
  | TData s -> TData s
  | TPred(x,_) -> elim_tpred_all @@ Id.typ x
  | TFuns _ -> unsupported "elim_tpred_all"
  | TVariant labels -> TVariant (List.map (Pair.map_snd @@ List.map @@ elim_tpred_all) labels)
  | TRecord fields -> TRecord (List.map (Pair.map_snd @@ Pair.map_snd @@ elim_tpred_all) fields)
  | Type(decls, s) -> Type(List.map (Pair.map_snd @@ elim_tpred_all) decls, s)

let rec decomp_tfun = function
  | TFun(x,typ) ->
      let xs,typ = decomp_tfun typ in
      x :: xs, typ
  | typ -> [], typ

let rec decomp_tfuns = function
  | TFuns(xs, typ) -> xs, typ
  | _ -> invalid_arg "decomp_tfuns"

let arity typ = List.length @@ fst @@ decomp_tfun typ

let rec print occur print_pred fm typ =
  let print' = print occur print_pred in
  let print_preds ps = print_list print_pred "; " ps in
  match typ with
  | TUnit -> Format.fprintf fm "unit"
  | TBool -> Format.fprintf fm "bool"
  | TInt -> Format.fprintf fm "int"
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
  | TTuple xs ->
      let pr fm x =
        if occur x typ then Format.fprintf fm "%a:" Id.print x;
        Format.fprintf fm "%a" print' (Id.typ x)
      in
      Format.fprintf fm "(@[<hov 2>%a@])" (print_list pr "@ *@ ") xs
  | TData s -> Format.pp_print_string fm s
  | TPred(x,ps) -> Format.fprintf fm "@[%a@[<hov 3>[\\%a. %a]@]@]" print' (Id.typ x) Id.print x print_preds ps
  | TVariant labels ->
      let pr fm (s, typs) =
        if typs = [] then
          Format.fprintf fm "%s" s
        else
          Format.fprintf fm "@[%s of %a@]" s (print_list print' "@ *@ ") typs
      in
      Format.fprintf fm "(@[%a@])" (print_list pr "@ |@ ") labels
  | TRecord fields ->
      let pr fm (s, (f, typ)) =
        let sf = if f = Mutable then "mutable " else "" in
        Format.fprintf fm "@[%s%s: %a@]" sf s print' typ
      in
      Format.fprintf fm "{@[%a@]}" (print_list pr "@ |@ ") fields
  | Type(decls, s) ->
      let pr fm (s, typ) =
        Format.fprintf fm "@[%s = %a@]" s print' typ
      in
      Format.fprintf fm "(@[type %a in %s@])" (print_list pr "@ and@ ") decls s
  | TApp(TRef, [typ]) -> Format.fprintf fm "@[%a ref@]" print' typ
  | TApp(TList, [typ]) -> Format.fprintf fm "@[%a list@]" print' typ
  | TApp(TOption, [typ]) -> Format.fprintf fm "@[%a option@]" print' typ
  | TApp(TArray, [typ]) -> Format.fprintf fm "@[%a array@]" print' typ
  | TApp _ -> assert false

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
  | TBool,TBool -> true
  | TInt,TInt -> true
  | TFuns([], typ1), typ2 -> can_unify typ1 typ2
  | typ1, TFuns([], typ2) -> can_unify typ1 typ2
  | TFuns(x::xs, typ1), typ2 -> can_unify (TFun(x, TFuns(xs, typ1))) typ2
  | typ1, TFuns(x::xs, typ2) -> can_unify typ1 (TFun(x, TFuns(xs, typ2)))
  | TFun(x1,typ1),TFun(x2,typ2) -> can_unify (Id.typ x1) (Id.typ x2) && can_unify typ1 typ2
  | TApp(c1, typs1), TApp(c2, typs2) -> List.for_all2 can_unify typs1 typs2
  | TTuple xs1, TTuple xs2 ->
      List.length xs1 = List.length xs2 &&
      List.for_all2 (fun x1 x2 -> can_unify (Id.typ x1) (Id.typ x2)) xs1 xs2
  | TData "event", TFun _ -> true
  | TFun _, TData "event" -> true
  | TVar{contents=None}, _ -> true
  | _, TVar{contents=None} -> true
  | TData s1, TData s2
  | Type(_, s1), Type(_, s2)
  | TData s1, Type(_, s2)
  | Type(_, s1), TData s2 -> s1 = s2
  | Type(decls, s), typ
  | typ, Type(decls, s) -> can_unify (List.assoc s decls) typ
  | TVariant labels1, TVariant labels2 ->
      List.for_all2 (fun (s1,typs1) (s2,typs2) -> s1 = s2 && List.for_all2 can_unify typs1 typs2) labels1 labels2
  | TRecord fields1, TRecord fields2 ->
      List.for_all2 (fun (s1,(f1,typ1')) (s2,(f2,typ2')) -> s1 = s2 && f1 = f2 && can_unify typ1' typ2') fields1 fields2
  | _ -> false


let rec flatten typ =
  match typ with
      TVar{contents = Some typ'} -> flatten typ'
    | _ -> typ

(* just for "unify"? *)
let rec occurs r typ =
  match flatten typ with
  | TUnit -> false
  | TBool -> false
  | TInt -> false
  | TVar({contents=None} as r') -> r == r'
  | TVar{contents=Some typ} -> assert false
  | TFun(x,typ) -> occurs r (Id.typ x) || occurs r typ
  | TApp(_, typs) -> List.exists (occurs r) typs
  | TTuple xs -> List.exists (occurs r -| Id.typ) xs
  | TData _ -> false
  | TPred(x,_) -> occurs r (Id.typ x)
  | TFuns _ -> unsupported ""
  | TVariant labels -> List.exists (snd |- List.exists @@ occurs r) labels
  | TRecord fields -> List.exists (snd |- snd |- occurs r) fields
  | Type(decls, s) -> List.exists (snd |- occurs r) decls

let rec data_occurs s typ =
  match flatten typ with
  | TUnit -> false
  | TBool -> false
  | TInt -> false
  | TVar r -> Option.exists (data_occurs s) !r
  | TFun(x,typ) -> data_occurs s (Id.typ x) || data_occurs s typ
  | TApp(_, typs) -> List.exists (data_occurs s) typs
  | TTuple xs -> List.exists (data_occurs s -| Id.typ) xs
  | TData s' -> s = s'
  | TPred(x,_) -> data_occurs s (Id.typ x)
  | TFuns _ -> unsupported ""
  | TVariant labels -> List.exists (snd |- List.exists @@ data_occurs s) labels
  | TRecord fields -> List.exists (snd |- snd |- data_occurs s) fields
  | Type(decls, _) -> List.exists (snd |- data_occurs s) decls


let rec unify typ1 typ2 =
  match flatten typ1, flatten typ2 with
  | TUnit, TUnit
  | TBool, TBool
  | TInt, TInt -> ()
  | TFun(x1, typ1), TFun(x2, typ2) ->
      unify (Id.typ x1) (Id.typ x2);
      unify typ1 typ2
  | TApp(_,typs1), TApp(_,typs2) -> List.iter2 unify typs1 typs2
  | TTuple xs1, TTuple xs2 ->
      List.iter2 (fun x1 x2 -> unify (Id.typ x1) (Id.typ x2)) xs1 xs2
  | TVar r1, TVar r2 when r1 == r2 -> ()
  | TVar({contents = None} as r), typ
  | typ, TVar({contents = None} as r) ->
      if occurs r typ then
        (Format.printf "occurs check failure: %a, %a@." print_typ_init (flatten typ1) print_typ_init (flatten typ2);
         raise CannotUnify)
      else
        r := Some typ
  | TPred(x,_), typ
  | typ, TPred(x,_) -> unify (Id.typ x) typ
  | TData s1, TData s2 -> assert (s1 = s2)
  | Type(_,s1), Type(_,s2) -> assert (s1 = s2)
  | TVariant labels1, TVariant labels2 -> List.iter2 (fun (s1,typs1) (s2,typs2) -> assert (s1 = s2); List.iter2 unify typs1 typs2) labels1 labels2
  | TRecord fields1, TRecord fields2 -> List.iter2 (fun (s1,(f1,typ1)) (s2,(f2,typ2)) -> assert (s1 = s2 && f1 = f2); unify typ1 typ2) fields1 fields2
  | _ ->
      Format.printf "unification error: %a, %a@." print_typ_init (flatten typ1) print_typ_init (flatten typ2);
      raise CannotUnify


let rec same_shape typ1 typ2 =
  match elim_tpred typ1, elim_tpred typ2 with
  | TUnit,TUnit -> true
  | TBool,TBool -> true
  | TInt,TInt -> true
  | TVar{contents=None}, TVar{contents=None} -> true
  | TVar{contents=Some typ1},TVar{contents=Some typ2} -> same_shape typ1 typ2
  | TFun(x1,typ1),TFun(x2,typ2) -> same_shape (Id.typ x1) (Id.typ x2) && same_shape typ1 typ2
  | TApp(c1,typs1), TApp(c2,typs2) -> c1=c2 && List.for_all2 same_shape typs1 typs2
  | TTuple xs1, TTuple xs2 ->
      List.length xs1 = List.length xs2
      && List.for_all2 (fun x1 x2 -> same_shape (Id.typ x1) (Id.typ x2)) xs1 xs2
  | TData s1, TData s2 -> s1 = s2
  | Type(_, s1), Type(_, s2) -> s1 = s2
  | TVariant labels1, TVariant labels2 ->
      List.eq ~eq:(Compare.eq_on snd ~eq:(List.eq ~eq:same_shape)) labels1 labels2
  | TRecord fields1, TRecord fields2 -> unsupported "same_shape"
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
  | typ' -> invalid_arg @@ Format.asprintf "proj_typ %d (%a)" i print_typ_init typ'

let fst_typ typ = proj_typ 0 typ
let snd_typ typ = proj_typ 1 typ

let ref_typ typ =
  match elim_tpred typ with
  | TApp(TRef, [typ]) -> typ
  | typ when typ = typ_unknown -> typ_unknown
  | _ -> invalid_arg "ref_typ"

let list_typ typ =
  match elim_tpred typ with
  | TApp(TList, [typ]) -> typ
  | typ when typ = typ_unknown -> typ_unknown
  | _ -> invalid_arg "list_typ"

let option_typ typ =
  match elim_tpred typ with
  | TApp(TOption, [typ]) -> typ
  | typ when typ = typ_unknown -> typ_unknown
  | _ -> invalid_arg "option_typ"

let array_typ typ =
  match elim_tpred typ with
  | TApp(TArray, [typ]) -> typ
  | typ when typ = typ_unknown -> typ_unknown
  | _ -> invalid_arg "array_typ"

let rec has_pred = function
  | TUnit -> false
  | TBool -> false
  | TInt -> false
  | TVar{contents=None} -> false
  | TVar{contents=Some typ} -> has_pred typ
  | TFun(x,typ) -> has_pred (Id.typ x) || has_pred typ
  | TApp(_,typs) -> List.exists has_pred typs
  | TTuple xs -> List.exists (has_pred -| Id.typ) xs
  | TData _ -> false
  | TPred(x,ps) -> has_pred (Id.typ x) || ps <> []
  | TFuns _ -> unsupported ""
  | TVariant labels -> List.exists (snd |- List.exists has_pred) labels
  | TRecord fields -> List.exists (snd |- snd |- has_pred) fields
  | Type(decls, _) -> List.exists (snd |- has_pred) decls

let rec to_id_string = function
  | TUnit -> "unit"
  | TBool -> "bool"
  | TInt -> "int"
  | TVar{contents=None} -> "abst"
  | TVar{contents=Some typ} -> to_id_string typ
  | TFun(x,typ) -> to_id_string (Id.typ x) ^ "__" ^ to_id_string typ
  | TApp(TList, [typ]) -> to_id_string typ ^ "_list"
  | TTuple xs ->
      let xs',x = List.decomp_snoc xs in
      let aux x s = to_id_string (Id.typ x) ^ "_x_" ^ s in
      List.fold_right aux xs' @@ to_id_string @@ Id.typ x
  | TData s -> s
  | TPred(x,_) -> to_id_string (Id.typ x)
  | TApp(TRef, [typ]) -> to_id_string typ ^ "_ref"
  | TApp(TOption, [typ]) -> to_id_string typ ^ "_option"
  | TApp(TArray, [typ]) -> to_id_string typ ^ "_array"
  | TApp _ -> assert false
  | TFuns _ -> unsupported ""
  | TVariant labels -> String.join "_" @@ List.map fst labels
  | TRecord fields -> String.join "_" @@ List.map fst fields
  | Type(_, s) -> s


(* order of simpl types *)
let rec order typ =
  match typ with
  | TUnit -> 0
  | TBool -> 0
  | TInt -> 0
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

let rec decomp_trecord typ =
  match typ with
  | TRecord fields -> fields
  | Type(decls, s) -> decomp_trecord @@ List.assoc s decls
  | _ -> invalid_arg "decomp_trecord"



let rec get_free_data_name typ =
  match typ with
  | TUnit -> []
  | TBool -> []
  | TInt -> []
  | TVar {contents=Some typ} -> get_free_data_name typ
  | TVar {contents=None} -> []
  | TFun(x, typ) -> get_free_data_name (Id.typ x) @ get_free_data_name typ
  | TApp(_, typs) -> List.flatten_map get_free_data_name typs
  | TTuple xs -> List.flatten_map (Id.typ |- get_free_data_name) xs
  | TData s -> [s]
  | TPred(x,_) -> get_free_data_name @@ Id.typ x
  | TFuns _ -> unsupported "elim_tpred_all"
  | TVariant labels -> List.flatten_map (snd |- List.flatten_map get_free_data_name) labels
  | TRecord fields -> List.flatten_map (snd |- snd |- get_free_data_name) fields
  | Type(decls, s) -> List.filter_out ((=) s) @@ List.flatten_map (snd |- get_free_data_name) decls
let get_free_data_name typ = List.unique @@ get_free_data_name typ


let rec is_mutable_record typ =
  match typ with
  | TRecord fields ->
      List.exists (fun (_,(f,_)) -> f = Mutable) fields
  | Type(decls, s) -> is_mutable_record @@ List.assoc s decls
  | _ -> invalid_arg "is_mutable_record"
