open Util

module Debug = Debug.Make(struct let check = make_debug_check __MODULE__ end)

type base =
  | TUnit
  | TBool
  | TInt
  | TPrim of string
and 'a t =
  | TBase of base
  | TVar of 'a t option ref * int option
  | TFun of 'a t Id.t * 'a t
  | TFuns of 'a t Id.t list * 'a t (* Just for fair-termination *)
  | TTuple of 'a t Id.t list
  | TData of string
  | TVariant of bool * (string * 'a t list) list (** true means polymorphic variant *)
  | TRecord of (string * (mutable_flag * 'a t)) list
  | TApp of constr * 'a t list
  | TAttr of 'a attr list * 'a t
  | TModule of (string * 'a t) list
and mutable_flag = Immutable | Mutable
and constr =
  | TList
  | TRef
  | TOption
  | TArray
  | TLazy
and 'a attr =
  | TAPred of 'a t Id.t * 'a list (* TAPred occur at most ones *)
  | TAPureFun
  | TAEffect of effect
  | TAAssumePredTrue
and effect = EVar of int | ENone | ECont | EExcep

exception CannotUnify

let _TFun x typ = TFun(x, typ)
let _TAttr attr typ =
  if attr = [] then
    typ
  else
    match typ with
    | TAttr(attr', typ') -> TAttr(attr@attr', typ')
    | _ -> TAttr(attr, typ)

let pureTFun(x,typ) = TAttr([TAPureFun], TFun(x,typ))

let typ_unknown = TData "???"


let var_name_of typ =
  match typ with
  | TBase TUnit -> "u"
  | TBase TBool -> "b"
  | TBase TInt -> "n"
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
  | [] -> TBase TUnit
  | [typ] -> typ
  | _ -> make_ttuple typs

let make_tpair typ1 typ2 = make_ttuple [typ1; typ2]

let make_tlist typ = TApp(TList, [typ])
let make_tref typ = TApp(TRef, [typ])
let make_toption typ = TApp(TOption, [typ])
let make_tarray typ = TApp(TArray, [typ])


let elim_tattr = function
  | TAttr(_,typ) -> typ
  | typ -> typ

let is_fun_typ typ =
  match elim_tattr typ with
  | TFun(_,_) -> true
  | TFuns(_,_) -> true
  | _ -> false

let rec is_base_typ = function
  | TBase _
  | TData "string" -> true
  | TAttr(_,typ) -> is_base_typ typ
  | _ -> false

let tfuns_to_tfun = function
  | TFuns(xs,typ) -> List.fold_right _TFun xs typ
  | typ -> typ

let rec elim_tattr_all ty =
  match ty with
  | TBase b -> TBase b
  | TVar({contents=Some typ},_) -> elim_tattr_all typ
  | TVar(r,id) -> TVar(r,id)
  | TFun(x, typ) -> TFun(Id.map_typ elim_tattr_all x, elim_tattr_all typ)
  | TApp(c, typs) -> TApp(c, List.map elim_tattr_all typs)
  | TTuple xs -> TTuple (List.map (Id.map_typ elim_tattr_all) xs)
  | TData s -> TData s
  | TAttr(_,typ) -> elim_tattr_all typ
  | TFuns _ -> unsupported "elim_tattr_all"
  | TVariant(poly,labels) -> TVariant(poly, List.map (Pair.map_snd @@ List.map @@ elim_tattr_all) labels)
  | TRecord fields -> TRecord (List.map (Pair.map_snd @@ Pair.map_snd @@ elim_tattr_all) fields)
  | TModule sgn -> TModule (List.map (Pair.map_snd @@ elim_tattr_all) sgn)

let rec decomp_tfun typ =
  match elim_tattr typ with
  | TFun(x,typ) ->
      let xs,typ = decomp_tfun typ in
      x :: xs, typ
  | _ -> [], typ

let rec decomp_tfuns = function
  | TFuns(xs, typ) -> xs, typ
  | _ -> invalid_arg "decomp_tfuns"

let arity typ = List.length @@ fst @@ decomp_tfun typ

let rec decomp_effect = function
  | TAttr(attrs, typ) ->
      let attrs1,attrs2 = List.partition (function TAEffect _ -> true | _ -> false) attrs in
      let typ' =
        if attrs2 = [] then
          typ
        else
          TAttr(attrs2, typ)
      in
      begin
        match attrs1 with
        | [] -> None
        | [TAEffect e] -> Some (e, typ')
        | _ -> invalid_arg "decomp_effect"
      end
  | _ -> None

let print_effect fm e =
  match e with
  | EVar n -> Format.fprintf fm "'e%d" n
  | ENone -> Format.fprintf fm "none"
  | ECont -> Format.fprintf fm "cont"
  | EExcep -> Format.fprintf fm "excep"

let print_tvar fm n =
  let c = char_of_int @@ int_of_char 'a' + n mod 26 in
  let d = if n < 26 then "" else string_of_int (n/26) in
  Format.fprintf fm "'%c%s" c d

let rec print occur print_pred fm typ =
  let print' = print occur print_pred in
  let print_preds ps = print_list print_pred "; " ps in
  match typ with
  | TBase TUnit -> Format.fprintf fm "unit"
  | TBase TBool -> Format.fprintf fm "bool"
  | TBase TInt -> Format.fprintf fm "int"
  | TBase (TPrim s) -> Format.fprintf fm "%s" s
  | TVar({contents=Some typ},_) -> print' fm typ
  | TVar({contents=None}, None) -> Format.fprintf fm "!!!"
  | TVar({contents=None}, Some n) -> print_tvar fm n
  | TFun _ ->
      let rec aux fm (xs, typ) =
        match xs with
        | [] ->
            print' fm typ
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
        print' fm @@ Id.typ x
      in
      Format.fprintf fm "(@[<hov 2>%a@])" (print_list pr "@ *@ ") xs
  | TData s -> Format.pp_print_string fm s
  | TAttr([], typ) -> print' fm typ
  | TAttr(TAPred(x,ps)::preds, typ) -> Format.fprintf fm "@[%a@[<hov 3>[\\%a. %a]@]@]" print' (TAttr(preds,typ)) Id.print x print_preds ps
  | TAttr([TAPureFun], TFun(x,typ)) ->
      if occur x typ
      then Format.fprintf fm "(@[<hov 2>%a:%a -+>@ %a@])" Id.print x print' (Id.typ x) print' typ
      else Format.fprintf fm "(@[<hov 2>%a -+>@ %a@])" print' (Id.typ x) print' typ
  | TAttr([TAEffect e], typ) ->
      Format.fprintf fm "(@[%a # %a@])" print' typ print_effect e
  | TAttr([TAAssumePredTrue], typ) ->
      if is_base_typ typ then
        Format.fprintf fm "@[%a^T@]" print' typ
      else
        Format.fprintf fm "@[(%a)^T@]" print' typ
  | TAttr _ -> unsupported "Type.print TAttr"
  | TVariant(poly,labels) ->
      let pr fm (s, typs) =
        let s' = if poly then "`" ^ s else s in
        if typs = [] then
          Format.fprintf fm "%s" s'
        else
          Format.fprintf fm "@[%s of %a@]" s' (print_list print' "@ *@ ") typs
      in
      Format.fprintf fm "(@[%a@])" (print_list pr "@ | ") labels
  | TRecord fields ->
      let pr fm (s, (f, typ)) =
        let sf = if f = Mutable then "mutable " else "" in
        Format.fprintf fm "@[%s%s: %a@]" sf s print' typ
      in
      Format.fprintf fm "{@[%a@]}" (print_list pr ";@ ") fields
  | TModule _ -> Format.fprintf fm "@[sig ...@ end@]"
  | TApp(TRef, [typ]) -> Format.fprintf fm "@[%a ref@]" print' typ
  | TApp(TList, [typ]) -> Format.fprintf fm "@[%a list@]" print' typ
  | TApp(TOption, [typ]) -> Format.fprintf fm "@[%a option@]" print' typ
  | TApp(TArray, [typ]) -> Format.fprintf fm "@[%a array@]" print' typ
  | TApp(TLazy, [typ]) -> Format.fprintf fm "@[%a Lazy.t@]" print' typ
  | TApp _ -> assert false

let print ?(occur=fun _ _ -> false) print_pred fm typ =
  Format.fprintf fm "@[%a@]" (print occur print_pred) typ
let print_init typ = print (fun _ -> assert false) typ

let rec decomp_module s =
  List.decomp_snoc @@ String.split_on_char '.' s

let base = snd -| decomp_module

let rec can_unify typ1 typ2 =
  match elim_tattr typ1, elim_tattr typ2 with
  | TVar({contents=Some typ1},_), typ2
  | typ1, TVar({contents=Some typ2},_) -> can_unify typ1 typ2
  | _ when typ1 = typ_unknown || typ2 = typ_unknown -> true
  | TBase b1, TBase b2 -> b1 = b2
  | TFuns([], typ1), typ2 -> can_unify typ1 typ2
  | typ1, TFuns([], typ2) -> can_unify typ1 typ2
  | TFuns(x::xs, typ1), typ2 -> can_unify (TFun(x, TFuns(xs, typ1))) typ2
  | typ1, TFuns(x::xs, typ2) -> can_unify typ1 (TFun(x, TFuns(xs, typ2)))
  | TFun(x1,typ1),TFun(x2,typ2) -> can_unify (Id.typ x1) (Id.typ x2) && can_unify typ1 typ2
  | TApp(c1, typs1), TApp(c2, typs2) -> c1 = c2 && List.for_all2 can_unify typs1 typs2
  | TTuple xs1, TTuple xs2 ->
      List.length xs1 = List.length xs2 &&
      List.for_all2 (fun x1 x2 -> can_unify (Id.typ x1) (Id.typ x2)) xs1 xs2
  | TData "event", TFun _ -> true
  | TFun _, TData "event" -> true
  | TVar({contents=None},_), _ -> true
  | _, TVar({contents=None},_) -> true
  | TVariant(poly1,labels1), TVariant(poly2,labels2) ->
      poly1 = poly2 &&
      (poly1 || List.for_all2 (fun (s1,typs1) (s2,typs2) -> s1 = s2 && List.for_all2 can_unify typs1 typs2) labels1 labels2)
  | TRecord fields1, TRecord fields2 ->
      List.for_all2 (fun (s1,(f1,typ1')) (s2,(f2,typ2')) -> s1 = s2 && f1 = f2 && can_unify typ1' typ2') fields1 fields2
  | TModule _, TModule _ -> true (* TODO *)
  | TModule _, TData _ -> true (* TODO *)
  | TData _, _ -> true (* TODO *)
  | _, TData _ -> true (* TODO *)
  | _ -> false


let rec flatten typ =
  match typ with
      TVar({contents = Some typ'},_) -> flatten typ'
    | _ -> typ

(* just for "unify"? *)
let rec occurs r typ =
  match flatten typ with
  | TBase _ -> false
  | TVar({contents=None} as r',_) -> r == r'
  | TVar({contents=Some typ},_) -> assert false
  | TFun(x,typ) -> occurs r (Id.typ x) || occurs r typ
  | TApp(_, typs) -> List.exists (occurs r) typs
  | TTuple xs -> List.exists (occurs r -| Id.typ) xs
  | TData _ -> false
  | TAttr(_,typ) -> occurs r typ
  | TFuns _ -> unsupported ""
  | TVariant(_,labels) -> List.exists (snd |- List.exists @@ occurs r) labels
  | TRecord fields -> List.exists (snd |- snd |- occurs r) fields
  | TModule sgn -> List.exists (snd |- occurs r) sgn

let rec data_occurs s typ =
  match flatten typ with
  | TBase _ -> false
  | TVar(r,_) -> Option.exists (data_occurs s) !r
  | TFun(x,typ) -> data_occurs s (Id.typ x) || data_occurs s typ
  | TApp(_, typs) -> List.exists (data_occurs s) typs
  | TTuple xs -> List.exists (data_occurs s -| Id.typ) xs
  | TData s' -> s = s'
  | TAttr(_,typ) -> data_occurs s typ
  | TFuns _ -> unsupported ""
  | TVariant(_,labels) -> List.exists (snd |- List.exists @@ data_occurs s) labels
  | TRecord fields -> List.exists (snd |- snd |- data_occurs s) fields
  | TModule sgn -> List.exists (snd |- data_occurs s) sgn


let rec unify typ1 typ2 =
  match flatten @@ elim_tattr typ1, flatten @@ elim_tattr typ2 with
  | TBase b1, TBase b2 -> if b1 <> b2 then raise CannotUnify
  | TFun(x1, typ1), TFun(x2, typ2) ->
      unify (Id.typ x1) (Id.typ x2);
      unify typ1 typ2
  | TApp(_,typs1), TApp(_,typs2) -> List.iter2 unify typs1 typs2
  | TTuple xs1, TTuple xs2 ->
      List.iter2 (fun x1 x2 -> unify (Id.typ x1) (Id.typ x2)) xs1 xs2
  | TVar(r1,_), TVar(r2,_) when r1 == r2 -> ()
  | TVar({contents = None} as r, n), typ
  | typ, TVar({contents = None} as r, n) ->
      if occurs r typ then
        (Format.printf "occurs check failure: %a, %a@." print_init (flatten typ1) print_init (flatten typ2);
         raise CannotUnify);
      Option.iter (fun n -> Debug.printf "%a := %a@." print_tvar n print_init typ) n;
      r := Some typ
  | TData _, TData _ -> ()
  | TVariant(true,labels1), TVariant(true,labels2) -> ()
  | TVariant(false,labels1), TVariant(false,labels2) ->
      List.iter2 (fun (s1,typs1) (s2,typs2) -> assert (s1 = s2); List.iter2 unify typs1 typs2) labels1 labels2
  | TRecord fields1, TRecord fields2 -> List.iter2 (fun (s1,(f1,typ1)) (s2,(f2,typ2)) -> assert (s1 = s2 && f1 = f2); unify typ1 typ2) fields1 fields2
  | _, TData _ -> ()
  | TData _, _ -> ()
  | _ ->
      Format.printf "unification error: %a, %a@." print_init (flatten typ1) print_init (flatten typ2);
      raise CannotUnify


let rec same_shape typ1 typ2 =
  match elim_tattr typ1, elim_tattr typ2 with
  | TBase b1, TBase b2 -> b1 = b2
  | TVar({contents=None},_), TVar({contents=None},_) -> true
  | TVar({contents=Some typ1},_),TVar({contents=Some typ2},_) -> same_shape typ1 typ2
  | TFun(x1,typ1),TFun(x2,typ2) -> same_shape (Id.typ x1) (Id.typ x2) && same_shape typ1 typ2
  | TApp(c1,typs1), TApp(c2,typs2) -> c1=c2 && List.for_all2 same_shape typs1 typs2
  | TTuple xs1, TTuple xs2 ->
      List.length xs1 = List.length xs2 &&
      List.for_all2 (fun x1 x2 -> same_shape (Id.typ x1) (Id.typ x2)) xs1 xs2
  | TData s1, TData s2 -> s1 = s2
  | TVariant(poly1,labels1), TVariant(poly2,labels2) ->
      poly1 = poly2 &&
      List.eq ~eq:(Compare.eq_on snd ~eq:(List.eq ~eq:same_shape)) labels1 labels2
  | TRecord fields1, TRecord fields2 -> unsupported "same_shape"
  | _ -> false





let rec copy = function
  | TVar({contents = Some typ},_) -> copy typ
  | TVar({contents = None},_) -> TVar(ref None,None)
  | typ -> typ



let rec app_typ typ typs =
  match typ,typs with
    | TFun(_,typ2), _::typs' -> app_typ typ2 typs'
    | _, [] -> typ
    | _ -> assert false

let tuple_num typ =
  match elim_tattr typ with
  | TTuple xs -> Some (List.length xs)
  | _ -> None

let proj_typ i typ =
  match elim_tattr typ with
  | TTuple xs -> Id.typ @@ List.nth xs i
  | typ when typ = typ_unknown -> typ_unknown
  | typ' -> invalid_arg @@ Format.asprintf "proj_typ %d (%a)" i print_init typ'

let fst_typ typ = proj_typ 0 typ
let snd_typ typ = proj_typ 1 typ

let ref_typ typ =
  match elim_tattr typ with
  | TApp(TRef, [typ]) -> typ
  | typ when typ = typ_unknown -> typ_unknown
  | _ -> invalid_arg "ref_typ"

let list_typ typ =
  match elim_tattr typ with
  | TApp(TList, [typ]) -> typ
  | typ when typ = typ_unknown -> typ_unknown
  | _ -> invalid_arg "list_typ"

let option_typ typ =
  match elim_tattr typ with
  | TApp(TOption, [typ]) -> typ
  | typ when typ = typ_unknown -> typ_unknown
  | _ -> invalid_arg "option_typ"

let array_typ typ =
  match elim_tattr typ with
  | TApp(TArray, [typ]) -> typ
  | typ when typ = typ_unknown -> typ_unknown
  | _ -> invalid_arg "array_typ"

let rec has_pred = function
  | TBase _ -> false
  | TVar({contents=None},_) -> false
  | TVar({contents=Some typ},_) -> has_pred typ
  | TFun(x,typ) -> has_pred (Id.typ x) || has_pred typ
  | TApp(_,typs) -> List.exists has_pred typs
  | TTuple xs -> List.exists (has_pred -| Id.typ) xs
  | TData _ -> false
  | TAttr(attr,typ) ->
      has_pred typ || List.exists (function TAPred(_, _::_) -> true | _ -> false) attr
  | TFuns _ -> unsupported ""
  | TVariant(_,labels) -> List.exists (snd |- List.exists has_pred) labels
  | TRecord fields -> List.exists (snd |- snd |- has_pred) fields
  | TModule sgn -> List.exists (snd |- has_pred) sgn

let rec to_id_string = function
  | TBase TUnit -> "unit"
  | TBase TBool -> "bool"
  | TBase TInt -> "int"
  | TBase (TPrim s) -> s
  | TVar({contents=None},_) -> "abst"
  | TVar({contents=Some typ},_) -> to_id_string typ
  | TFun(x,typ) -> to_id_string (Id.typ x) ^ "__" ^ to_id_string typ
  | TApp(TList, [typ]) -> to_id_string typ ^ "_list"
  | TTuple xs ->
      let xs',x = List.decomp_snoc xs in
      let aux x s = to_id_string (Id.typ x) ^ "_x_" ^ s in
      List.fold_right aux xs' @@ to_id_string @@ Id.typ x
  | TData s -> s
  | TAttr(_,typ) -> to_id_string typ
  | TApp(TRef, [typ]) -> to_id_string typ ^ "_ref"
  | TApp(TOption, [typ]) -> to_id_string typ ^ "_option"
  | TApp(TArray, [typ]) -> to_id_string typ ^ "_array"
  | TApp _ -> assert false
  | TFuns _ -> unsupported ""
  | TVariant(_,labels) -> String.join "_" @@ List.map fst labels
  | TRecord fields -> String.join "_" @@ List.map fst fields
  | TModule _ -> "module"


(* order of simpl types *)
let rec order typ =
  match typ with
  | TBase _ -> 0
  | TData _ -> 0
  | TVar({contents=None},_) -> assert false
  | TVar({contents=Some typ},_) -> order typ
  | TFun(x,typ) -> max (order (Id.typ x) + 1) (order typ)
  | TTuple xs -> List.fold_left (fun m x -> max m (order @@ Id.typ x)) 0 xs
  | TAttr(_,typ) -> order typ
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

let decomp_trecord typ =
  match typ with
  | TRecord fields -> fields
  | _ ->
      Format.printf "%a@." print_init typ;
      invalid_arg "decomp_trecord"

let decomp_tvariant ty =
  match ty with
  | TVariant(poly,labels) ->
      poly, labels
  | _ ->
      Format.printf "%a@." print_init ty;
      invalid_arg "decomp_tvariant"



let rec is_mutable_record typ =
  match typ with
  | TRecord fields ->
      List.exists (fun (_,(f,_)) -> f = Mutable) fields
  | _ -> invalid_arg "is_mutable_record"


let prim_base_types =
  ["char";
   "string";
   "float";
   "int32";
   "int64";
   "nativeint";
   "format4";
   "format6";
   "Format.format";
   "Format.formatter"]


let rec remove_arg_at i typ =
  match typ with
  | TFun(x, typ) when i = 0 -> typ
  | TFun(x, typ) -> TFun(x, remove_arg_at (i-1) typ)
  | _ -> assert false


let decomp_tattr ty =
  match ty with
  | TAttr(attrs, ty') -> attrs, ty'
  | _ -> [], ty

let is_tvar ty =
  match ty with
  | TVar({contents=None},_) -> true
  | _ -> false

module Ty = struct
  let unit = TBase TUnit
  let bool = TBase TBool
  let int = TBase TInt
  let fun_ = make_tfun
  let funs tys ty = List.fold_right make_tfun tys ty
  let tuple = make_ttuple
  let tuple' = make_ttuple'
  let pair = make_tpair
  let list = make_tlist
  let ref = make_tref
  let option = make_toption
  let array = make_tarray
end
