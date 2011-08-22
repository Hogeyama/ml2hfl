
open Utilities

type 'a t =
    TUnit
  | TBool
  | TAbsBool
  | TInt of 'a list
  | TRInt of 'a
  | TVar of 'a t option ref
  | TFun of ('a t Id.t) * 'a t
  | TList of 'a t
  | TPair of 'a t * 'a t
  | TConstr of string * bool
  | TUnknown
  | TAbs of ('a t -> 'a t)
  | TVariant of 'a t
  | TBottom

let typ_event = TConstr("event", true)
let typ_excep = TConstr("exn", true)


let rec decomp_tfun = function
    TFun(x,typ) ->
      let xs,typ = decomp_tfun typ in
        x :: xs, typ
  | typ -> [], typ

let rec can_unify typ1 typ2 =
  match typ1,typ2 with
      TBottom, _ -> true
    | _, TBottom -> true
    | TVar{contents=Some typ1},typ2
    | typ1,TVar{contents=Some typ2} -> can_unify typ1 typ2
    | TUnit,TUnit -> true
    | (TBool|TAbsBool),(TBool|TAbsBool) -> true
    | TInt _,TInt _ -> true
    | TRInt _,TRInt _ -> true
    | TFun(x1,typ1),TFun(x2,typ2) -> can_unify (Id.typ x1) (Id.typ x2) && can_unify typ1 typ2
    | TList typ1, TList typ2 -> can_unify typ1 typ2
    | TPair(typ11,typ12), TPair(typ21,typ22) -> can_unify typ11 typ21 && can_unify typ12 typ22
    | TConstr(s1,_),TConstr(s2,_) -> s1 = s2
(*
    | TVariant stypss1,TVariant stypss2 ->
        let aux (s1,typs1) (s2,typs2) = s1=s2 && List.for_all2 can_unify typs1 typs2 in
          List.length stypss1 = List.length stypss2 && List.for_all2 aux stypss1 stypss2
    | TRecord(_,fields1),TRecord(_,fields2) -> List.for_all2 (fun (s1,(_,typ1)) (s2,(_,typ2)) -> s1=s2 && can_unify typ1 typ2) fields1 fields2
*)
    | TUnknown, TUnknown -> true
    | TVar{contents=None}, _ -> true
    | _, TVar{contents=None} -> true
    | _ -> false

(*
let rec occurs x = function
      TUnit
    | TBool
    | TAbsBool
    | TUnknown
    | TVar _ -> false
    | TInt ts -> List.exists (Id.same x) (List.concat (List.map get_fv ts))
    | TRInt t -> List.exists (Id.same x) (get_fv t)
    | TFun(y,typ) -> occurs x (Id.typ y) || occurs x typ
*)

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
          assert (not Flag.check_fun_arg_typ || can_unify (Id.typ x) typ);
          (*      if (match typ1 with TInt _ -> true | TRInt _ -> true | _ -> false) (*&& occurs x typ2*)
                  then fprintf fm "(%a -> %a)" print_id_typ x print typ2
                  else*) Format.fprintf fm "(%a -> %a)" print (Id.typ x) print typ
      | TUnknown -> Format.fprintf fm "???"
      | TList typ -> Format.fprintf fm "%a list" print typ
      | TPair(typ1,typ2) -> Format.fprintf fm "(%a * %a)" print typ1 print typ2
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
      | TBottom -> Format.fprintf fm "_|_"

and print_preds print_pred fm = function
    [] -> ()
  | [x] -> print_pred fm x
  | x1::x2::xs -> Format.fprintf fm "%a;%a" print_pred x1 (print_preds print_pred) (x2::xs)



