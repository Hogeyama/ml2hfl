open Syntax
open Type
open Util

type kind =
  | TKVariant of (string * typ list) list
  | TKRecord of (string * (mutable_flag * typ)) list
  | Primitive
  | Abstract

let debug () = List.mem "Type_decl" !Flag.debug_module

let primitives = ["char";"string";"float";"int32";"int64";"nativeint";"format4";"format6"]

let typ_decls = ref (("ABST", Abstract) :: ("exn",TKVariant["Assert_failure",[]]) :: List.map (Pair.pair -$- Primitive) primitives)

let print_kind fm = function
  | Primitive -> Format.fprintf fm "(prim)"
  | Abstract -> Format.fprintf fm "(abst)"
  | TKVariant stypss ->
      let aux fm (s,typs) =
        if typs = []
        then Format.fprintf fm "%s" s
        else Format.fprintf fm "%s of %a" s (print_list Print.typ " * ") typs
      in
      print_list aux "@ | " fm stypss
  | TKRecord sftyps ->
      let aux fm (s,(f,typ)) =
        Format.fprintf fm "%s%s:%a" (if f = Mutable then "mutable " else "") s Print.typ typ
      in
      Format.fprintf fm "@[{%a}@]" (print_list aux "; ") sftyps

let from_type_kind kind =
  match kind with
  | KAbstract -> invalid_argument "Type_decl.from_type_kind"
  | KVariant decls -> TKVariant decls
  | KRecord decls -> TKRecord decls
  | KOpen -> Abstract

let in_typ_decls s = List.mem_assoc s !typ_decls

let assoc s =
  try
    List.assoc s !typ_decls
  with Not_found -> Format.printf "%s@." s; assert false

let is_primitive s = List.mem s primitives

let is_variant s =
  match assoc s with
  | TKVariant _ -> true
  | _ -> false

let is_record s =
  match assoc s with
  | TKRecord _ -> true
  | _ -> false

let need_add s =
  not (in_typ_decls s) && s <> "unit" && s <> "bool" && s <> "list"

let add_typ_decl s k =
  if need_add s
  then
    begin
      if !!debug then Format.printf "ADD %s = %a@." s print_kind k;
      typ_decls := (s,k)::!typ_decls
    end

let add_exc_decl s typs =
  let exc_decls,remains = List.partition (fun (s,_) -> s = "exn") !typ_decls in
  let exc_decl =
    match exc_decls with
    | [_, TKVariant stypss] ->
        let stypss' =
          if List.mem_assoc s stypss
          then stypss
          else (s,typs)::stypss
        in
        if !!debug then Format.printf "add_exc(%d): %a@." (List.length typs) print_kind @@ TKVariant stypss';
        "exn", TKVariant stypss'
    | _ -> assert false
  in
  typ_decls := exc_decl :: remains

let map_kind f k =
  match k with
  | Primitive -> Primitive
  | Abstract -> Abstract
  | TKVariant stypss -> TKVariant (List.map (Pair.map_snd @@ List.map f) stypss)
  | TKRecord sftyps -> TKRecord (List.map (Pair.map_snd @@ Pair.map_snd f) sftyps)

let map f = Ref.map (List.map (Pair.map Fun.id @@ map_kind f)) typ_decls


let constr_typ s =
  let rec search = function
    | [] -> Format.printf "Not found: constructor %s@." s; assert false
    | (_, Primitive)::kinds -> search kinds
    | (_, Abstract)::kinds -> search kinds
    | (_, TKRecord _)::kinds -> search kinds
    | (c, TKVariant stypss)::kinds ->
        if List.mem_assoc s stypss
        then TData(c, true)
        else search kinds
  in
  search !typ_decls

let constr_arg_typs s =
  let rec search = function
    | [] -> Format.printf "Not found: constructor %s@." s; assert false
    | (_, Primitive)::kinds -> search kinds
    | (_, Abstract)::kinds -> search kinds
    | (_, TKRecord _)::kinds -> search kinds
    | (_, TKVariant stypss)::kinds ->
        try
          List.assoc s stypss
        with Not_found -> search kinds
  in
  search !typ_decls

let kind_of_field s =
  let rec search = function
    | [] -> Format.printf "Not found: field %s@." s; assert false
    | (_, Primitive)::kinds -> search kinds
    | (_, Abstract)::kinds -> search kinds
    | (_, TKVariant _)::kinds -> search kinds
    | (c, (TKRecord sftyps as kind))::kinds ->
        if List.mem_assoc s sftyps
        then c, kind
        else search kinds
  in
  search !typ_decls

let field_typ s =
  let c,_ = kind_of_field s in
  TData(c, true)

let field_arg_typ s =
  let sftyps =
    match kind_of_field s with
    | _, TKRecord sftyps -> sftyps
    | _ -> assert false
  in
  snd @@ List.assoc s sftyps

let constr_pos s =
  let aux = function
    | _, Abstract -> []
    | _, Primitive -> []
    | _, TKVariant stypss -> List.map fst stypss
    | _, TKRecord _ -> []
  in
  let constrs = List.flatten_map aux !typ_decls in
  let rec search i = function
    | [] -> Format.printf "Not found: constructor %s@." s; assert false
    | c::_ when c = s -> i
    | _::cs -> search (i+1) cs
  in
  search 0 constrs


let is_mutable c =
  let sftyps =
    match List.assoc c !typ_decls with
    | TKRecord sftyps -> sftyps
    | _ -> assert false
  in
  List.exists (fun (_,(f,_)) -> f = Mutable) sftyps

let get_mutable_flag s =
  match kind_of_field s with
  | _, TKRecord sftyps -> fst @@ List.assoc s sftyps
  | _ -> invalid_argument "get_mutable_flag"

let get_pos s =
  match kind_of_field s with
  | _, TKRecord sftyps ->
      List.assoc s @@ List.mapi (fun i (s,(f,typ)) -> s, i) sftyps
  | _ -> invalid_argument "get_mutable_flag"

let rec can_reach acc s1 s2 =
  let aux (b,acc) typ =
    if b
    then true, acc
    else
      match typ with
      | TData(s,_) when not @@ List.mem s acc ->
          can_reach (s::acc) s s2
      | _ -> false, acc
  in
  match assoc s1 with
  | Primitive -> false, acc
  | Abstract -> false, acc
  | TKRecord sftyps when List.exists (function (_,(_,TData(s,_))) -> s = s2 | _ -> false) sftyps -> true, acc
  | TKRecord sftyps ->
      List.fold_left (fun bacc (_,(_,typ)) -> aux bacc typ) (false,[]) sftyps
  | TKVariant stypss ->
      List.fold_left (fun bacc (s,typs) -> List.fold_left aux bacc typs) (false,[]) stypss

let can_reach s1 s2 = fst @@ can_reach [] s1 s2

(* Not implemented *)
let get_mutual_decls s =
  !typ_decls

let get_ground_types s =
  let decls = get_mutual_decls s in
  let names = List.map fst decls in
  let add typ typs = if List.exists (same_shape typ) typs then typs else typ::typs in
  let rec elim_and_decomp acc = function
    | [] -> acc
    | typ::typs ->
        match typ with
        | TUnit -> elim_and_decomp acc typs
        | TBool -> elim_and_decomp (add TBool acc) typs
        | TAbsBool -> assert false
        | TInt -> elim_and_decomp (add TInt acc) typs
        | TRInt _ -> assert false
        | TVar({contents=None}) -> unsupported "(type 'a t = ...)"
        | TVar _ -> unsupported "(type 'a t = ...)"
        | TFun _ -> elim_and_decomp (add TInt acc) typs
        | TList (TData(s',_) as typ') when can_reach s' s -> elim_and_decomp acc (typ'::typs)
        | TList _ -> elim_and_decomp (add typ acc) typs
        | TTuple xs -> elim_and_decomp acc (List.map Id.typ xs @ typs)
        | TData(s,true) ->
            if List.mem s names then
              elim_and_decomp acc typs
            else
              fatal "Not implemented (Type_decl.get_base_types)"
        | TData(s,false) -> elim_and_decomp (add typ acc) typs
        | _ -> assert false
  in
  let aux = function
    | Abstract -> []
    | Primitive -> []
    | TKVariant styps -> List.rev_map_flatten snd styps
    | TKRecord sftyps -> List.map (fun (_,(_,typ)) -> typ) sftyps
  in
  elim_and_decomp [] @@ List.rev_map_flatten aux @@ List.map snd decls
