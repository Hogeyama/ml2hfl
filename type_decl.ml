
open Syntax
open Type
open Util

type kind =
  | TKVariant of (string * typ list) list
  | TKRecord of (string * (mutable_flag * typ)) list

let debug () = List.mem "Type_decl" !Flag.debug_module

let typ_decls : (string * kind) list ref = ref ["ABST", TKVariant ["Abst", []]]


let print_kind fm = function
  | TKVariant stypss ->
      let aux fm (s,typs) =
        if typs = []
        then Format.fprintf fm "%s" s
        else Format.fprintf fm "%s of %a" s (print_list print_typ " * ") typs
      in
      print_list aux " | " fm stypss
  | TKRecord sftyps ->
      let aux fm (s,(f,typ)) =
        Format.fprintf fm "%s%s:%a" (if f = Mutable then "mutable " else "") s print_typ typ
      in
      Format.fprintf fm "@[{%a}@]" (print_list aux "; ") sftyps

let in_typ_decls s = List.mem_assoc s !typ_decls


let add_typ_decl s k =
  if not (in_typ_decls s) && s <> "unit" && s <> "bool" && s <> "list"
  then
    begin
      if debug() then Format.printf "ADD %s = %a@." s print_kind k;
      typ_decls := (s,k)::!typ_decls
    end

let add_exc_decl s typs =
  if in_typ_decls "exn"
  then
    let exc_decls,remains = List.partition (fun (s,_) -> s = "exn") !typ_decls in
    let exc_decl =
      match exc_decls with
      | [_, TKVariant stypss] ->
          let stypss' =
            if List.exists (fun (s',_) -> s = s') stypss
            then stypss
            else (s,typs)::stypss
          in
          if debug() then Format.printf "add_exc(%d): %a@." (List.length typs) print_kind @@ TKVariant stypss';
          "exn", TKVariant stypss'
      | _ -> assert false
    in
    typ_decls := exc_decl :: remains
  else typ_decls := ("exn",TKVariant[s,typs])::!typ_decls


let assoc_typ s = List.assoc s !typ_decls


let constr_arg_typs s =
  let rec search = function
      [] -> Format.printf "Not found: constructor %s@." s; assert false
    | (_, TKRecord _)::kinds -> search kinds
    | (_, TKVariant stypss)::kinds ->
        try
          List.assoc s stypss
        with Not_found -> search kinds
  in
    search !typ_decls


let constr_pos s =
  let aux = function
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
        | TVar({contents=None}) -> fatal "Unsupported (type 'a t = ..."
        | TVar _ -> fatal "Unsupported (type 'a t = ..."
        | TFun _ -> elim_and_decomp (add TInt acc) typs
        | TList _ -> fatal "Unsupported (type t = ... t list ...)"
        | TTuple xs -> elim_and_decomp acc (List.map Id.typ xs @ typs)
        | TConstr(s,true) ->
            if List.mem s names then
              elim_and_decomp acc typs
            else
              fatal "Not implemented (Type_decl.get_base_types)"
        | TConstr(s,false) -> elim_and_decomp (add typ acc) typs
        | _ -> assert false
  in
  let aux = function
    | TKVariant styps -> List.rev_map_flatten snd styps
    | TKRecord sftyps -> List.map (fun (_,(_,typ)) -> typ) sftyps
  in
  elim_and_decomp [] @@ List.rev_map_flatten aux @@ List.map snd decls
