
open Syntax
open Type
open Utilities

type kind =
    TKVariant of (string * typ list) list
  | TKRecord of (string * (mutable_flag * typ)) list

let typ_decls : (string * kind) list ref = ref []
let exc_decls : (string * typ list) list ref = ref []


let print_kind fm = function
    TKVariant stypss ->
      let aux fm (s,typs) =
        if typs = []
        then Format.fprintf fm "%s" s
        else Format.fprintf fm "%s of %a" s (print_list print_typ " * " false) typs
      in
        print_list aux " | " false fm stypss
  | TKRecord sftyps ->
      let aux fm (s,(f,typ)) =
        Format.fprintf fm "%s%s:%a" (if f = Mutable then "mutable " else "") s print_typ typ
      in
        Format.fprintf fm "@[{%a}@]" (print_list aux "; " false) sftyps

let in_typ_decls s = List.mem_assoc s !typ_decls
let in_exc_decls s = List.mem_assoc s !exc_decls


let add_type_decl s k =
  if not (in_typ_decls s) && s <> "unit" && s <> "bool" && s <> "list"
  then
    begin
      if true then Format.printf "ADD %s = %a@." s print_kind k;
      typ_decls := (s,k)::!typ_decls
    end

let add_exc_decl s typs =
  if not (in_exc_decls s)
  then exc_decls := (s,typs)::!exc_decls


let get_constr_typ s =
  if in_exc_decls s
  then !typ_excep
  else assert false




let assoc_typ s = List.assoc s !typ_decls
let assoc_exc s = List.assoc s !exc_decls



let constr_pos s =
  let aux = function
      _, TKVariant stypss -> List.map fst stypss
    | _, TKRecord _ -> []
  in
  let constrs = flatten_map aux !typ_decls in
  let rec search i = function
      [] -> Format.printf "Not found: constructor %s@." s; assert false
    | c::_ when c = s -> i
    | _::cs -> search (i+1) cs
  in
    search 0 constrs


let rec get_exc_typs = uniq' compare (rev_flatten_map snd !exc_decls)

(* Not implemented *)
let get_mutual_decls s =
  !typ_decls

let get_ground_types s =
  let decls = get_mutual_decls s in
  let names = List.map fst decls in
  let add typ typs = if List.exists (same_shape typ) typs then typs else typ::typs in
  let rec elim_and_decomp acc = function
      [] -> acc
    | typ::typs ->
        match typ with
            TUnit -> elim_and_decomp acc typs
          | TBool -> elim_and_decomp (add TBool acc) typs
          | TAbsBool -> assert false
          | TInt -> elim_and_decomp (add TInt acc) typs
          | TRInt _ -> assert false
          | TVar _ -> raise (Fatal "Unsupported (type 'a t = ...")
          | TFun _ -> elim_and_decomp (add TInt acc) typs
          | TList _ -> raise (Fatal "Unsupported (type t = ... t list ...)")
          | TPair(typ1,typ2) -> elim_and_decomp acc (typ1::typ2::typs)
          | TConstr(s,b) ->
              if List.mem s names
              then elim_and_decomp acc typs
              else raise (Fatal "Not implemented (Type_decl.get_base_types)")
          | TPred _ -> assert false
  in
  let aux = function
      TKVariant styps -> rev_map_flatten snd styps
    | TKRecord sftyps -> List.map (fun (_,(_,typ)) -> typ) sftyps
  in
    elim_and_decomp [] (rev_map_flatten aux (List.map snd decls))
