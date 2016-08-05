open Util

module S = Syntax
module U = Term_util
module T = Type

let debug () = List.mem "Ref_type" !Flag.debug_module

type base =
  | Unit
  | Bool
  | Int
  | Abst of string

type t =
  | Base of base * S.id * S.typed_term
  | Fun of S.id * t * t
  | Tuple of (S.id * t) list
  | Inter of S.typ * t list
  | Union of S.typ * t list
  | ExtArg of S.id * t * t
  | List of S.id * S.typed_term * S.id * S.typed_term * t
  | Exn of t * t

let typ_result = Base(Abst "X", U.dummy_var, U.true_term)

let _Inter styp typs = Inter(styp, typs)
let _Union styp typs = Union(styp, typs)
let _ExtArg x typ1 typ2 = ExtArg(x, typ1, typ2)

let top styp = Inter(styp, [])
let bottom styp = Union(styp, [])

let decomp_base typ =
  match typ with
  | Base(base,x,t) -> Some (base, x, t)
  | _ -> None
let decomp_fun typ =
  match typ with
  | Fun(x,typ1,typ2) -> Some (x, typ1, typ2)
  | _ -> None
let decomp_list typ =
  match typ with
  | List(x,p_len,y,p_i,typ2) -> Some (x, p_len, y, p_i, typ2)
  | _ -> None
let rec decomp_inter typ =
  match typ with
  | Inter(_, typs) -> List.flatten_map decomp_inter typs
  | _ -> [typ]
let rec decomp_union typ =
  match typ with
  | Inter(_, typs) -> List.flatten_map decomp_union typs
  | _ -> [typ]

let is_base = Option.is_some -| decomp_base
let is_fun = Option.is_some -| decomp_fun
let is_list = Option.is_some -| decomp_list
let is_bottom typ =
  match typ with
  | Union(_, []) -> true
  | _ -> false
let is_top typ =
  match typ with
  | Inter(_, []) -> true
  | _ -> false
let rec is_bottom' typ =
  match typ with
  | Union(_, []) -> true
  | Base(_, _, {S.desc=S.Const S.False}) -> true
  | Fun(_, typ1, typ2) -> is_top' typ1 && is_bottom' typ2
  | _ -> false
and is_top' typ =
  match typ with
  | Inter(_, []) -> true
  | Base(_, _, {S.desc=S.Const S.True}) -> true
  | Fun(_, typ1, typ2) -> is_bottom' typ1 && is_top' typ2
  | _ -> false

let print_base fm = function
  | Unit -> Format.pp_print_string fm "unit"
  | Bool -> Format.pp_print_string fm "bool"
  | Int -> Format.pp_print_string fm "int"
  | Abst s -> Format.pp_print_string fm s

let rec occur x = function
  | Base(_,_,p) -> List.exists (Id.same x) @@ U.get_fv p
  | Fun(_,typ1,typ2) -> occur x typ1 || occur x typ2
  | Tuple xtyps -> List.exists (occur x -| snd) xtyps
  | Inter(_, typs)
  | Union(_, typs) -> List.exists (occur x) typs
  | ExtArg(_,typ1,typ2) -> occur x typ1 || occur x typ2
  | List(_,p_len,_,p_i,typ) ->
      let aux =  Id.mem x -| U.get_fv in
      aux p_len || aux p_i || occur x typ

let rec decomp_fun_full typ =
  match typ with
  | Fun(x,typ1,typ2) ->
      Pair.map_fst (List.cons (x,typ1)) @@ decomp_fun_full typ2
  | _ -> [], typ

let rec print fm = function
  | Base(base,x,p) when p = U.true_term ->
      Format.fprintf fm "%a" print_base base
  | Base(Bool,x,p) when U.make_var x = p ->
      Format.fprintf fm "{true}"
  | Base(Bool,x,p) when U.make_not (U.make_var x) = p ->
      Format.fprintf fm "{false}"
  | Base(Int,x,{S.desc=S.BinOp(S.Eq, {S.desc=S.Var y}, {S.desc=S.Const(S.Int n)})})
  | Base(Int,x,{S.desc=S.BinOp(S.Eq, {S.desc=S.Const (S.Int n)}, {S.desc=S.Var y})}) when x = y ->
      Format.fprintf fm "{%d}" n
  | Base(base,x,p) when p.S.desc = S.Const S.False ->
      Format.fprintf fm "_|_"
  | Base(base,x,p) ->
      Format.fprintf fm "{%a:%a | %a}" Id.print x print_base base Print.term p
  | Fun _ as typ ->
      let rec aux fm (xtyps, typ) =
        match xtyps with
        | [] -> print fm typ
        | (x,typ1)::xtyps' ->
            if List.exists (snd |- occur x) xtyps
            then Format.fprintf fm "@[<hov 2>%a:%a ->@ %a@]" Id.print x print typ1 aux (xtyps', typ)
            else Format.fprintf fm "@[<hov 2>%a ->@ %a@]" print typ1 aux (xtyps', typ)
      in
      Format.fprintf fm "(%a)" aux @@ decomp_fun_full typ
  | Fun(x, typ1, Exn(typ2, typ3)) ->
      let arg =
        if occur x typ2 || occur x typ3 then
          Format.asprintf "%a:" Id.print x
        else
          ""
      in
      Format.fprintf fm "(@[<hov 2>%s%a ->^{%a}@ %a@])" arg print typ1 print typ3 print typ2
  | Tuple xtyps ->
      let n = List.length xtyps in
      let pr fm (i,(x,typ)) =
        if i < n-1 && occur x @@ Tuple xtyps
        then Format.fprintf fm "%a:" Id.print x;
        print fm typ
      in
      Format.fprintf fm "(@[%a@])" (print_list pr " *@ ") @@ List.mapi Pair.pair xtyps
  | Inter(styp, []) when !!debug -> Format.fprintf fm "Top(%a)" Print.typ styp
  | Inter(_, []) -> Format.fprintf fm "Top"
  | Inter(_, [typ]) -> print fm typ
  | Inter(_, typs) -> Format.fprintf fm "(@[%a@])" (print_list print " /\\@ ") typs
  | Union(styp, []) when !!debug -> Format.fprintf fm "Bottom(%a)" Print.typ styp
  | Union(_, []) -> Format.fprintf fm "Bottom"
  | Union(_, [typ]) -> print fm typ
  | Union(_, typs) -> Format.fprintf fm "(@[%a@])" (print_list print " \\/@ ") typs
  | ExtArg(x,typ1,typ2) ->
      Format.fprintf fm "(@[%a where %a:%a@])" print typ2 Id.print x print typ1
  | List(x,p_len,y,p_i,typ2) ->
      Format.fprintf fm "(@[";
      if p_i = U.true_term then
        if occur y typ2
        then Format.fprintf fm "[%a]%a" Id.print y print typ2
        else Format.fprintf fm "%a" print typ2
      else
        Format.fprintf fm "[%a: %a]%a" Id.print y Print.term p_i print typ2;
      if p_len <> U.true_term then
        Format.fprintf fm "|%a: %a|" Id.print x Print.term p_len
      else
        if List.exists (Id.same x) (U.get_fv p_i) || occur x typ2
        then Format.fprintf fm "|%a|" Id.print x;
      Format.fprintf fm " list@])"
  | Exn(typ1, typ2) ->
      Format.fprintf fm "(@[<hov 2>%a@ |^{%a}@])" print typ1 print typ2

let rec decomp_funs n typ =
  match typ with
  | Fun _ when n <= 0 ->
      [], [], typ
  | Fun(x,typ1,typ2) ->
      let exts,typs,typ' = decomp_funs (n-1) typ2 in
      exts, (x,typ1)::typs, typ'
  | ExtArg(x,typ1,typ2) ->
      let exts,typs,typ' = decomp_funs n typ2 in
      (x,typ1)::exts, typs, typ'
  | _ when n = 0 -> [], [], typ
  | _ ->
      if !!debug then Format.printf "%a@." print typ;
      assert false


let rec arg_num = function
  | Base _ -> 0
  | Tuple _ -> 0
  | Inter(typ, []) -> List.length @@ fst @@ T.decomp_tfun typ
  | Inter(_, typ::_) -> arg_num typ
  | Union(typ, []) -> List.length @@ fst @@ T.decomp_tfun typ
  | Union(_, typ::_) -> arg_num typ
  | Fun(_,_,typ2) -> 1 + arg_num typ2
  | ExtArg(_,_,typ2) -> arg_num typ2
  | List _ -> 0

let rec map_pred f typ =
  match typ with
  | Base(base,y,p) -> Base(base, y, f p)
  | Fun(y,typ1,typ2) -> Fun(y, map_pred f typ1, map_pred f typ2)
  | Tuple xtyps -> Tuple (List.map (Pair.map_snd @@ map_pred f) xtyps)
  | Inter(typ, typs) -> Inter(typ, List.map (map_pred f) typs)
  | Union(typ, typs) -> Union(typ, List.map (map_pred f) typs)
  | ExtArg(y,typ1,typ2) -> ExtArg(y, map_pred f typ1, map_pred f typ2)
  | List(y,p_len,z,p_i,typ) -> List(y, f p_len, z, f p_i, map_pred f typ)
  | Exn(typ1, typ2) -> Exn(map_pred f typ1, map_pred f typ2)

let subst x t typ = map_pred (U.subst x t) typ
let subst_var x y typ = map_pred (U.subst_var x y) typ
let subst_rev t x typ = map_pred (U.subst_rev t x) typ
let replace_term t1 t2 t3 =
  let x = Id.new_var t1.S.typ in
  subst x t2 @@ subst_rev t1 x t3

let rec rename var = function
  | Base(base, x, p) ->
      let x' = Option.default (Id.new_var_id x) var in
      Base(base, x', U.subst_var x x' p)
  | Fun(x, typ1, (Fun(_, typ, _) as typ2)) when !Flag.web && is_fun typ ->
      let x' = Id.new_var ~name:("@" ^ Id.name x) (Id.typ x) in
      let typ2' = subst_var x x' typ2 in
      Fun(x', rename (Some x') typ1, rename None typ2')
  | Fun(x,typ1,typ2) ->
      let x' = Id.new_var_id x in
      let typ2' = subst_var x x' typ2 in
      Fun(x', rename (Some x') typ1, rename None typ2')
  | Tuple xtyps ->
      let aux (x,typ) xtyps =
        let x' = Id.new_var_id x in
        let sbst = subst_var x x' in
        let xtyps' = List.map (Pair.map_snd sbst) xtyps in
        (x', rename (Some x') @@ sbst typ) :: xtyps'
      in
      Tuple (List.fold_right aux xtyps [])
  | Inter(typ, typs) -> Inter(typ, List.map (rename var) typs)
  | Union(typ, typs) -> Union(typ, List.map (rename var) typs)
  | ExtArg(x,typ1,typ2) ->
      let x' = Id.new_var_id x in
      let typ2' = subst_var x x' typ2 in
      ExtArg(x', rename (Some x') typ1, rename None typ2')
  | List(x,p_len,y,p_i,typ) ->
      let x' = Id.new_var_id x in
      let y' = Id.new_var_id y in
      let p_len' = U.subst_var x x' p_len in
      let p_i' = U.subst_var y y' p_i in
      let typ' = subst_var x x' typ in
      let typ'' = subst_var y y' typ' in
      List(x', p_len', y', p_i', rename None typ'')
let rename typ =
  Id.save_counter ();
  Id.clear_counter ();
  let typ' = rename None typ in
  Id.reset_counter ();
  typ'


let rec of_simple typ =
  match T.elim_tpred typ with
  | T.TUnit -> Base(Unit, Id.new_var typ, U.true_term)
  | T.TBool -> Base(Bool, Id.new_var typ, U.true_term)
  | T.TInt -> Base(Int, Id.new_var typ, U.true_term)
  | T.TData s -> Base(Abst s, Id.new_var typ, U.true_term)
  | T.TFun(x, typ) -> Fun(x, of_simple @@ Id.typ x, of_simple typ)
  | T.TTuple xs ->
      Tuple(List.map (Pair.add_right @@ of_simple -| Id.typ) xs)
  | _ ->
      if !!debug then Format.printf "%a@." Print.typ typ;
      unsupported "Ref_type.of_simple"


let rec to_simple typ =
  match typ with
  | Base(Unit, _, _) -> T.TUnit
  | Base(Bool, _, _) -> T.TBool
  | Base(Int, _, _) -> T.TInt
  | Base(Abst s, _, _) -> T.TData s
  | Fun(x,typ1,typ2) -> T.TFun(Id.new_var @@ to_simple typ1, to_simple typ2)
  | Tuple xtyps -> T.TTuple (List.map (Id.new_var -| to_simple -| snd) xtyps)
  | Inter(typ, _) -> typ
  | Union(typ, _) -> typ
  | ExtArg _ -> assert false
  | List(_,_,_,_,typ) -> T.make_tlist @@ to_simple typ
  | Exn(typ1, _) -> to_simple typ1

let to_abst_typ_base b =
  match b with
  | Unit -> T.TUnit
  | Bool -> T.TBool
  | Int -> T.TInt
  | Abst s -> T.TData s

let rec to_abst_typ typ =
  let r =
  match typ with
  | Base(b, x, t) when t = U.true_term ->
      to_abst_typ_base b
  | Base(b, x, t) ->
      let x' = Id.new_var ~name:(Id.name x) @@ to_abst_typ_base b in
      let ps = Term_util.decomp_bexp @@ U.subst_var x x' t in
      T.TPred(x', ps)
  | Fun(x,typ1,typ2) ->
      let x' = Id.new_var ~name:(Id.name x) @@ to_abst_typ typ1 in
      let typ2' = to_abst_typ @@ subst_var x x' typ2 in
      T.TFun(x', typ2')
  | Tuple xtyps ->
      let aux (x,typ) xs =
        let x' = Id.new_var ~name:(Id.name x) @@ to_abst_typ typ in
        List.map (Id.map_typ @@ U.subst_type_var x x') (x'::xs)
      in
      T.TTuple (List.fold_right aux xtyps [])
  | Inter(styp, typs)
  | Union(styp, typs) ->
      List.fold_right (Term_util.merge_typ -| to_abst_typ) typs styp
  | ExtArg _ -> unsupported "Ref_type.to_abst_typ"
  | List(x,p_len,y,p_i,typ1) ->
      if p_i.S.desc <> S.Const S.True || occur y typ1 then
        unsupported "Ref_type.to_abst_typ"
      else
        let typ1' = to_abst_typ typ1 in
        let x' = Id.new_var ~name:"xs" @@ T.make_tlist typ1' in
        if p_len = U.true_term
        then Id.typ x'
        else T.TPred(x', [U.subst x (U.make_length @@ U.make_var x') p_len])
  in
  if !!debug then Format.printf "Ref_type.to_abst_typ IN: %a@." print typ;
  if !!debug then Format.printf "Ref_type.to_abst_typ OUT: %a@." Print.typ r;
  r

let rec set_base_var x = function
  | Base(base, y, p) -> Base(base, x, U.subst_var y x p)
  | Inter(typ, typs) -> Inter(typ, List.map (set_base_var x) typs)
  | Union(typ, typs) -> Union(typ, List.map (set_base_var x) typs)
  | typ -> typ
let rec copy_fun_arg_to_base = function
  | Base(base, x, p) -> Base(base, x, p)
  | Fun(x,typ1,typ2) -> Fun(x, set_base_var x @@ copy_fun_arg_to_base typ1, copy_fun_arg_to_base typ2)
  | Tuple xtyps -> Tuple (List.map (Pair.map_snd copy_fun_arg_to_base) xtyps)
  | Inter(typ, typs) -> Inter(typ, List.map copy_fun_arg_to_base typs)
  | Union(typ, typs) -> Union(typ, List.map copy_fun_arg_to_base typs)
  | ExtArg(x,typ1,typ2) -> ExtArg(x, copy_fun_arg_to_base typ1, copy_fun_arg_to_base typ2)
  | List(x,p_len,y,p_i,typ) -> List(x, p_len, y, p_i, copy_fun_arg_to_base typ)


let rec same typ1 typ2 =
  match typ1,typ2 with
  | Base(base1,x1,p1), Base(base2,x2,p2) -> base1 = base2 && U.same_term p1 @@ U.subst_var x2 x1 p2
  | Fun(x1,typ11,typ12), Fun(x2,typ21,typ22) -> same typ11 typ21 && same typ12 @@ subst_var x2 x1 typ22
  | Tuple xtyps1, Tuple xtyps2 ->
      let typs1 = List.map snd xtyps1 in
      let typs2 = List.map (fun (_,typ) -> List.fold_left2 (fun typ (x1,_) (x2,_) -> subst_var x2 x1 typ) typ xtyps1 xtyps2) xtyps2 in
      List.eq ~eq:same typs1 typs2
  | Inter(_, typs1), Inter(_, typs2) -> List.eq ~eq:same typs1 typs2
  | Union(_, typs1), Union(_, typs2) -> List.eq ~eq:same typs1 typs2
  | ExtArg(x1,typ11,typ12), ExtArg(x2,typ21,typ22) -> same typ11 typ21 && same typ12 @@ subst_var x2 x1 typ22
  | List(x1,p1_len,y1,p1_i,typ1'), List(x2,p2_len,y2,p2_i,typ2') ->
      U.same_term p1_len @@ U.subst_var x2 x1 p2_len &&
      U.same_term p1_i @@ U.subst_var x2 x1 @@ U.subst_var y2 y1 p2_i &&
      same typ1' @@ subst_var x2 x1 @@ subst_var y2 y1 typ2'
  | _ -> false

let rec has_no_predicate typ =
  match typ with
  | Base(b, x, t) -> t = U.true_term
  | Fun(x,typ1,typ2) -> has_no_predicate typ1 && has_no_predicate typ2
  | Tuple xtyps -> List.for_all (has_no_predicate -| snd) xtyps
  | Inter(_, typs)
  | Union(_, typs) -> List.for_all has_no_predicate typs
  | ExtArg _ -> unsupported "has_no_predicate"
  | List(x,p_len,y,p_i,typ1) -> p_len = U.true_term && p_i = U.true_term && has_no_predicate typ1


let conv = Fpat.Formula.of_term -| FpatInterface.of_typed_term
let is_sat = FpatInterface.is_sat -| conv
let is_valid = FpatInterface.is_valid -| conv
let implies ts t = FpatInterface.implies (List.map conv ts) [conv t]

let rec simplify_pred t =
  if true
  then
    try
      if not @@ is_sat t then
        U.false_term
      else if is_valid t then
        U.true_term
      else
        match S.desc t with
        | S.BinOp(S.And, t1, t2) ->
            let t1' = simplify_pred t1 in
            let t2' = simplify_pred t2 in
            if implies [t1'] t2' then
              t1'
            else if implies [t2'] t1' then
              t2'
            else
              U.make_and t1' t2'
        | S.BinOp(S.Or, t1, t2) ->
            let t1' = simplify_pred t1 in
            let t2' = simplify_pred t2 in
            if implies [t1'] t2' then
              t2'
            else if implies [t2'] t1' then
              t1'
            else
              U.make_or t1' t2'
        | _ -> t
    with Unsupported _ -> t
  else
    FpatInterface.simplify_typed_term t

let rec flatten typ =
  match typ with
  | Inter(styp, typs) ->
      let typs' = List.map flatten typs in
      let typs'' = List.flatten_map decomp_inter typs' in
      Inter(styp, typs'')
  | Union(styp, typs) ->
      let typs' = List.map flatten typs in
      let typs'' = List.flatten_map decomp_union typs' in
      Union(styp, typs'')
  | _ -> typ



let make_env x typ =
  match typ with
  | Base(_,y,t) -> U.subst_var y x t
  | _ -> U.true_term
let rec subtype env typ1 typ2 =
  if 0=0 && !!debug then
    begin
      Format.printf "typ1: %a@." print typ1;
      Format.printf "typ2: %a@." print typ2
    end;
  match typ1, typ2 with
  | Base(base1, x, t1), Base(base2, y, t2) ->
      base1 = base2 && implies (t1::env) (U.subst_var y x t2)
  | Fun(x, typ11, typ12), Fun(y, typ21, typ22) ->
      let env' = make_env x typ11 :: env in
      subtype env typ21 typ11 && subtype env' typ12 (subst_var y x typ22)
  | _, Inter(_, typs) ->
      List.for_all (subtype env typ1) typs
  | Inter(_, typs), _ ->
      List.exists (subtype env -$- typ2) typs
  | Union(_, typs), _ ->
      List.for_all (subtype env typ1) typs
  | _, Union(_, typs) ->
      List.exists (subtype env -$- typ2) typs
  | Tuple xtyps1, Tuple xtyps2 ->
      let aux (env,acc) (x1,typ1) (x2,typ2) =
        make_env x1 typ1 :: env,
        acc && subtype env typ1 typ2
      in
      List.length xtyps1 = List.length xtyps2 &&
        snd @@ List.fold_left2 aux (env,true) xtyps1 xtyps2
  | Exn(typ11,typ12), Exn(typ21,typ22) ->
      subtype env typ11 typ21 && subtype env typ12 typ22
  | _, Exn(typ21,typ22) ->
      subtype env typ1 typ21
  | Exn _, _ -> unsupported "Ref_type.subtype Exn"
  | _ ->
      Format.printf "typ1: %a@." print typ1;
      Format.printf "typ2: %a@." print typ2;
      unsupported "Ref_type.subtype"
let subtype typ1 typ2 = subtype [] typ1 typ2

let equiv typ1 typ2 = subtype typ1 typ2 && subtype typ2 typ1

let rec remove_if f typs =
  let rec aux acc typs =
    match typs with
    | [] -> acc
    | typ::typs' ->
        let acc' =
          if List.exists (f -$- typ) (acc@typs') then
            acc
          else
            typ::acc
        in
        aux acc' typs'
  in
  aux [] typs

let rec remove_subtype typs = remove_if subtype typs
let rec remove_equiv typs = remove_if equiv typs

let rec simplify_typs constr styp is_zero make_zero and_or typs =
  let decomp typ =
    match typ with
    | Inter(_, typs) -> typs
    | Union(_, typs) -> typs
    | typ -> [typ]
  in
  if !!debug then Format.printf "ST@.";
  let typs' =
    typs
    |> List.map simplify
    |> remove_subtype
    |*> List.unique ~cmp:same
    |> constr styp
    |> flatten
    |> decomp
  in
  if !!debug then Format.printf "ST: @[%a ==>@ %a@." (List.print print) typs (List.print print) typs';
  if List.exists is_zero typs' then
    make_zero styp
  else
    match typs' with
    | [] -> constr styp []
    | [typ] -> typ
    | _ ->
        if List.for_all is_base typs' then
          let bs,xs,ts = List.split3 @@ List.map (Option.get -| decomp_base) typs' in
          let base = List.hd bs in
          assert (List.for_all ((=) base) bs);
          let x = List.hd xs in
          let ts' = List.map2 (U.subst_var -$- x) xs ts in
          Base(base, x, and_or ts')
        else if List.for_all is_fun typs' then
          let xs,typs1,typs2 = List.split3 @@ List.map (Option.get -| decomp_fun) typs' in
          if List.for_all (same @@ List.hd typs1) @@ List.tl typs1 then
            let x = List.hd xs in
            let typs2' = List.map2 (subst_var -$- x) xs typs2 in
            let styp' = to_simple @@ List.hd typs2 in
            Fun(x, List.hd typs1, simplify_typs constr styp' is_zero make_zero and_or typs2')
          else
            flatten @@ constr styp typs'
(*
    else if List.for_all is_list typs' then
      let xs,p_lens,ys,p_is,typs'' = List.split3 @@ List.map (Option.get -| decomp_fun) typs' in
*)
        else
          flatten @@ constr styp typs'

and simplify typ =
  let r =
  match flatten typ with
  | Base(base, x, p) ->
      let p' = simplify_pred p in
      if p' = U.false_term
      then Union(to_simple typ, [])
      else Base(base, x, p')
  | Fun(x,typ1,typ2) ->
      begin
        match simplify typ1 with
        | Union(_, []) -> Inter(to_simple typ, [])
        | typ1' -> Fun(x, typ1', simplify typ2)
      end
  | Tuple xtyps -> Tuple (List.map (Pair.map_snd simplify) xtyps)
  | Inter(styp, []) -> Inter(styp, [])
  | Inter(styp, typs) -> simplify_typs _Inter styp is_bottom (_Union -$- []) U.make_ands typs
  | Union(styp, []) -> Union(styp, [])
  | Union(styp, typs) -> simplify_typs _Union styp is_top (_Inter -$- []) U.make_ors typs
  | ExtArg(x,typ1,typ2) -> ExtArg(x, simplify typ1, simplify typ2)
  | List(x,p_len,y,p_i,typ) ->
      let p_len' = simplify_pred p_len in
      if p_len' = U.false_term
      then Union(to_simple typ, [])
      else List(x, p_len', y, simplify_pred p_i, simplify typ)
  in
  if !!debug then Format.printf "REF_TYPE SIMPLIFY @[@[%a@] ==>@ @[%a@]@." print typ print r;
  r


(*
let from_fpat_const typ =
  match typ with
  | Fpat.TypConst.Unit -> Unit
  | Fpat.TypConst.Bool -> Bool
  | Fpat.TypConst.Int -> Int
  | Fpat.TypConst.Ext "X" -> Unit
  | _ -> unsupported "Ref_type.from_fpat"
let rec from_fpat typ =
  match typ with
  | Fpat.RefT.Bot -> Base(Int, Id.new_var T.TInt, U.false_term)
  | Fpat.RefT.Top -> Inter []
  | Fpat.RefT.Base(x, c, p) ->
      let base = from_fpat_const c in
      let typ =
        match base with
        | Int -> T.TInt
        | Bool -> T.TBool
        | Unit -> T.TUnit
        | _ -> assert false
      in
      let x' = Id.from_string (Fpat.Idnt.string_of x) typ in
      let t = U.from_fpat_formula p in
      Base(base, x', t)
  | Fpat.RefT.Fun typs ->
      let aux (typ1,typ2) =
        let typ1' = from_fpat typ1 in
        let typ2' = from_fpat typ2 in
        let x =
          let typ1_simple = to_simple typ1' in
          if is_base typ1'
          then Id.from_string (Fpat.Idnt.string_of @@ Fpat.RefT.bv_of typ1) typ1_simple
          else Id.new_var typ1_simple
        in
        Fun(x, typ1', typ2')
      in
      _Inter @@ List.map aux typs
 *)

let rec make_strongest typ =
  match typ with
  | T.TUnit -> Base(Unit, Id.new_var typ, U.false_term)
  | T.TBool -> Base(Bool, Id.new_var typ, U.false_term)
  | T.TInt -> Base(Int, Id.new_var typ, U.false_term)
  | T.TData s -> Base(Abst s, Id.new_var typ, U.false_term)
  | T.TFun(x, typ) -> Fun(x, make_weakest @@ Id.typ x, make_strongest typ)
  | T.TTuple xs -> Tuple(List.map (Pair.add_right (make_strongest -| Id.typ)) xs)
  | T.TApp(T.TList, _) -> unsupported "Ref_type.make_strongest TList"
  | _ when typ = U.typ_result -> Base(Unit, Id.new_var typ, U.false_term)
  | _ ->
      Format.printf "make_strongest: %a@." Print.typ typ;
      unsupported "Ref_type.make_strongest"

and make_weakest typ =
  match typ with
  | T.TUnit -> Base(Unit, Id.new_var typ, U.true_term)
  | T.TBool -> Base(Bool, Id.new_var typ, U.true_term)
  | T.TInt -> Base(Int, Id.new_var typ, U.true_term)
  | T.TData s -> Base(Abst s, Id.new_var typ, U.true_term)
  | T.TFun(x, typ) -> Fun(x, make_strongest @@ Id.typ x, make_weakest typ)
  | T.TTuple xs -> Tuple(List.map (Pair.add_right (make_weakest -| Id.typ)) xs)
  | T.TApp(T.TList, _) -> unsupported "Ref_type.make_weakest List"
  | _ when typ = U.typ_result -> Base(Unit, Id.new_var typ, U.true_term)
  | _ ->
      Format.printf "make_weakest: %a@." Print.typ typ;
      unsupported "Ref_type.make_weakest"

let inter styp typs = simplify @@ Inter(styp, typs)
let union styp typs = simplify @@ Union(styp, typs)


let decomp_funs_and_classify typs =
  let typs' =
    let decomp = function
      | Fun(y, typ1, typ2) -> y, typ1, typ2
      | _ -> assert false
    in
    List.map decomp typs
  in
  List.classify ~eq:(fun (_,typ1,_) (_,typ2,_) -> same typ1 typ2) typs'
let merge constr typs =
  let x,typ1,_ = List.hd typs in
  let typs' = List.map (fun (y,_,typ2) -> subst_var y x typ2) typs in
  Fun(x, typ1, constr typs')
let rec push_inter_union_into typ =
  match typ with
  | Inter(styp, (Fun _::_ as typs)) ->
      let typss = decomp_funs_and_classify @@ List.map push_inter_union_into typs in
      inter styp @@ List.map (merge @@ inter styp) typss
  | Union(styp, (Fun _::_ as typs)) ->
      let typss = decomp_funs_and_classify @@ List.map push_inter_union_into typs in
      union styp @@ List.map (merge @@ union styp) typss
  | _ -> typ



let rec make_rand typ =
  match typ with
  | T.TVar {contents=None} -> unsupported "make_randue_base"
  | T.TVar {contents=Some typ} -> make_rand typ
  | T.TFun(x,typ) ->
      let t =
        match Id.typ x with
        | typ when T.is_base_typ typ -> U.unit_term
        | T.TFun(x',typ') -> U.make_br U.unit_term @@ U.make_seq (U.make_app (U.make_var x) [make_rand @@ Id.typ x']) U.unit_term
        | _ -> unsupported "make_rand"
      in
      U.make_fun (Id.new_var_id x) @@ U.make_seq t @@ make_rand typ
  | T.TTuple xs -> U.make_tuple @@ List.map (make_rand -| Id.typ) xs
  | T.TData _ -> unsupported "make_rand"
  | T.TApp(T.TRef, _) -> unsupported "make_rand"
  | T.TApp(T.TOption, _) -> unsupported "make_rand"
  | T.TPred(x,_) -> make_rand @@ Id.typ x
  | _ -> U.make_randvalue_unit typ


let rec generate_check genv cenv x typ =
  if !!debug then Format.printf "Ref_type.generate_check: %a : %a@." Id.print x print typ;
  match typ with
  | Base(base, y, p) ->
      genv, cenv, U.subst_var y x p
  | Fun(y,typ1,typ2) ->
      let genv',cenv',t_typ1 = generate genv cenv typ1 in
      let z = Id.new_var t_typ1.S.typ in
      let t_typ2 = U.make_app (U.make_var x) [U.make_var z] in
      let r = Id.new_var ~name:"r" t_typ2.S.typ in
      let genv'',cenv'',t_typ2' =
        let typ2' = subst_var y z typ2 in
        generate_check genv' cenv' r typ2'
      in
      genv'', cenv'', U.make_lets [z,[],t_typ1; r,[],t_typ2] t_typ2'
  | Tuple xtyps ->
      let xs' = List.map (fun (x,typ) -> Id.new_var ~name:(Id.name x) @@ to_simple typ) xtyps  in
      let typs = List.fold_right2 (fun x' (x,typ) acc -> typ :: List.map (subst_var x x') acc) xs' xtyps [] in
      let genv',cenv',ts = List.fold_right2 (fun x' typ (genv,cenv,ts) -> let genv',cenv',t = generate_check genv cenv x' typ in genv',cenv',t::ts) xs' typs (genv,cenv,[]) in
      genv', cenv', U.make_lets (List.mapi (fun i x' -> x', [], U.make_proj i @@ U.make_var x) xs') @@ U.make_ands ts
  | List(l,p_len,y,p_i,typ1) when p_i.S.desc = S.Const S.True && not @@ occur y typ1 ->
      let styp = to_simple typ in
      let atyp1 = to_abst_typ typ1 in
      let atyp = to_abst_typ typ in
      let l' = Id.new_var ~name:"l" T.TInt in
      let add_len t =
        let t' = U.make_and t @@ U.subst_var l l' p_len in
        if t' = U.true_term
        then U.true_term
        else U.make_let [l',[],U.make_length @@ U.make_var x] t'
      in
      let typ1' = subst_var l l' typ1 in
      let genv',cenv',t =
        if List.mem_assoc ~eq:same typ cenv
        then
          let f,_,_ = List.assoc ~eq:same typ cenv in
          genv, cenv, U.make_app (U.make_var f) [U.make_var x]
        else
          let zs = Id.new_var ~name:"xs" atyp in
          let f = Id.new_var ~name:("check_" ^ T.to_id_string styp) @@ T.TFun(zs,T.TBool) in
          let z = Id.new_var ~name:"x" atyp1 in
          let zs' = Id.new_var ~name:"xs'" atyp in
          let genv',cenv',t_b1 = generate_check genv cenv z typ1' in
          if t_b1 = U.true_term
          then
            genv', cenv', U.true_term
          else
            let t_body =
              let pat_nil = U.make_pnil styp, U.true_term, U.true_term in
              let pat_cons =
                let t_b2 = U.make_app (U.make_var f) [U.make_var zs'] in
                U.make_pcons (U.make_pvar z) (U.make_pvar zs'), U.true_term, U.make_and t_b1 t_b2
              in
              U.make_match (U.make_var zs) [pat_nil; pat_cons]
            in
            let def = f, [zs], U.add_comment (Format.asprintf "CHECK: %a" print typ) t_body in
            if debug() then Format.printf "CHECK: %a: %a@." print typ (Triple.print Print.id (List.print Print.id) Print.term) def;
            let t = U.make_app (U.make_var f) [U.make_var x] in
            if List.Set.supset ~eq:Id.eq [zs;U.length_var;f] @@ U.get_fv t_body
            then genv'@[typ,def], cenv', t
            else genv', cenv', U.make_letrec [def] t
      in
      genv', cenv', add_len t
  | Inter(_, typs) ->
      let aux (genv',cenv',ts) typ =
        let genv'',cenv'',t = generate_check genv' cenv' x typ in
        genv'', cenv'', ts@[t]
      in
      let genv'',cenv'',ts = List.fold_left aux (genv,cenv,[]) typs in
      if !!debug then Format.printf "generate_check typ: %a@." (List.print print) typs;
      if !!debug then Format.printf "generate_check ts: %a@." (List.print  Print.term) ts;
      genv'', cenv'', U.make_ands ts
  | Union(_, typs) ->
      let aux (genv',cenv',ts) typ =
        let genv'',cenv'',t = generate_check genv' cenv' x typ in
        genv'', cenv'', ts@[t]
      in
      let genv'',cenv'',ts = List.fold_left aux (genv,cenv,[]) typs in
      genv'', cenv'', U.make_ors ts
  | _ -> Format.printf "%a@." print typ; unsupported "Ref_type.generate_check"
and generate_simple typ = U.make_fail typ

and generate genv cenv typ =
  if !!debug then Format.printf "Ref_type.generate: %a@." print typ;
  let genv',cenv',t =
    match typ with
    | Base(Int, x, p) ->
        let x' = Id.new_var T.TInt in
        let genv',cenv',t_check = generate_check genv cenv x' typ in
        genv', cenv', U.make_let [x',[],U.randint_unit_term] @@ U.make_assume t_check @@ U.make_var x'
    | Base(Bool, x, p) ->
        let x' = Id.new_var T.TBool in
        let genv',cenv',t_check = generate_check genv cenv x' typ in
        genv', cenv', U.make_let [x',[],U.randbool_unit_term] @@ U.make_assume t_check @@ U.make_var x'
    | Base(Unit, x, p) ->
        let genv',cenv',t_check = generate_check genv cenv x typ in
        genv', cenv', U.make_assume t_check U.unit_term
    | Base(Abst s, x, p) ->
        let typ' = to_simple typ in
        let x' = Id.new_var typ' in
        let genv',cenv',t_check = generate_check genv cenv x' typ in
        genv', cenv', U.make_let [x',[],U.make_randvalue_unit typ'] @@ U.make_assume t_check @@ U.make_var x'
    | Fun(x,typ1,typ2) ->
        let x' = Id.new_var @@ to_abst_typ typ1 in
        let typ2' = subst_var x x' typ2 in
        let genv',cenv',t_typ1 = generate_check genv cenv x' typ1 in
        if !!debug then Format.printf "Ref_type.generate t_typ1: %a@." Print.term t_typ1;
        let t1 = U.make_or U.randbool_unit_term t_typ1 in
        let genv'',cenv'',t2 = generate genv' cenv' typ2' in
        let t3 = generate_simple @@ to_simple typ2' in
        genv'', cenv'', U.make_fun x' @@ U.add_comment (Format.asprintf "GEN FUN: %a" print typ2) @@ U.make_if t1 t2 t3
    | Tuple xtyps ->
        let xs' = List.map (fun (x,typ) -> Id.new_var ~name:(Id.name x) @@ to_simple typ) xtyps  in
        let typs = List.fold_right2 (fun x' (x,typ) acc -> typ :: List.map (subst_var x x') acc) xs' xtyps [] in
        let genv',cenv',ts = List.fold_right (fun typ (genv,cenv,ts) -> let genv',cenv',t = generate genv cenv typ in genv',cenv',t::ts) typs (genv,cenv,[]) in
        genv', cenv', U.make_lets (List.map2 (fun x t -> x,[],t) xs' ts) @@ U.make_tuple @@ List.map U.make_var xs'
    | Inter(styp, []) -> generate genv cenv @@ make_weakest styp
    | Inter(_, [typ]) -> generate genv cenv typ
    | Inter(_, Base(base,x,p)::typs) ->
        let p' =
          let aux p typ =
            match typ with
            | Base(base', x', p') ->
                assert (base = base');
                U.make_and p (U.subst_var x' x p')
            | _ -> assert false
          in
          List.fold_left aux p typs
        in
        generate genv cenv @@ Base(base, x, p')
    | Inter(_, ((Fun _)::_ as typs)) ->
        Flag.fail_as_exception := true;
        let bss = Combination.take_each @@ List.map (Fun.const [true;false]) typs in
        if !!debug then Format.printf "GEN bss: %a@." (List.print @@ List.print Format.pp_print_bool) bss;
        let x =
          match typs with
          | Fun(_,typ1,_)::_ -> Id.new_var @@ to_abst_typ typ1
          | _ -> assert false
        in
        let typs1,typs2 = List.split_map (function Fun(y,typ1,typ2) -> typ1, subst_var y x typ2 | _ -> assert false) typs in
        if !!debug then Format.printf "GEN typs1: %a@." (List.print print) typs1;
        if !!debug then Format.printf "GEN typs2: %a@." (List.print print) typs2;
        let xs = List.map (fun _ -> Id.new_var ~name:"b" T.TBool) typs in
        if !!debug then Format.printf "GEN xs: %a@." (List.print Id.print) xs;
        let genv',cenv',tbs =
          let aux typ1 (genv,cenv,tbs) =
            let genv', cenv', tb = generate_check genv cenv x typ1 in
            let tb' =
              let e = Id.new_var ~name:"e" U.typ_exn in
              U.make_trywith tb e [U.make_pany @@ Id.typ e, U.true_term, U.false_term]
              |> U.make_or U.randbool_unit_term
              |*> U.add_comment @@ Format.asprintf "GEN INTER: beta(%a)" print typ1
            in
            genv', cenv', tb'::tbs
          in
          List.fold_right aux typs1 (genv,cenv,[])
        in
        if !!debug then Format.printf "GEN tbs: %a@." (List.print Print.term) tbs;
        let tcs =
          let aux bs =
            xs
            |> List.map U.make_var
            |> List.filter_map2 Option.some_if bs
            |> U.make_ands
          in
          List.map aux bss
        in
        if !!debug then Format.printf "GEN tcs: %a@." (List.print Print.term) tcs;
        let rstyp = to_simple @@ List.hd typs2 in
        let genv'',cenv'',trs =
          let aux bs (genv,cenv,trs) =
            let typ =
              typs2
              |> List.filter_map2 Option.some_if bs
              |> inter rstyp
            in
            if !!debug then Format.printf "GEN typ: %a@." print typ;
            let genv',cenv',tr = generate genv cenv typ in
            genv', cenv', tr::trs
          in
          List.fold_right aux bss (genv',cenv',[])
        in
        if !!debug then Format.printf "GEN trs: %a@." (List.print Print.term) trs;
        let t =
          U.make_bottom rstyp
          |> List.fold_right2 U.make_if tcs trs
          |> U.make_lets @@ List.map2 (fun x tb -> x, [], tb) xs tbs
          |> U.make_fun x
        in
        if !!debug then Format.printf "GEN t: %a@."  Print.term t;
        genv'', cenv'', t
    | Inter(_, _) ->
        Format.printf "INTER: %a@." print typ;
        unsupported "Ref_type.generate: Inter"
    | Union(styp, []) -> [], [], U.make_bottom styp
    | Union(_, [typ]) -> generate genv cenv typ
    | Union(_, typs) -> unsupported "Ref_type.generate: Union"
    | ExtArg(x,typ1,typ2) -> unsupported "Ref_type.generate: ExtArg"
    | List(x,p_len,y,p_i,typ') ->
        if p_i.S.desc <> S.Const S.True || occur y typ' then
          unsupported "Ref_type.generate"
        else
          let styp = to_simple typ in
          let l = Id.new_var ~name:"l" T.TInt in
          let p_len' = U.subst_var x l p_len in
          let genv',cenv',t =
            if List.mem_assoc ~eq:same typ genv
            then
              let f,_,_ = List.assoc ~eq:same typ genv in
              let t = U.make_app (U.make_var f) [U.make_var l] in
              genv, cenv, t
            else
              let n = Id.new_var ~name:"n" T.TInt in
              let f = Id.new_var ~name:("make_r_" ^ T.to_id_string styp) @@ T.TFun(n, to_abst_typ typ) in
              let t_nil = U.make_nil2 styp in
              let genv',cenv',t_typ' = generate genv cenv typ' in
              let t_cons = U.make_cons t_typ' @@ U.make_app (U.make_var f) [U.make_sub (U.make_var n) (U.make_int 1)] in
              let t_b = U.make_leq (U.make_var n) (U.make_int 0) in
              let def = f, [n], U.add_comment (Format.asprintf "GEN LIST: %a" print typ) @@ U.make_if t_b t_nil t_cons in
              let t = U.make_app (U.make_var f) [U.make_var l] in
              if debug() then Format.printf "GEN: %a: %a@." print typ (Triple.print Print.id (List.print Print.id) Print.term) def;
              if List.Set.supset ~eq:Id.eq [n] @@ U.get_fv @@ Triple.trd def
              then genv'@[typ,def], cenv', t
              else genv', cenv', U.make_letrec [def] t
          in
          genv', cenv', U.make_let [l,[],U.randint_unit_term] @@ U.make_assume p_len' t
  in
  if !!debug then Format.printf "Ref_type.generate': %a@." print typ;
  genv', cenv', {t with S.typ = to_abst_typ typ}

module Value = struct
  type t' = t
  type t = t'
  let print = print
  let merge typ1 typ2 = [inter (to_simple typ1) [typ1; typ2]]
  let eq = equiv
end
module Env = Ext.Env.Make(Syntax.ID)(Value)
type env = Env.t
