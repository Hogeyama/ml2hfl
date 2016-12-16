open Util

module S = Syntax
module U = Term_util
module T = Type

module Debug = Debug.Make(struct let check = make_debug_check __MODULE__ end)

type base =
  | Unit
  | Bool
  | Int
  | Abst of string

type t =
  | Base of base * S.id * S.term
  | Fun of S.id * t * t
  | Tuple of (S.id * t) list
  | Inter of S.typ * t list
  | Union of S.typ * t list
  | ExtArg of S.id * t * t
  | List of S.id * S.term * S.id * S.term * t
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
  | Base(_, _, {S.desc=S.Const S.False}) -> true
  | _ -> false
let is_top typ =
  match typ with
  | Inter(_, []) -> true
  | Base(_, _, {S.desc=S.Const S.True}) -> true
  | _ -> false
let rec is_bottom' typ =
  match typ with
  | Union(_, []) -> true
  | Base(_, _, {S.desc=S.Const S.False}) -> true
  | Fun(_, typ1, typ2) -> is_top typ1 && is_bottom' typ2
  | Tuple xtyps -> List.exists (snd |- is_bottom') xtyps
  | _ -> false
and is_top' typ =
  match typ with
  | Inter(_, []) -> true
  | Base(_, _, {S.desc=S.Const S.True}) -> true
  | Fun(_, typ1, typ2) -> is_bottom' typ1
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
  | Exn(typ1, typ2) -> occur x typ1 || occur x typ2

let rec decomp_funs typ =
  match typ with
  | Fun(_,_,Exn _) -> [], typ
  | Fun(x,typ1,typ2) ->
      Pair.map_fst (List.cons (x,typ1)) @@ decomp_funs typ2
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
      Format.fprintf fm "Bot"
  | Base(base,x,p) ->
      Format.fprintf fm "{%a:%a | %a}" Id.print x print_base base Print.term p
  | Fun(x, typ1, Exn(typ2, typ3)) when is_bottom typ3 ->
      print fm (Fun(x, typ1, typ2))
  | Fun(x, typ1, Exn(typ2, typ3)) ->
      let arg =
        if occur x typ2 || occur x typ3 then
          Format.asprintf "%a:" Id.print x
        else
          ""
      in
      Format.fprintf fm "(@[<hov 2>%s%a ->^[%a]@ %a@])" arg print typ1 print typ3 print typ2
  | Fun _ as typ ->
      let rec aux fm (xtyps, typ) =
        match xtyps with
        | [] -> print fm typ
        | (x,typ1)::xtyps' ->
            if List.exists (occur x) @@ typ :: List.map snd xtyps'
            then Format.fprintf fm "@[<hov 2>%a:%a ->@ %a@]" Id.print x print typ1 aux (xtyps', typ)
            else Format.fprintf fm "@[<hov 2>%a ->@ %a@]" print typ1 aux (xtyps', typ)
      in
      Format.fprintf fm "(%a)" aux @@ decomp_funs typ
  | Tuple xtyps ->
      let n = List.length xtyps in
      let pr fm (i,(x,typ)) =
        if i < n-1 && occur x @@ Tuple xtyps
        then Format.fprintf fm "%a:" Id.print x;
        print fm typ
      in
      Format.fprintf fm "(@[%a@])" (print_list pr " *@ ") @@ List.mapi Pair.pair xtyps
  | Inter(styp, []) when !!Debug.check -> Format.fprintf fm "Top(%a)" Print.typ styp
  | Inter(_, []) -> Format.fprintf fm "Top"
  | Inter(_, [typ]) -> print fm typ
  | Inter(_, typs) -> Format.fprintf fm "(@[%a@])" (print_list print " /\\@ ") typs
  | Union(styp, []) when !!Debug.check -> Format.fprintf fm "Bot(%a)" Print.typ styp
  | Union(_, []) -> Format.fprintf fm "Bot"
  | Union(_, [typ]) -> print fm typ
  | Union(_, typs) -> Format.fprintf fm "(@[%a@])" (print_list print " \\/@ ") typs
  | ExtArg(x,typ1,typ2) ->
      Format.fprintf fm "(@[%a where %a:%a@])" print typ2 Id.print x print typ1
  | List(x,p_len,y,p_i,typ2) ->
      Format.fprintf fm "(@[";
      if p_i = U.true_term then
        if occur y typ2
        then Format.fprintf fm "[%a]%a " Id.print y print typ2
        else Format.fprintf fm "%a " print typ2
      else
        Format.fprintf fm "[%a: %a]%a " Id.print y Print.term p_i print typ2;
      if p_len <> U.true_term then
        Format.fprintf fm "|%a: %a|" Id.print x Print.term p_len
      else
        if List.exists (Id.same x) (U.get_fv p_i) || occur x typ2
        then Format.fprintf fm "|%a|" Id.print x;
      Format.fprintf fm "list@])"
  | Exn(typ1, typ2) ->
      if is_bottom typ2 then
        print fm typ1
      else
        Format.fprintf fm "(@[<hov 2>%a@ |^[%a]@])" print typ1 print typ2

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
      Debug.printf "%a@." print typ;
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
  | Exn(typ, _) -> arg_num typ

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
  | Exn(typ1, typ2) -> Exn(rename var typ1, rename var typ2)
let rename typ =
  Id.save_counter ();
  Id.clear_counter ();
  let typ' = rename None typ in
  Id.reset_counter ();
  typ'


let rec of_simple typ =
  match T.elim_tattr typ with
  | T.TUnit -> Base(Unit, Id.new_var typ, U.true_term)
  | T.TBool -> Base(Bool, Id.new_var typ, U.true_term)
  | T.TInt -> Base(Int, Id.new_var typ, U.true_term)
  | T.TData s -> Base(Abst s, Id.new_var typ, U.true_term)
  | T.TFun(x, typ) -> Fun(x, of_simple @@ Id.typ x, of_simple typ)
  | T.TTuple xs -> Tuple(List.map (Pair.add_right @@ of_simple -| Id.typ) xs)
  | _ ->
      Debug.printf "%a@." Print.typ typ;
      unsupported "Ref_type.of_simple"


let to_simple_base b =
  match b with
  | Unit -> T.TUnit
  | Bool -> T.TBool
  | Int -> T.TInt
  | Abst s -> T.TData s

let rec to_simple typ =
  match typ with
  | Base(b, _, _) -> to_simple_base b
  | Fun(x,typ1,typ2) -> T.TFun(Id.new_var @@ to_simple typ1, to_simple typ2)
  | Tuple xtyps -> T.TTuple (List.map (Id.new_var -| to_simple -| snd) xtyps)
  | Inter(typ, _) -> typ
  | Union(typ, _) -> typ
  | ExtArg _ -> assert false
  | List(_,_,_,_,typ) -> T.make_tlist @@ to_simple typ
  | Exn(typ1, _) -> to_simple typ1

let rec to_abst_typ typ =
  let r =
  match typ with
  | Base(b, x, t) when t = U.true_term ->
      to_simple_base b
  | Base(b, x, t) ->
      let x' = Id.new_var_id x in
      let typ' = to_simple_base b in
      let ps = Term_util.decomp_bexp @@ U.subst_var x x' t in
      T.TAttr([T.TAPred(x', ps)], typ')
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
        if p_len = U.true_term then
          Id.typ x'
        else
          T.TAttr([T.TAPred(x', [U.subst x (U.make_length @@ U.make_var x') p_len])], Id.typ x')
  | Exn(typ1, _) -> to_abst_typ typ1
  in
  Debug.printf "Ref_type.to_abst_typ IN: %a@." print typ;
  Debug.printf "Ref_type.to_abst_typ OUT: %a@." Print.typ r;
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
  | Exn(typ1,typ2) -> Exn(copy_fun_arg_to_base typ1, copy_fun_arg_to_base typ2)


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
  | Exn(typ1,typ2) -> has_no_predicate typ1 && has_no_predicate typ2


let conv = Fpat.Formula.of_term -| FpatInterface.of_term
let is_sat = FpatInterface.is_sat -| conv
let is_valid = FpatInterface.is_valid -| conv
let implies ts t = FpatInterface.implies (List.map conv ts) [conv t]

let rec simplify_pred t =
  if true then
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
    FpatInterface.simplify_term t

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
  let r =
  match typ1, typ2 with
  | Base(base1, x, t1), Base(base2, y, t2) ->
      base1 = base2 && implies (t1::env) (U.subst_var y x t2)
  | Fun(x, typ11, typ12), Fun(y, typ21, typ22) ->
      let env' = make_env x typ11 :: env in
      subtype env typ21 typ11 && subtype env' typ12 (subst_var y x typ22)
  | _, Inter(_, typs) ->
      List.for_all (subtype env typ1) typs
  | Union(_, typs), _ ->
      List.for_all (subtype env -$- typ2) typs
  | Inter(_, typs), _ ->
      List.exists (subtype env -$- typ2) typs || is_top' typ2
  | _, Union(_, typs) ->
      List.exists (subtype env typ1) typs || is_bottom typ1
  | Tuple xtyps1, Tuple xtyps2 ->
      let aux (env,acc) (x1,typ1) (x2,typ2) =
        make_env x1 typ1 :: env,
        acc && subtype env typ1 typ2
      in
      List.length xtyps1 = List.length xtyps2 &&
        snd @@ List.fold_left2 aux (env,true) xtyps1 xtyps2
  | Exn(typ11,typ12), Exn(typ21,typ22) ->
      subtype env typ11 typ21 && subtype env typ12 typ22
  | Exn(typ11,typ12), _ when is_bottom typ12 -> subtype env typ11 typ2
  | _, Exn(typ21,typ22) -> subtype env typ1 typ21
  | Exn _, _ -> false
  | List(x1,p_len1,y1,p_i1,typ1'), List(x2,p_len2,y2,p_i2,typ2') ->
      let typ1'' = Tuple [x1,Base(Int,x1,p_len1); Id.new_var Type.TInt,Fun(y1,Base(Int,y1,p_i1),typ1')] in
      let typ2'' = Tuple [x2,Base(Int,x2,p_len2); Id.new_var Type.TInt,Fun(y2,Base(Int,y2,p_i2),typ2')] in
      subtype env typ1'' typ2''
  | _ ->
      Format.printf "typ1: %a@." print typ1;
      Format.printf "typ2: %a@." print typ2;
      unsupported "Ref_type.subtype"
  in
  if 0=0 then
    begin
      Debug.printf "typ1: %a@." print typ1;
      Debug.printf "typ2: %a@." print typ2;
      Debug.printf "r: %b@." r
    end;
  r

let subtype typ1 typ2 = subtype [] typ1 typ2
let suptype typ1 typ2 = subtype typ2 typ1

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

let rec remove_subtype ?(sub=subtype) typs = remove_if sub typs
let rec remove_equiv typs = remove_if equiv typs

let rec simplify_typs constr sub styp is_zero make_zero and_or typs =
  let decomp typ =
    match typ with
    | Inter(_, typs) -> typs
    | Union(_, typs) -> typs
    | typ -> [typ]
  in
  Debug.printf "ST@.";
  let typs' =
    typs
    |> List.map simplify
    |> remove_subtype ~sub
    |*> List.unique ~cmp:same
    |> constr styp
    |> flatten
    |> decomp
  in
  let typs'' =
    let remove_exn = function Exn(typ,_) | typ -> typ in
    if List.length typs' >= 2 && List.exists (function Exn _ -> false | _ -> true) typs' then
      List.map remove_exn typs'
    else
      typs'
  in
  Debug.printf "ST: @[%a ==>@ %a@." (List.print print) typs (List.print print) typs'';
  if List.exists is_zero typs'' then
    make_zero styp
  else
    match typs'' with
    | [] -> constr styp []
    | [typ] -> typ
    | _ ->
        if List.for_all is_base typs'' then
          let bs,xs,ts = List.split3 @@ List.map (Option.get -| decomp_base) typs'' in
          let base = List.hd bs in
          assert (List.for_all ((=) base) bs);
          let x = List.hd xs in
          let ts' = List.map2 (U.subst_var -$- x) xs ts in
          Base(base, x, and_or ts')
        else if List.for_all is_fun typs'' then
          let xs,typs1,typs2 = List.split3 @@ List.map (Option.get -| decomp_fun) typs'' in
          if List.for_all (same @@ List.hd typs1) @@ List.tl typs1 then
            let x = List.hd xs in
            let typs2' = List.map2 (subst_var -$- x) xs typs2 in
            let styp' = to_simple @@ List.hd typs2 in
            Fun(x, List.hd typs1, simplify_typs constr sub styp' is_zero make_zero and_or typs2')
          else
            flatten @@ constr styp typs''
(*
    else if List.for_all is_list typs' then
      let xs,p_lens,ys,p_is,typs'' = List.split3 @@ List.map (Option.get -| decomp_fun) typs' in
*)
        else
          flatten @@ constr styp typs''

and simplify typ =
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
  | Tuple xtyps ->
      let xtyps' = List.map (Pair.map_snd simplify) xtyps in
      if List.exists (snd |- is_bottom) xtyps' then
        Union(to_simple typ, [])
      else
        Tuple xtyps'
  | Inter(styp, []) -> Inter(styp, [])
  | Inter(styp, typs) ->
      let _Inter' styp typs =
        if typs <> [] && List.for_all (function Tuple _ -> true | _ -> false) typs then
          let xtypss = List.map (function Tuple xs -> xs | _ -> assert false) typs in
          let xs,typss =
            match xtypss with
            | [] -> assert false
            | xtyps::ytypss ->
                let xs = List.map fst xtyps in
                let rename ytyps =
                  List.fold_right2 (fun x (y,typ) acc -> let sbst = subst_var x y in sbst typ :: List.map sbst acc) xs ytyps []
                in
                xs, List.map snd xtyps :: List.map rename ytypss
          in
          let typss' = List.transpose typss in
          Tuple (List.map2 (fun x typs -> x, simplify @@ _Inter (to_simple @@ List.hd typs) typs) xs typss')
        else
          _Inter styp typs
      in
      simplify_typs _Inter' subtype styp is_bottom (_Union -$- []) U.make_ands typs
  | Union(styp, []) -> Union(styp, [])
  | Union(styp, typs) -> simplify_typs _Union suptype styp is_top (_Inter -$- []) U.make_ors typs
  | ExtArg(x,typ1,typ2) -> ExtArg(x, simplify typ1, simplify typ2)
  | List(x,p_len,y,p_i,typ) ->
      let p_len' = simplify_pred p_len in
      if p_len' = U.false_term
      then Union(to_simple typ, [])
      else List(x, p_len', y, simplify_pred p_i, simplify typ)
  | Exn(typ1,typ2) ->
      let typ1' = simplify typ1 in
      let typ2' = simplify typ2 in
      if is_bottom typ2' then
        typ1'
      else
        Exn(typ1', typ2')


let rec make_strongest typ =
  match T.elim_tattr typ with
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
  | T.TAttr([T.TAPureFun],T.TFun(x, typ)) -> Fun(x, make_weakest @@ Id.typ x, make_weakest typ)
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



module Value = struct
  type t' = t
  type t = t'
  let print = print
  let merge typ1 typ2 = [inter (to_simple typ1) [typ1; typ2]]
  let eq = equiv
end
module Env = Ext.Env.Make(Syntax.ID)(Value)
type env = Env.t

module NegValue = struct
  type t' = t
  type t = t'
  let print = print
  let merge typ1 typ2 = [union (to_simple typ1) [typ1; typ2]]
  let eq = equiv
end
module NegEnv = Ext.Env.Make(Syntax.ID)(NegValue)
type neg_env = NegEnv.t


let rec contract typ =
  match flatten typ with
  | Base _ -> typ
  | Fun(x, typ1, typ2) -> Fun(x, contract typ1, contract typ2)
  | Tuple xtyps -> Tuple (List.map (Pair.map_snd contract) xtyps)
  | Inter(typ, typs) ->
      let typs = List.map contract typs in
      let typs' = List.fold_right (fun typ acc -> if List.exists (equiv typ) acc then acc else typ::acc) typs [] in
      begin
        match typs' with
        | [typ'] -> typ'
        | _ -> Inter(typ, typs')
      end
  | Union(typ, typs) ->
      let typs = List.map contract typs in
      let typs' = List.fold_right (fun typ acc -> if List.exists (equiv typ) acc then acc else typ::acc) typs [] in
      begin
        match typs' with
        | [typ'] -> typ'
        | _ -> Union(typ, typs')
      end
  | ExtArg(x,typ1,typ2) -> ExtArg(x, contract typ1, contract typ2)
  | List(x,p_len,y,p_i,typ) -> List(x, p_len, y, p_i, contract typ)
  | Exn(typ1,typ2) -> Exn(contract typ1, contract typ2)

let rec split_inter typ =
  match typ with
  | Base(base, x, p) ->
      let rec decomp t =
        match t.Syntax.desc with
        | Syntax.BinOp(Syntax.And, t1, t2) -> decomp t1 @ decomp t2
        | _ -> [t]
      in
      let ps = decomp p in
      List.map (fun p' -> Base(base, x, p')) ps
  | Fun(x,typ1,typ2) ->
      let typs2 = split_inter typ2 in
      List.map (fun typ2' -> Fun(x,typ1,typ2')) typs2
  | Tuple xtyps ->
      let make i typ =
        let xtyps' = List.mapi (fun j (x,typ') -> x, if i = j then typ else top @@ to_simple typ') xtyps in
        Tuple xtyps'
      in
      List.flatten_mapi (fun i (x,typ) -> List.map (make i) @@ split_inter typ) xtyps
  | Inter(styp, typs) ->
      [Inter(styp, List.flatten_map split_inter typs)]
  | _ -> [typ]
