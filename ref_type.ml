open Util

module S = Syntax
module U = Term_util

type base =
  | Unit
  | Bool
  | Int
  | Abst of string

type t =
  | Base of base * S.id * S.typed_term
  | Fun of S.id * t * t
  | Tuple of (S.id * t) list
  | Inter of t list
  | Union of t list
  | ExtArg of S.id * t * t
  | List of S.id * S.typed_term * S.id * S.typed_term * t

let _ExtArg x typ1 typ2 = ExtArg(x, typ1, typ2)

let is_fun_typ = function
  | Fun(_,_,_) -> true
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
  | Inter typs
  | Union typs -> List.exists (occur x) typs
  | ExtArg(_,typ1,typ2) -> occur x typ1 || occur x typ2
  | List(_,p_len,_,p_i,typ) ->
      let aux p =  List.exists (Id.same x) @@ U.get_fv p in
      aux p_len || aux p_i || occur x typ

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
  | Base(base,x,p) ->
      Format.fprintf fm "{%a:%a | %a}" Id.print x print_base base Print.term p
  | Fun(x, typ1, typ2) ->
      if occur x typ2
      then Format.fprintf fm "(@[<hov 4>%a:%a@ ->@ %a@])" Id.print x print typ1 print typ2
      else Format.fprintf fm "(@[<hov 4>%a@ ->@ %a@])" print typ1 print typ2
  | Tuple xtyps ->
      let n = List.length xtyps in
      let pr fm (i,(x,typ)) =
        if i < n-1 && occur x @@ Tuple xtyps
        then Format.fprintf fm "%a:" Id.print x;
        print fm typ
      in
      Format.fprintf fm "(@[%a@])" (print_list pr " *@ ") @@ List.mapi Pair.pair xtyps
  | Inter [] -> Format.fprintf fm "Top"
  | Inter [typ] -> print fm typ
  | Inter typs -> Format.fprintf fm "(@[%a@])" (print_list print " /\\@ ") typs
  | Union [] -> Format.fprintf fm "Bottom"
  | Union [typ] -> print fm typ
  | Union typs -> Format.fprintf fm "(@[%a@])" (print_list print " \\/@ ") typs
  | ExtArg(x,typ1,typ2) ->
      Format.fprintf fm "(@[%a where %a:%a@])" print typ2 Id.print x print typ1
  | List(x,p_len,y,p_i,typ2) ->
      Format.fprintf fm "(@[";
      if occur y typ2
      then
        if p_i = U.true_term
        then Format.fprintf fm "[%a]@ %a" Id.print y print typ2
        else Format.fprintf fm "[%a:%a]@ %a" Id.print y Print.term p_i print typ2
      else
        if p_i = U.true_term
        then Format.fprintf fm "%a" print typ2
        else Format.fprintf fm "[%a: %a]@ %a" Id.print y Print.term p_i print typ2;
      if p_len <> U.true_term
      then Format.fprintf fm " |%a: %a|" Id.print x Print.term p_len
      else
        if List.exists (Id.same x) (U.get_fv p_i) || occur x typ2
        then Format.fprintf fm " |%a|" Id.print x;
      Format.fprintf fm "list@])"

let rec decomp_fun n typ =
  match typ with
  | Base _
  | Tuple _
  | Inter _
  | Union _
  | List _ -> assert (n=0); [], [], typ
  | Fun _ when n <= 0 ->
      [], [], typ
  | Fun(x,typ1,typ2) ->
      let exts,typs,typ' = decomp_fun (n-1) typ2 in
      exts, (x,typ1)::typs, typ'
  | ExtArg(x,typ1,typ2) ->
      let exts,typs,typ' = decomp_fun n typ2 in
      (x,typ1)::exts, typs, typ'

let rec arg_num = function
  | Base _ -> 0
  | Tuple _ -> 0
  | Inter [] -> assert false
  | Inter (typ::_) -> arg_num typ
  | Union [] -> assert false
  | Union (typ::_) -> arg_num typ
  | Fun(_,_,typ2) -> 1 + arg_num typ2
  | ExtArg(_,_,typ2) -> arg_num typ2
  | List _ -> 0

let rec subst x t typ =
  match typ with
  | Base(base,y,p) -> Base(base, y, U.subst x t p)
  | Fun(y,typ1,typ2) -> Fun(y, subst x t typ1, subst x t typ2)
  | Tuple xtyps -> Tuple (List.map (fun (y,typ) -> y, subst x t typ) xtyps)
  | Inter typs -> Inter (List.map (subst x t) typs)
  | Union typs -> Union (List.map (subst x t) typs)
  | ExtArg(y,typ1,typ2) -> ExtArg(y, subst x t typ1, subst x t typ2)
  | List(y,p_len,z,p_i,typ) ->
      List(y, U.subst x t p_len, z, U.subst x t p_i, subst x t typ)

let subst_var x y t = subst x (U.make_var y) t

let rec rename var = function
  | Base(base, x, p) ->
      let x' = Option.default (Id.new_var_id x) var in
      Base(base, x', U.subst_var x x' p)
  | Fun(x, typ1, (Fun(_, typ, _) as typ2)) when !Flag.web && is_fun_typ typ ->
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
  | Inter typs -> Inter (List.map (rename var) typs)
  | Union typs -> Union (List.map (rename var) typs)
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


let to_abst_typ_base b =
  match b with
  | Unit -> Type.TUnit
  | Bool -> Type.TBool
  | Int -> Type.TInt
  | Abst _ -> unsupported "to_abst_typ_base"

let rec to_abst_typ typ =
  match typ with
  | Base(b, x, t) ->
      let x' = Id.set_typ x @@ to_abst_typ_base b in
      Type.TPred(x', Term_util.decomp_bexp t)
  | Fun(x,typ1,typ2) ->
      let x' = Id.set_typ x @@ to_abst_typ typ1 in
      let typ2' = to_abst_typ typ2 in
      Type.TFun(x', typ2')
  | Tuple xtyps ->
      Type.TTuple (List.map (fun (x,typ) -> Id.set_typ x @@ to_abst_typ typ) xtyps)
  | Inter typs
  | Union typs -> List.fold_right (Term_util.merge_typ -| to_abst_typ) typs Type.typ_unknown
  | ExtArg _ -> unsupported "to_abst_typ"
  | List _ -> unsupported "to_abst_typ"

let rec set_base_var x = function
  | Base(base, y, p) -> Base(base, x, U.subst_var y x p)
  | Inter typs -> Inter (List.map (set_base_var x) typs)
  | Union typs -> Union (List.map (set_base_var x) typs)
  | typ -> typ
let rec copy_fun_arg_to_base = function
  | Base(base, x, p) -> Base(base, x, p)
  | Fun(x,typ1,typ2) -> Fun(x, set_base_var x @@ copy_fun_arg_to_base typ1, copy_fun_arg_to_base typ2)
  | Tuple xtyps -> Tuple (List.map (Pair.map_snd copy_fun_arg_to_base) xtyps)
  | Inter typs -> Inter (List.map copy_fun_arg_to_base typs)
  | Union typs -> Union (List.map copy_fun_arg_to_base typs)
  | ExtArg(x,typ1,typ2) -> ExtArg(x, copy_fun_arg_to_base typ1, copy_fun_arg_to_base typ2)
  | List(x,p_len,y,p_i,typ) -> List(x, p_len, y, p_i, copy_fun_arg_to_base typ)

let rec to_simple typ =
  match typ with
  | Base(Unit, _, _) -> Type.TUnit
  | Base(Bool, _, _) -> Type.TBool
  | Base(Int, _, _) -> Type.TInt
  | Base(Abst _, _, _) -> assert false
  | Fun(x,typ1,typ2) -> Type.TFun(Id.new_var @@ to_simple typ1, to_simple typ2)
  | Tuple xtyps -> Type.TTuple (List.map (Id.new_var -| to_simple -| snd) xtyps)
  | Inter [] -> assert false
  | Inter (typ::_) -> to_simple typ
  | Union [] -> assert false
  | Union (typ::_) -> to_simple typ
  | ExtArg _ -> assert false
  | List(_,_,_,_,typ) -> Type.TList (to_simple typ)


let rec generate_check x typ =
  match typ with
  | Base(base, y, p) ->
      U.subst_var y x p
  | Fun(y,typ1,typ2) ->
      let t_typ1 = generate typ1 in
      let z = Id.new_var t_typ1.Syntax.typ in
      let t_typ2 = U.make_app (U.make_var x) [U.make_var z] in
      let r = Id.new_var ~name:"r" t_typ2.Syntax.typ in
      let typ2' = subst_var y z typ2 in
      U.make_lets [z,[],t_typ1; r,[],t_typ2] @@ generate_check r typ2'
  | Tuple [y,typ1;_,typ2] ->
      let t1 = U.make_fst @@ U.make_var x in
      let t2 = U.make_snd @@ U.make_var x in
      let x1 = U.var_of_term t1 in
      let x2 = U.var_of_term t2 in
      let typ2' = subst_var y x1 typ2 in
      let t = U.make_and (generate_check x1 typ1) (generate_check x2 typ2') in
      U.make_lets [x1,[],t1; x2,[],t2] t
  | _ -> assert false

and generate_simple_aux typ =
  match typ with
  | Type.TInt -> U.randint_unit_term
  | Type.TBool -> U.randbool_unit_term
  | Type.TUnit -> U.unit_term
  | Type.TFun(x,typ') ->
      let x' = Id.new_var_id x in
      U.make_fun x' @@ generate_simple typ'
  | Type.TTuple xs ->
      U.make_tuple @@ List.map (generate_simple -| Id.typ) xs
  | _ -> unsupported "Ref_type.generate_simple"

and generate_simple typ =
  U.make_br (U.make_fail typ) (generate_simple_aux typ)

and generate typ =
  match typ with
  | Base(Int, x, p) ->
      let x' = Id.new_var Type.TInt in
      U.make_let [x',[],U.randint_unit_term] @@ U.make_assume (generate_check x' typ) @@ U.make_var x'
  | Base(Bool, x, p) ->
      let x' = Id.new_var Type.TBool in
      U.make_let [x',[],U.randbool_unit_term] @@ U.make_assume (generate_check x' typ) @@ U.make_var x'
  | Base(Unit, x, p) ->
      U.make_assume (generate_check x typ) @@ U.unit_term
  | Base(_, _, _) -> unsupported "Ref_type.generate: Base"
  | Fun(x,typ1,typ2) ->
      let x' = Id.new_var @@ to_simple typ1 in
      let typ2' = subst_var x x' typ2 in
      let t1 = U.make_or U.randbool_unit_term @@ generate_check x' typ1 in
      let t2 = generate typ2' in
      let t3 = generate_simple @@ to_simple typ2' in
      U.make_fun x' @@ U.make_if t1 t2 t3
  | Tuple [x,typ1;_,typ2] ->
      let x' = Id.new_var @@ to_simple typ1 in
      let typ2' = subst_var x x' typ2 in
      let t1 = generate typ1 in
      let t2 = generate typ2' in
      U.make_let [x',[],t1] @@ U.make_tuple [U.make_var x'; t2]
  | Tuple xtyps -> unsupported "Ref_type.generate: Tuple"
  | Inter typs -> unsupported "Ref_type.generate: Inter"
  | Union typs -> unsupported "Ref_type.generate: Union"
  | ExtArg(x,typ1,typ2) -> unsupported "Ref_type.generate: ExtArg"
  | List(_,_,_,_,typ') ->
      let open Type in
      let open Term_util in
      let styp = to_simple typ in
      let u = Id.new_var ~name:"u" TUnit in
      let f = Id.new_var ~name:("make_" ^ to_id_string styp) (TFun(u,styp)) in
      let t_nil = make_nil2 styp in
      let t_cons = make_cons (generate typ') @@ make_app (make_var f) [unit_term] in
      let t_body = make_if randbool_unit_term t_nil t_cons in
      make_letrec [f,[u],t_body] @@ make_app (make_var f) [unit_term]


let to_abst_typ_base b =
  match b with
  | Unit -> Type.TUnit
  | Bool -> Type.TBool
  | Int -> Type.TInt
  | Abst _ -> unsupported "to_abst_typ_base"

let rec to_abst_typ typ =
  match typ with
  | Base(b, x, t) ->
      let x' = Id.set_typ x @@ to_abst_typ_base b in
      Type.TPred(x', Term_util.decomp_bexp t)
  | Fun(x,typ1,typ2) ->
      let x' = Id.set_typ x @@ to_abst_typ typ1 in
      let typ2' = to_abst_typ typ2 in
      Type.TFun(x', typ2')
  | Tuple xtyps ->
      Type.TTuple (List.map (fun (x,typ) -> Id.set_typ x @@ to_abst_typ typ) xtyps)
  | Inter typs
  | Union typs -> List.fold_right (Term_util.merge_typ -| to_abst_typ) typs Type.typ_unknown
  | ExtArg _ -> unsupported "to_abst_typ"
  | List _ -> unsupported "to_abst_typ"
