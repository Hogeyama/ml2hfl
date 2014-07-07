open Util

module S = Syntax
module U = Term_util

type base =
    Unit
  | Bool
  | Int
  | Abst of string

type t =
    Base of base * S.id * S.typed_term
  | Fun of S.id * t * t
  | Tuple of (S.id * t) list
  | Inter of t list
  | Union of t list
  | ExtArg of S.id * t * t
  | List of S.id * S.typed_term * S.id * S.typed_term * t

let is_fun_typ = function
    Fun(_,_,_) ->
      true
  | _ ->
      false

let print_base fm = function
    Unit -> Format.pp_print_string fm "unit"
  | Bool -> Format.pp_print_string fm "bool"
  | Int -> Format.pp_print_string fm "int"
  | Abst s -> Format.pp_print_string fm s

let rec occur x = function
    Base(_,_,p) -> List.exists (Id.same x) (U.get_fv p)
  | Fun(_,typ1,typ2) -> occur x typ1 || occur x typ2
  | Tuple xtyps -> List.exists (fun (_,typ) -> occur x typ) xtyps
  | Inter typs
  | Union typs -> List.exists (occur x) typs
  | ExtArg(_,typ1,typ2) -> occur x typ1 || occur x typ2
  | List(_,p_len,_,p_i,typ) ->
      let aux p =  List.exists (Id.same x) (U.get_fv p) in
        aux p_len || aux p_i || occur x typ

let rec print fm = function
    Base(base,x,p) when p = U.true_term ->
      Format.fprintf fm "%a" print_base base
  | Base(Bool,x,p) when U.make_var x = p ->
      Format.fprintf fm "{true}"
  | Base(Bool,x,p) when U.make_not (U.make_var x) = p ->
      Format.fprintf fm "{false}"
  | Base(Int,x,{S.desc=S.BinOp(S.Eq, {S.desc=S.Var y}, {S.desc=S.Const(S.Int n)})})
  | Base(Int,x,{S.desc=S.BinOp(S.Eq, {S.desc=S.Const (S.Int n)}, {S.desc=S.Var y})}) when x = y ->
      Format.fprintf fm "{%d}" n
  | Base(base,x,p) ->
      Format.fprintf fm "{%a:%a | %a}" Id.print x print_base base S.print_term p
  | Fun(x, typ1, typ2) ->
      if occur x typ2
      then Format.fprintf fm "(@[<hov 4>%a:%a@ ->@ %a@])" Id.print x print typ1 print typ2
      else Format.fprintf fm "(@[<hov 4>%a@ ->@ %a@])" print typ1 print typ2
  | Tuple xtyps ->
      let pr fm (x,typ) =
        if occur x @@ Tuple xtyps
        then Format.fprintf fm "%a:" Id.print x;
        print fm typ
      in
      Format.fprintf fm "(@[%a@])" (print_list pr "@ *@ ") xtyps
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
        else Format.fprintf fm "[%a:%a]@ %a" Id.print y S.print_term p_i print typ2
      else
        if p_i = U.true_term
        then Format.fprintf fm "%a" print typ2
        else Format.fprintf fm "[%a:%a]@ %a" Id.print y S.print_term p_i print typ2;
      Format.fprintf fm " list";
      if p_len <> U.true_term
      then Format.fprintf fm "|%a:%a|" Id.print x S.print_term p_len
      else
        if List.exists (Id.same x) (U.get_fv p_i) || occur x typ2
        then Format.fprintf fm "|%a|" Id.print x;
      Format.fprintf fm "@])"

let rec decomp_fun n typ =
  match typ with
      Base _
    | Tuple _
    | Inter _
    | Union _
    | List _ -> assert (n=0); [], [], typ
    | Fun(x,typ1,typ2) ->
        if n <= 0
        then [], [], typ
        else
          let exts,typs,typ' = decomp_fun (n-1) typ2 in
            exts, (x,typ1)::typs, typ'
    | ExtArg(x,typ1,typ2) ->
        let exts,typs,typ' = decomp_fun n typ2 in
          (x,typ1)::exts, typs, typ'

let rec arg_num = function
    Base _ -> 0
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
      Base(base,y,p) -> Base(base, y, U.subst x t p)
    | Fun(y,typ1,typ2) -> Fun(y, subst x t typ1, subst x t typ2)
    | Tuple xtyps -> Tuple (List.map (fun (y,typ) -> y, subst x t typ) xtyps)
    | Inter typs -> Inter (List.map (subst x t) typs)
    | Union typs -> Union (List.map (subst x t) typs)
    | ExtArg(y,typ1,typ2) -> ExtArg(y, subst x t typ1, subst x t typ2)
    | List(y,p_len,z,p_i,typ) ->
        List(y, U.subst x t p_len, z, U.subst x t p_i, subst x t typ)

let rec rename var = function
  | Base(base, x, p) ->
      let x' = Option.default (Id.new_var_id x) var in
      Base(base, x', U.subst x (U.make_var x') p)
  | Fun(x, typ1, (Fun(_, typ, _) as typ2)) when !Flag.web && is_fun_typ typ ->
      let x' = Id.new_var ~name:("@" ^ Id.name x) (Id.typ x) in
      let typ2' = subst x (U.make_var x') typ2 in
      Fun(x', rename (Some x') typ1, rename None typ2')
  | Fun(x,typ1,typ2) ->
      let x' = Id.new_var_id x in
      let typ2' = subst x (U.make_var x') typ2 in
      Fun(x', rename (Some x') typ1, rename None typ2')
  | Tuple xtyps ->
      let aux (x,typ) =
        let x' = Id.new_var_id x in
        x', rename (Some x') @@ subst x (U.make_var x') typ
      in
      Tuple (List.map aux xtyps)
  | Inter typs -> Inter (List.map (rename var) typs)
  | Union typs -> Union (List.map (rename var) typs)
  | ExtArg(x,typ1,typ2) ->
      let x' = Id.new_var_id x in
      let typ2' = subst x (U.make_var x') typ2 in
      ExtArg(x', rename (Some x') typ1, rename None typ2')
  | List(x,p_len,y,p_i,typ) ->
      let x' = Id.new_var_id x in
      let y' = Id.new_var_id y in
      let p_len' = U.subst x (U.make_var x') p_len in
      let p_i' = U.subst y (U.make_var y') p_i in
      let typ' = subst x (U.make_var x') typ in
      let typ'' = subst y (U.make_var y') typ' in
      List(x', p_len', y', p_i', rename None typ'')

let rename typ =
  Id.save_counter ();
  Id.clear_counter ();
  let typ' = rename None typ in
  Id.reset_counter ();
  typ'
