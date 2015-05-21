open Syntax
open Term_util
open Type
open Util
open Rose_tree

module RT = Ref_type

let debug () = List.mem "Curry" !Flag.debug_module

let rec element_num typ =
  match elim_tpred typ with
  | TTuple xs -> List.fold_right ((+) -| element_num -| Id.typ) xs 0
  | _ -> 1

let rec get_tuple_name_map rtyp =
  match rtyp with
  | RT.Tuple xrtyps ->
      let ixrtyps = List.mapi Pair.pair xrtyps in
      let ixs = List.filter_map (function (i,(_,RT.Base(_,x,_))) -> Some(i,x) | _ -> None) ixrtyps in
      let map = List.rev_flatten_map (fun (i,(_,typ)) -> List.map (Pair.map_snd (List.cons i)) @@ get_tuple_name_map typ) ixrtyps in
      List.map (fun (i,x) -> x,[i]) ixs @@@ map
  | RT.Inter rtyps
  | RT.Union rtyps -> List.rev_flatten_map get_tuple_name_map rtyps
  | _ -> []

let rec correct_arg_refer rtyp =
  match rtyp with
  | RT.Base _ -> rtyp
  | RT.Fun(x,rtyp1,rtyp2) ->
      let rtyp1' = correct_arg_refer rtyp1 in
      let rtyp2' = correct_arg_refer rtyp2 in
      let map = get_tuple_name_map rtyp1 in
      let aux (y,path) typ =
        let t = List.fold_left (Fun.flip make_proj) (make_var x) path in
        Ref_type.subst y t typ
      in
      RT.Fun(x, rtyp1', List.fold_right aux map rtyp2')
  | RT.Tuple xrtyps ->
      let aux (x,rtyp) xrtyps =
        let rtyp' = correct_arg_refer rtyp in
        let map = get_tuple_name_map rtyp in
        let aux' (y,path) typ =
          let t = List.fold_left (Fun.flip make_proj) (make_var x) path in
          Ref_type.subst y t typ
        in
        let xrtyps' = List.map (Pair.map_snd @@ List.fold_right aux' map) xrtyps in
        (x, rtyp') :: xrtyps'
      in
      RT.Tuple (List.fold_right aux xrtyps [])
  | RT.Inter rtyps -> RT.Inter (List.map correct_arg_refer rtyps)
  | RT.Union rtyps -> RT.Union (List.map correct_arg_refer rtyps)
  | RT.ExtArg(x,rtyp1,rtyp2) -> assert false
  | RT.List _ -> assert false

let rec uncurry_typ rtyp typ =
  if debug()
  then Format.printf "rtyp:%a@.typ:%a@.@." RT.print rtyp Print.typ typ;
  match rtyp,typ with
  | RT.Inter rtyps, _ -> RT.Inter (List.map (uncurry_typ -$- typ) rtyps)
  | RT.Union rtyps, _ -> RT.Union (List.map (uncurry_typ -$- typ) rtyps)
  | _, TFun(x,typ2) ->
      let typ1 = Id.typ x in
      let n = element_num typ1 in
      let exts,xrtyps,rtyp2 = RT.decomp_fun n rtyp in
      let rtyp1' = uncurry_typ_arg (List.map snd xrtyps) typ1 in
      let rtyp2' = uncurry_typ rtyp2 typ2 in
      let y =
          match rtyp with
          | RT.Fun(y, _, _) -> y
          | _ -> assert false
      in
      let rtyp = RT.Fun(y, rtyp1', rtyp2') in
      List.fold_right (Fun.uncurry RT._ExtArg) exts rtyp
  | _ -> rtyp

and uncurry_typ_arg rtyps typ =
  if debug()
  then Format.printf "rtyps:%a@.typ:%a@.@." (print_list RT.print ";" ~last:true) rtyps Print.typ typ;
  match rtyps, elim_tpred typ with
  | _, TTuple xs ->
      let aux (rtyps,xrtyps) {Id.typ} =
        let rtyps1,rtyps2 = List.split_nth (element_num typ) rtyps in
        let rtyp = uncurry_typ_arg rtyps1 typ in
        let x =
          match rtyp with
          | RT.Base(_,x,_) -> x
          | _ -> Id.new_var typ_unknown
        in
        rtyps2, xrtyps @ [x, rtyp]
      in
      let rtyps',xrtyps = List.fold_left aux (rtyps,[]) xs in
      assert (rtyps' = []);
      RT.Tuple xrtyps
  | [rtyp], _ -> uncurry_typ rtyp typ
  | _ -> assert false

let uncurry_rtyp t f rtyp =
  let typ = Trans.assoc_typ f t in
  let rtyp' = correct_arg_refer @@ uncurry_typ (RT.copy_fun_arg_to_base rtyp) typ in
  if debug()
  then Format.printf "%a:@.rtyp:%a@.typ:%a@.===> %a@.@." Id.print f RT.print rtyp Print.typ typ RT.print rtyp';
  if Flag.print_ref_typ_debug
  then Format.printf "UNCURRY: %a: @[@[%a@]@ ==>@ @[%a@]@]@." Id.print f RT.print rtyp RT.print rtyp';
  rtyp'

let rec remove_pair_typ = function
  | TUnit -> Leaf TUnit
  | TBool -> Leaf TBool
  | TAbsBool -> Leaf TAbsBool
  | TInt -> Leaf TInt
  | TRInt p -> Leaf (TRInt p)
  | TVar _ -> assert false
  | TFun _ as typ ->
      let xs,typ' = decomp_tfun typ in
      let xs' = List.flatten_map (fun y -> flatten (remove_pair_var y)) xs in
      Leaf (List.fold_right (fun x typ -> TFun(x,typ)) xs' typ')
  | TTuple xs -> Node (List.map (remove_pair_typ -| Id.typ) xs)
  | TList typ -> Leaf (TList (root (remove_pair_typ typ)))
  | TData(s,b) -> Leaf (TData(s,b))
  | TPred({Id.typ=TTuple[x; {Id.typ=typ}]} as y, ps) ->
      begin
        match typ with (* Function types cannot have predicates *)
        | TFun _ ->
            let x1 = Id.new_var ~name:(Id.name x) (elim_tpred @@ Id.typ x) in
            let x2 = Id.new_var ~name:"f" typ in
            let ps' = List.map (subst y @@ make_pair (make_var x1) (make_var x2)) ps in
            let x' = Id.set_typ x (TPred(x1,ps')) in
            remove_pair_typ @@ TTuple [x'; Id.new_var typ]
        | _ ->
            let y' = Id.set_typ y typ in
            let ps' = List.map (subst y @@ make_pair (make_var x) (make_var y')) ps in
            let typ' = TPred(y', ps') in
            remove_pair_typ @@ TTuple [x; Id.new_var typ']
      end
  | TPred({Id.typ=TTuple _}, ps) ->
      unsupported "Not implemented: remove_pair_typ"
  | TPred(x,ps) ->
      let ps' = List.map remove_pair ps in
      let typ' =
        match remove_pair_typ (Id.typ x) with
        | Leaf typ -> typ
        | Node _ -> raise (Fatal "Not implemented CPS.remove_pair_typ(TPred)")
      in
      Leaf (TPred(Id.set_typ x typ', ps'))
  | typ -> Format.printf "remove_pair_typ: %a@." Print.typ typ; assert false

and remove_pair_var x =
  let to_string path = List.fold_left (fun acc i -> acc ^ string_of_int i) "" path in
  let aux path typ = Id.set_typ (Id.add_name_after x @@ to_string path) typ in
  map aux @@ remove_pair_typ (Id.typ x)

and remove_pair_aux t typ_opt =
  let typ = match typ_opt with None -> t.typ | Some typ -> typ in
  let typs = remove_pair_typ typ in
  match t.desc with
  | Const _
  | Event _ -> Leaf t
  | Bottom -> map (Fun.const make_bottom) typs
  | Var x -> map (Fun.const make_var) (remove_pair_var x)
  | Fun(x, t) ->
      let xs = flatten @@ remove_pair_var x in
      let t' = root @@ remove_pair_aux t None in
      Leaf (List.fold_right make_fun xs t')
  | App(t1, ts) ->
      let typs = get_argtyps t1.typ in
      assert (List.length typs >= List.length ts);
      let typs' = List.take (List.length ts) typs in
      let t' = root (remove_pair_aux t1 None) in
      let ts' = List.flatten (List.map2 (fun t typ -> flatten (remove_pair_aux t @@ Some typ)) ts typs') in
      Leaf (make_app t' ts')
  | If(t1, t2, t3) ->
      let t1' = root @@ remove_pair_aux t1 None in
      let t2' = root @@ remove_pair_aux t2 None in
      let t3' = root @@ remove_pair_aux t3 None in
      Leaf (make_if t1' t2' t3')
  | Let(flag, bindings, t) ->
      let aux (f,xs,t) =
        let f' = root @@ remove_pair_var f in
        let xs' = List.flatten_map (flatten -| remove_pair_var) xs in
        let t' = root @@ remove_pair_aux t None in
        f', xs', t'
      in
      let bindings' = List.map aux bindings in
      let t' = root @@ remove_pair_aux t None in
      Leaf (make_let_f flag bindings' t')
  | BinOp(op, t1, t2) ->
      begin
        match op, elim_tpred t1.typ with
        | (Eq | Lt | Gt | Leq | Geq), (TUnit | TBool | TInt | TData(_,false)) -> ()
        | (Eq | Lt | Gt | Leq | Geq), _ ->
            Format.printf "%a@." Print.typ t1.typ;
            Format.printf "%a@." Print.typ t2.typ;
            Format.printf "%a@." Print.term' t;
            unsupported "polymorphic comparison"
        | _ -> ()
      end;
      let t1' = root @@ remove_pair_aux t1 None in
      let t2' = root @@ remove_pair_aux t2 None in
      Leaf {desc=BinOp(op, t1', t2'); typ=root typs; attr=[]}
  | Not t1 ->
      let t1' = root @@ remove_pair_aux t1 None in
      Leaf (make_not t1')
  | Record fields -> assert false
  | Field(s,t1) -> assert false
  | SetField(s,t1,t2) -> assert false
  | Nil -> assert false
  | Cons(t1,t2) -> assert false
  | Constr(s,ts) -> assert false
  | Match(t1,pats) -> assert false
  | TryWith(t1,t2) -> assert false
  | Tuple ts -> Node (List.map (remove_pair_aux -$- None) ts)
  | Proj(i, {desc=Var x}) when x = abst_var -> Leaf (make_var x) (* for predicates *)
  | Proj(i,t) ->
      begin
        match remove_pair_aux t None with
        | Leaf _ -> Format.printf "%a@." Print.term t; assert false
        | Node ts -> List.nth ts i
      end
  | _ ->
      Format.printf "%a@." Print.term t;
      assert false

and remove_pair t = {(root (remove_pair_aux t None)) with attr=t.attr}



let remove_pair t =
  assert (List.mem ACPS t.attr);
  let t' = remove_pair t in
  Type_check.check t' typ_result;
  t', uncurry_rtyp t
