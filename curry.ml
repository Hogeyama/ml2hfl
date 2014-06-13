open Syntax
open Term_util
open Type
open Util
open Tree

module RT = Ref_type

let debug = false

let rec element_num typ =
  match elim_tpred typ with
  | TPair(x,typ) -> element_num (Id.typ x) + element_num typ
  | _ -> 1

let rec uncurry_typ rtyp typ =
  if debug then Format.printf "rtyp:%a@.typ:%a@.@."
                              RT.print rtyp print_typ typ;
  match rtyp,typ with
  | RT.Inter rtyps, _ ->
      RT.Inter (List.map (fun rtyp -> uncurry_typ rtyp typ) rtyps)
  | _, TFun(x,typ2) ->
      let typ1 = Id.typ x in
      let n = element_num typ1 in
      let exts,xrtyps,rtyp2 = RT.decomp_fun n rtyp in
      let rtyps = List.map snd xrtyps in
      let map,rtyp1' = uncurry_typ_arg rtyps typ1 in
      let rtyp2' = uncurry_typ rtyp2 typ2 in
      let aux (x,typ1) typ2 = RT.ExtArg(x,typ1,typ2) in
      let x = Id.new_var "x" typ_unknown in
      let rtyp2'' = List.fold_left (fun typ (x',f) -> RT.subst x' (f x) typ) rtyp2' map in
      List.fold_right aux exts (RT.Fun(x, rtyp1', rtyp2''))
  | _ -> rtyp

and get_arg_var = function
  | RT.Base(_,x,_) -> x
  | _ -> Id.new_var "x" typ_unknown

and uncurry_typ_arg rtyps typ =
  if debug then
    Format.printf "rtyps:%a@.typ:%a@.@." (print_list RT.print ";" ~last:true) rtyps print_typ typ;
  match rtyps, elim_tpred typ with
  | _, TPair(x,typ) ->
      let rtyps1,rtyps2 = List.split_nth (element_num @@ Id.typ x) rtyps in
      let map1,rtyp1 = uncurry_typ_arg rtyps1 (Id.typ x) in
      let map2,rtyp2 = uncurry_typ_arg rtyps2 typ in
      let map1' = List.map (fun (x,f) -> x, fun x' -> make_fst (f x')) map1 in
      let map2' = List.map (fun (x,f) -> x, fun x' -> make_snd (f x')) map1 in
      map1'@@@map2', RT.Pair(get_arg_var rtyp1, rtyp1, rtyp2)
  | [RT.Base(base,x,p) as rtyp], _ -> [x, fun x' -> make_var x'], uncurry_typ rtyp typ
  | [rtyp], _ -> [], uncurry_typ rtyp typ
  | _ -> assert false

let uncurry_rtyp t f rtyp =
  let typ = Trans.assoc_typ f t in
  let rtyp' = uncurry_typ rtyp typ in
    if debug then Format.printf "%a:@.rtyp:%a@.typ:%a@.===> %a@.@."
      Id.print f RT.print rtyp print_typ typ RT.print rtyp';
  if !Flag.print_ref_typ
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
  | TPair(x,typ) -> Node(remove_pair_typ (Id.typ x), remove_pair_typ typ)
  | TList typ -> Leaf (TList (root (remove_pair_typ typ)))
  | TConstr(s,b) -> Leaf (TConstr(s,b))
  | TPred({Id.typ=TPair(x, typ)} as y, ps) ->
      begin
        match typ with (* Function types cannot have predicates *)
          TFun _ ->
          let x1 = Id.new_var (Id.name x) (elim_tpred (Id.typ x)) in
          let x2 = Id.new_var "f" typ in
          let ps' = List.map (subst y (make_pair (make_var x1) (make_var x2))) ps in
          let x' = Id.set_typ x (TPred(x1,ps')) in
          remove_pair_typ (TPair(x', typ))
        | _ ->
            let y' = Id.set_typ y typ in
            let ps' = List.map (subst y (make_pair (make_var x) (make_var y'))) ps in
            let typ' = TPred(y', ps') in
            remove_pair_typ (TPair(x, typ'))
      end
  | TPred(x,ps) ->
      let ps' = List.map remove_pair ps in
      let typ' =
        match remove_pair_typ (Id.typ x) with
        | Leaf typ -> typ
        | Node _ -> raise (Fatal "Not implemented CPS.remove_pair_typ(TPred)")
      in
      Leaf (TPred(Id.set_typ x typ', ps'))
  | typ -> Format.printf "remove_pair_typ: %a@." print_typ typ; assert false

and remove_pair_var x =
  let to_string path = List.fold_left (fun acc i -> acc ^ string_of_int i) "" path in
  let aux path typ = Id.set_typ (Id.add_name x (to_string path)) typ in
  map aux (remove_pair_typ (Id.typ x))

and remove_pair_aux t typ_opt =
  let typ = match typ_opt with None -> t.typ | Some typ -> typ in
  let typs = remove_pair_typ typ in
  match t.desc with
  | Const _
  | RandInt _
  | Event _
  | RandValue _ -> Leaf t
  | Bottom -> map (fun _ -> make_bottom) typs
  | Var x -> map (fun _ -> make_var) (remove_pair_var x)
  | Fun(x, t) ->
      let xs = flatten (remove_pair_var x) in
      let t' = root (remove_pair_aux t None) in
      Leaf (List.fold_right make_fun xs t')
  | App(t1, ts) ->
      let typs = get_argtyps t1.typ in
      let () = assert (List.length typs >= List.length ts) in
      let typs' = List.take (List.length ts) typs in
      let t' = root (remove_pair_aux t1 None) in
      let ts' = List.flatten (List.map2 (fun t typ -> flatten (remove_pair_aux t (Some typ))) ts typs') in
      Leaf (make_app t' ts')
  | If(t1, t2, t3) ->
      let t1' = root (remove_pair_aux t1 None) in
      let t2' = root (remove_pair_aux t2 None) in
      let t3' = root (remove_pair_aux t3 None) in
      Leaf (make_if t1' t2' t3')
  | Branch(t1, t2) ->
      let t1' = root (remove_pair_aux t1 None) in
      let t2' = root (remove_pair_aux t2 None) in
      Leaf {desc=Branch(t1',t2'); typ=t1'.typ}
  | Let(flag, bindings, t) ->
      let aux (f,xs,t) =
        let f' = root (remove_pair_var f) in
        let xs' = List.flatten (List.map (fun x -> flatten (remove_pair_var x)) xs) in
        let t' = root (remove_pair_aux t None) in
        f', xs', t'
      in
      let bindings' = List.map aux bindings in
(*
Color.printf Color.Reverse "ROOT: ";
Color.printf Color.Cyan "%a@.@." pp_print_term' t;
 *)
      let t' = root (remove_pair_aux t None) in
      Leaf (make_let_f flag bindings' t')
  | BinOp(op, t1, t2) ->
      begin
        match op, elim_tpred t1.typ with
        | (Eq | Lt | Gt | Leq | Geq), (TUnit | TBool | TInt | TConstr(_,false)) -> ()
        | (Eq | Lt | Gt | Leq | Geq), _ ->
            Format.printf "%a@." print_typ t1.typ;
            Format.printf "%a@." print_term' t;
            raise (Fatal "Unsupported (polymorphic comparison)")
        | _ -> ()
      end;
      let t1' = root (remove_pair_aux t1 None) in
      let t2' = root (remove_pair_aux t2 None) in
      Leaf {desc=BinOp(op, t1', t2'); typ=root typs}
  | Not t1 ->
      let t1' = root (remove_pair_aux t1 None) in
      Leaf (make_not t1')
  | Record fields -> assert false
  | Proj(i,s,f,t1) -> assert false
  | SetField(n,i,s,f,t1,t2) -> assert false
  | Nil -> assert false
  | Cons(t1,t2) -> assert false
  | Constr(s,ts) -> assert false
  | Match(t1,pats) -> assert false
  | TryWith(t1,t2) -> assert false
  | Pair(t1,t2) -> Node(remove_pair_aux t1 None, remove_pair_aux t2 None)
  | Fst {desc=Var x} when x = abst_var -> Leaf (make_var x) (* for predicates *)
  | Fst t ->
      begin
        match remove_pair_aux t None with
        | Leaf _ -> Format.printf "%a@." print_term t; assert false
        | Node(t',_) -> t'
      end
  | Snd {desc=Var x} when x = abst_var -> Leaf (make_var x) (* for predicates *)
  | Snd t ->
      begin
        match remove_pair_aux t None with
        | Leaf _ -> assert false
        | Node(_,t') -> t'
      end
  | _ ->
      Format.printf "%a@." print_term t;
      assert false

and remove_pair t = root (remove_pair_aux t None)



let add_pred = make_trans ()

let add_pred_typ typ =
  match typ with
  | TFun(b1,TFun(b2,TFun(x,TFun(c1,TFun(c2,TFun(y,typ')))))) ->
      begin
        match Id.typ b1, Id.typ b2, Id.typ x, Id.typ c1, Id.typ c2, Id.typ y with
        | TBool, TBool, TInt, TBool, TBool, TInt ->
            let pred = make_eq (make_var x) (make_var y) in
            let y' = Id.set_typ y @@ TPred(y, [pred]) in
            TFun(b1,TFun(b2,TFun(x,TFun(c1,TFun(c2,TFun(y',typ'))))))
        | _ -> add_pred.tr_typ_rec typ
      end
  | _ -> add_pred.tr_typ_rec typ

let () = add_pred.tr_typ <- add_pred_typ

let add_pred = add_pred.tr_term



let remove_pair t =
  let t' = remove_pair t in
  Type_check.check t' typ_result;
  let t'' = add_pred t' in
  t'', uncurry_rtyp t
