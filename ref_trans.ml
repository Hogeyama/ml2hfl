open Util
open Type
open Syntax
open Term_util


let trans = make_trans2 ()

let rec root x bb path_rev =
  let aux = function
      y, {desc=Fst{desc=Var z}} when Id.same x y -> Some (z,1)
    | y, {desc=Snd{desc=Var z}} when Id.same x y -> Some (z,2)
    | _ -> None
  in
  try
    let y,dir = get_opt_val @@ List.find ((<>) None) @@ List.map aux bb in
    root y bb (dir::path_rev)
  with Not_found -> x, List.rev path_rev
let root x bb = root x bb []

let rec find_fst x bb =
  match bb with
    [] -> None
  | (y,{desc=Fst{desc=Var z}})::bb' when Id.same x z -> Some y
  | _::bb' -> find_fst x bb'

let rec find_snd x bb =
  match bb with
    [] -> None
  | (y,{desc=Snd{desc=Var z}})::bb' when Id.same x z -> Some y
  | _::bb' -> find_snd x bb'

let rec find_app x bb =
  match bb with
    [] -> []
  | (_,{desc=App({desc=Var y},[t])})::bb' when Id.same x y -> t::find_app x bb'
  | _::bb' -> find_app x bb'


let rec make_tree x bb =
  match find_fst x bb, find_snd x bb, find_app x bb with
    Some lhs, Some rhs, _ -> Tree.Node(make_tree lhs bb, make_tree rhs bb)
  | None, None, args ->
      let typ =
        match Id.typ x with
          TFun(x,_) -> Some (Id.typ x)
        | _ -> None
      in
      Tree.Leaf(typ, args)
  | _ -> assert false

let rec make_trees tree =
  match tree with
    Tree.Leaf(None, []) -> assert false
  | Tree.Leaf(None, _) -> assert false
  | Tree.Leaf(Some typ, []) -> [Tree.Leaf (make_none typ)]
  | Tree.Leaf(Some _, args) -> List.map (fun t -> Tree.Leaf t) args
  | Tree.Node(lhs,rhs) ->
      let trees1 = make_trees lhs in
      let trees2 = make_trees rhs in
      flatten_map (fun t1 -> List.map (fun t2 -> Tree.Node(t1, t2)) trees2) trees1

let rec term_of_tree tree =
  match tree with
    Tree.Leaf t -> t
  | Tree.Node(t1,t2) -> make_pair (term_of_tree t1) (term_of_tree t2)

(*
let rec make_args tree =
  match tree with
    Tree.Leaf(None, []) -> assert false
  | Tree.Leaf(None, _) -> assert false
  | Tree.Leaf(Some typ, []) -> [make_bottom typ]
  | Tree.Leaf(Some _, args) -> args
  | Tree.Node(lhs,rhs) ->
      let trees1 = make_args lhs in
      let trees2 = make_args rhs in
      flatten_map (fun t1 -> List.map (fun t2 -> make_pair t1 t2) trees2) trees1
*)

let rec proj_of_path path t =
  match path with
    [] -> t
  | 1::[] -> make_get_val @@ make_fst t
  | 1::path' -> assert false; make_fst @@ proj_of_path path' t
  | 2::path' -> make_snd @@ proj_of_path path' t
  | _::path' -> assert false

let make_some' t =
  if is_none t
  then t
  else make_some t

let rec make_app' t typ arg =
Format.printf "   arg: %a@." pp_print_term arg;
  match typ with
    TInt -> t
  | TFun _ -> print_begin_end "BEGIN\n" (lazy (make_app t [arg])) "END\n"
  | TPair({Id.typ=TInt}, typ) -> make_app' t typ @@ make_snd arg
  | TPair({Id.typ=TFun _}, typ) ->
      let t = print_begin_end "BEGIN\n" (lazy ( make_app t [make_some' @@ make_fst arg])) "END\n" in
      let p = Id.new_var "r" t.typ in
      let pt = make_var p in
      make_let [p, [], t] @@
        make_pair (make_fst pt) (make_app' (make_snd pt) typ (make_snd arg))
  | _ -> assert false

let rec same_arg path_rev t1 t2 =
  match t1,t2 with
    Tree.Leaf t1', Tree.Leaf t2' when t1' = t2' -> List.rev path_rev
  | Tree.Leaf t1', Tree.Leaf t2' -> []
  | Tree.Node(t11,t12), Tree.Node(t21,t22) -> same_arg (1::path_rev) t11 t21 @ same_arg (2::path_rev) t12 t22
  | _ -> assert false
let same_arg t1 t2 = same_arg [] t1 t2

let inst_var x tt bb t =
  match Id.typ x with
    TFun(y,_) ->
      let y' = Id.new_var_id y in
      Format.printf "x: %a, y': %a@." Id.print x Id.print y';
      let r,path = root x bb in
      let r' = trans.tr2_var (tt,bb) r in
      let tree = make_tree r bb in
      let tree' = Tree.update path (Tree.Leaf(Some (Id.typ y'), [make_var y'])) tree in
let pr _ (_,ts) =
  Format.printf "[%a]" (print_list pp_print_term' "; ") ts
in
Format.printf "TREE: %a@." (Tree.print pr) tree';
      let trees = make_trees tree' in
      let args = List.map term_of_tree trees in
Format.printf "r': %a:%a@." Id.print r' pp_print_typ (Id.typ r');
      let apps = List.map (make_app' (make_var r') (Id.typ r)) args in
(*
      Format.printf "TREE(%a --%a-- %a):%d@." Id.print r (print_list Format.pp_print_int "") path Id.print x @@ List.length apps;
      List.iter (Format.printf "  %a@." pp_print_term) apps;
      Format.printf "orig: %a@." pp_print_term t;
*)
      let same_arg_apps = (* negligence *)
        let rec aux i ts acc =
          match ts with
            [] -> assert false
          | [t] -> acc
          | t1::t2::ts ->
              let paths = same_arg t1 t2 in
              let paths' = List.map (fun path -> i,i+1,path) paths in
              aux (i+1) (t2::ts) (paths' @ acc)
        in
        aux 0 trees []
      in
      let xs = List.map (fun t -> Id.new_var "x" t.typ) apps in
Format.printf "root: %a, %a@." Id.print r pp_print_typ (Id.typ r);
Format.printf "hd: %a, %a@." Id.print (List.hd xs) pp_print_typ (Id.typ @@ List.hd xs);
      let t' = proj_of_path path @@ make_var @@ List.hd xs in
      let t'' =
        let aux t (i,j,path) =
          let t1 = make_var (List.nth xs i) in
          let t2 = make_var (List.nth xs j) in
          make_assume (make_eq t1 t2) t
        in
        List.fold_left aux t' same_arg_apps
      in
      let t''' = (*List.fold_left () t'' bs*)t'' in
      let t'''' = List.fold_left2 (fun t x app -> make_let [x,[],app] t) t''' xs apps in
      let x' = Id.new_var_id x in
      make_let [x',[y'],t''''] @@ subst x (make_var x') t
  | _ -> t (* negligence *)

let trans_typ (tt,bb) typ =
  match typ with
    TPair(x,typ2) ->
      let x' = trans.tr2_var (tt,bb) x in
      let typ2' = trans.tr2_typ (tt,bb) typ2 in
      begin match Id.typ x' with
        TFun(y,typ12) ->
          let y' = Id.set_typ y @@ opt_typ @@ Id.typ y in
          TFun(y', TPair(Id.new_var "r" @@ opt_typ typ12, typ2'))
      | _ -> TPair(x', typ2')
      end
  | _ -> trans.tr2_typ_rec (tt,bb) typ

let trans_desc (tt,bb) desc =
  match desc with
    Let(Nonrecursive, [x,[],({desc=App({desc=Var x1},_)} as t1)], t) ->
      let x1' = trans.tr2_var (tt,bb) x1 in
      let t1' = trans.tr2_term (tt,bb) t1 in
      let bb' = (x,t1)::bb in
Format.printf "B: ";
List.iter (fun (x,t) -> Format.printf "%a = %a; " Id.print x pp_print_term t) bb';
Format.printf "@.";
      let t' = trans.tr2_term (tt,bb') t in
      let t'' = inst_var x1 tt bb' @@ make_let [x,[],t1'] t' in
      t''.desc
  | Let(Nonrecursive, [x,[],({desc=Pair({desc=Var x1},t2)} as t1)], t) ->
      let x' =  trans.tr2_var (tt,bb) x in
      let x1' = trans.tr2_var (tt,bb) x1 in
      let t2' = trans.tr2_term (tt,bb) t2 in
      let bb' = (x,t1)::bb in
      let t' = trans.tr2_term (tt,bb') t in
      let t1' =
        match Id.typ x1 with
        | TFun _ ->
            let x11 = match Id.typ x1' with TFun(x11,_) -> x11 | _ -> assert false in
            let y = Id.new_var "x" @@ opt_typ @@ Id.typ x11 in
            let t1 = make_some @@ make_app (make_var x1') [make_get_val @@ make_var y] in
            let t1' = make_if (make_is_none (make_var y)) (make_none @@ get_opt_typ t1.typ) t1 in
            make_fun y (make_pair t1' t2')
        | _ -> make_pair (make_var x1') t2'
      in
      (make_let [x',[],t1'] t').desc
  | Let(Nonrecursive, [x,[],({desc=Fst{desc=Var x1}} as t1)], t) ->
      let bb' = (x,t1)::bb in
      (trans.tr2_term (tt,bb') t).desc
(*
  | Let(Nonrecursive, [x,[],({desc=Fst{desc=Var x1}} as t1)], t) ->
      let x1' = trans.tr2_var (tt,bb) x1 in
      let bb' = (x,t1)::bb in
      let t' = trans.tr2_term (tt,bb') t in
      let t1' =
        match t1.typ with
        | TFun _ ->
            let typ = match Id.typ x1' with TFun(y,_) -> get_opt_typ @@ Id.typ y | _ -> assert false in
            let z = Id.new_var "x" typ in
            make_fun z @@ make_get_val @@ make_fst @@ make_app (make_var x1') [make_some @@ make_var z]
        | _ ->
            make_fst @@ make_var x1'
      in
      (make_let [x,[],t1'] t').desc
*)
  | Let(Nonrecursive, [x,[],({desc=Snd{desc=Var x1}} as t1)], t) ->
      let bb' = (x,t1)::bb in
      (trans.tr2_term (tt,bb') t).desc
(*
  | Let(Nonrecursive, [x,[],({desc=Snd{desc=Var x1}} as t1)], t) ->
      let x1' = trans.tr2_var (tt,bb) x1 in
      let bb' = (x,t1)::bb in
      let t' = trans.tr2_term (tt,bb') t in
      let t1' =
        match fst_typ @@ Id.typ x1 with
        | TFun _ ->
            let typ = match Id.typ x1' with TFun(y,_) -> get_opt_typ @@ Id.typ y | _ -> assert false in
            make_snd @@ make_app (make_var x1') [make_none typ]
        | _ ->
            make_snd @@ make_var x1'
      in
      (make_let [x,[],t1'] t').desc
*)
  | _ -> trans.tr2_desc_rec (tt,bb) desc

let () = trans.tr2_desc <- trans_desc
let () = trans.tr2_typ <- trans_typ

let trans tt t = t
  |> Trans.inline_no_effect
  |> Trans.flatten_let
  |> do_and_return (Format.printf "BEFORE: %a@." pp_print_term)
  |> trans.tr2_term (tt,[])
