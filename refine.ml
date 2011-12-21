
open Utilities
open CEGAR_syntax
open CEGAR_type
open CEGAR_util


exception CannotRefute
      

let add env ps p =
  if List.exists (Wrapper2.equiv env [] p) ps
  then ps
  else p::ps

let rec merge_typ env typ typ' =
    match typ,typ' with
        TBase(b1,ps1),TBase(b2,ps2) ->
          assert (b1 = b2);
          let x = new_id "x" in
          let env' = (x,typ)::env in
          let ps1' = ps1 (Var x) in
          let ps2' = ps2 (Var x) in
          let ps = List.fold_left (add env') ps1' ps2' in
          let ps t = List.map (subst x t) ps in
            TBase(b1, ps)
      | TFun(typ11,typ12), TFun(typ21,typ22) ->
          let x = new_id "x" in
          let typ12 = typ12 (Var x) in
          let typ22 = typ22 (Var x) in
          let env' = (x,typ11)::env in
          let typ1 = merge_typ env typ11 typ21 in
          let typ2 = merge_typ env' typ12 typ22 in
            TFun(typ1, fun t -> subst_typ x t typ2)
      | TBase _, _
      | TFun _, _
      | TAbs _, _
      | TApp _, _ -> Format.printf "merge_typ: %a,%a@." CEGAR_print.typ typ CEGAR_print.typ typ'; assert false

let add_preds map env =
  let aux (f,typ) =
    try
      f, merge_typ [] typ (List.assoc f map)
    with Not_found -> f, typ
  in
    List.map aux env


let rec add_to_path path typ1 typ2 =
  match path,typ2 with
      [],_ -> merge_typ [] typ1 typ2
    | 0::path',TFun(typ21,typ22) -> TFun(add_to_path path' typ1 typ21, typ22)
    | 1::path',TFun(typ21,typ22) -> TFun(typ21, fun x -> add_to_path path' typ1 (typ22 x))
    | _ -> Format.printf "%a@." CEGAR_print.typ typ2; assert false

let rec add_pred n path typ =
  match typ with
      TBase _ -> assert false
    | TFun(typ1,typ2) when n=0 ->
        TFun(typ1, fun x -> add_to_path (List.tl path) typ1 (typ2 x))
    | TFun(typ1,typ2) ->
        assert (List.hd path = 1);
        TFun(typ1, fun x -> add_pred (n-1) (List.tl path) (typ2 x))
    | TAbs _ -> assert false
    | TApp _ -> assert false


let refine preds prefix ces ((env,defs,main):prog) =
  let tmp = get_time () in
  if Flag.print_progress then Format.printf "\n(%d-4) Discovering predicates ... @?" !Flag.cegar_loop;
  let ces =
    if Flag.use_prefix_trace
    then
      let prefix' =
        assert false
          (*
            match !Flag.refine with
            Flag.RefineDependentType ->
            let rec aux = function
            [] -> []
            | [LineNode _] -> [EventNode "then_fail"]
            | [LineNode _] -> [EventNode "else_fail"]
            | n::ce -> n :: aux ce
            in
            aux prefix
            | _ -> prefix
          *)
      in
        Format.printf "\nPrefix of spurious counter-example::\n%a\n@." CEGAR_print.print_ce prefix';
        prefix' :: List.tl ces
    else ces
  in
  let map =
    match !Flag.refine with
        Flag.RefineSizedType ->
          let rec aux = function
              (BranchNode n)::ce -> n :: aux ce
            | (EventNode _)::ce -> aux ce
            | [] -> []
          in
            LazyInterface.infer [List.hd (List.map aux ces)] (env,defs,main)
      | Flag.RefineDependentType ->
          if not (List.mem Flag.CPS !Flag.form)
          then failwith "Program must be in CPS @ ModelCheckCPS"; 
          try
            RefineDepTyp.infer ces (env,defs,main)
          with RefineDepTyp.Untypable -> raise CannotRefute
  in
  let map' =
    let aux map (f,n,path) =
      List.map (fun (g,typ) -> if f = g then g, add_pred n path typ else g, typ) map
    in
      List.fold_left aux map preds
  in
  let env' = add_preds map' env in
    add_time tmp Flag.time_cegar;
    if Flag.print_progress then Format.printf "DONE!@.";
    env', defs, main


