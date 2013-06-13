
open Util
open CEGAR_syntax
open CEGAR_type
open CEGAR_util


exception CannotRefute

let equiv env t1 t2 =
  let t1' = FpatInterface.conv_term t1 in
  let t2' = FpatInterface.conv_term t2 in
  Fpat.Cvc3Interface.implies [t1'] [t2'] && Fpat.Cvc3Interface.implies [t1'] [t2']


let new_id' x = new_id (Format.sprintf "%s_%d" x !Flag.cegar_loop)

let add env ps p =
  if List.exists (equiv env p) ps
  then ps
  else normalize_bool_term p :: ps

let rec merge_typ env typ typ' =
  match typ,typ' with
      TBase(b1,ps1),TBase(b2,ps2) ->
        assert (b1 = b2);
        let x = new_id' "x" in
        let env' = (x,typ)::env in
        let ps1' = ps1 (Var x) in
        let ps2' = ps2 (Var x) in
        let ps = List.fold_left (add env') ps1' ps2' in
        let ps t = List.map (subst x t) ps in
          TBase(b1, ps)
    | TFun(typ11,typ12), TFun(typ21,typ22) ->
        let x = new_id' "x" in
        let env' = (x,typ11)::env in
        let typ12 = typ12 (Var x) in
        let typ22 = typ22 (Var x) in
        let typ1 = merge_typ env typ11 typ21 in
        let typ2 = merge_typ env' typ12 typ22 in
          TFun(typ1, fun t -> subst_typ x t typ2)
    | TBase _, _
    | TFun _, _
    | TAbs _, _
    | TApp _, _ -> Format.printf "merge_typ: %a,%a@." CEGAR_print.typ typ CEGAR_print.typ typ'; assert false

let add_preds_env map env =
  let aux (f,typ) =
    try
      f, merge_typ [] typ (List.assoc f map)
    with Not_found -> f, typ
  in
    List.map aux env

let add_preds_env map env =
  let aux (f,typ) =
    try
      let typ1 = typ in
      let typ2 = List.assoc f map in
      let typ' = merge_typ [] typ1 typ2 in
        f, typ'
    with Not_found -> f, typ
  in
    List.map aux env

let add_preds map prog =
  {prog with env = add_preds_env map prog.env}


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



let refine labeled prefix ces {env=env;defs=defs;main=main} =
  let tmp = get_time () in
    try
      if !Flag.print_progress then Format.printf "(%d-4) Discovering predicates ... @." !Flag.cegar_loop;
      if Flag.use_prefix_trace then raise (Fatal "Not implemented: Flag.use_prefix_trace");
      let map =
        match !Flag.refine with
            Flag.RefineRefType(flags) ->
      	      Format.printf "@[<v>";
              let ces = if !Fpat.Global.use_multiple_paths then ces else [Fpat.Util.List.hd ces] in
      	      let map = FpatInterface.infer flags labeled ces (env, defs, main) in
      	      Format.printf "@]";
              map
      in
      let env' = if !Flag.disable_predicate_accumulation then map else add_preds_env map env in
        if !Flag.print_progress then Format.printf "DONE!@.@.";
        Fpat.Cvc3Interface.close_cvc3 ();
        Fpat.Cvc3Interface.open_cvc3 ();
        add_time tmp Flag.time_cegar;
        map, {env=env';defs=defs;main=main}
    with e ->
      Fpat.Cvc3Interface.close_cvc3 ();
      Fpat.Cvc3Interface.open_cvc3 ();
      add_time tmp Flag.time_cegar;
      raise e

exception PostCondition of (Fpat.Var.t * Fpat.SimType.t) list * Fpat.Term.t

let refine_term ce { env=env; defs=defs; main=main } =
  let tmp = get_time () in
    try
      if !Flag.print_progress then Format.printf "(%d-4) Discovering ranking function ... @." !Flag.cegar_loop;
      let env, spc =
        Format.printf "@[<v>";
        let env, spc = FpatInterface.compute_strongest_post (env, defs, main) ce in
        Format.printf "@]";
        env, spc
      in
      if !Flag.print_progress then Format.printf "DONE!@.@.";
      Fpat.Cvc3Interface.close_cvc3 ();
      Fpat.Cvc3Interface.open_cvc3 ();
      add_time tmp Flag.time_cegar;
      raise (PostCondition (env, spc))
    with e ->
      Fpat.Cvc3Interface.close_cvc3 ();
      Fpat.Cvc3Interface.open_cvc3 ();
      add_time tmp Flag.time_cegar;
      raise e
