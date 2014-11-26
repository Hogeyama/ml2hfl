
open Util
open CEGAR_syntax
open CEGAR_type
open CEGAR_print
open CEGAR_util
open CEGAR_abst_util

exception NotRefined

let debug () = List.mem "CEGAR_abst" !Flag.debug_module

let incr_wp_max = ref false
let prev_abst_defs : fun_def list ref = ref []

let abst_arg x typ =
  let ps =
    match typ with
    | TBase(_,ps) -> ps (Var x)
    | _ -> []
  in
  let n = List.length ps in
  List.mapi (fun i p -> p, App(Const (Proj(n,i)), Var x)) ps



let rec coerce env cond pts typ1 typ2 t =
  match typ1,typ2 with
  | _ when congruent env cond typ1 typ2 -> t
  | TBase(_,ps1),TBase(_,ps2) ->
      let var = match t with Var x -> Some x | _ -> None in
      let x = Option.default (new_id "x") var in
      let env' = (x,typ1)::env in
      let pts' = abst_arg x typ1 @@@ pts in
      let ts = List.map (abst env' cond pts') (ps2 (Var x)) in
      begin
        match var with
        | None -> Let(x, t, make_app (Const (Tuple (List.length ts))) ts)
        | Some _ -> make_app (Const (Tuple (List.length ts))) ts
      end
  | TFun(typ11,typ12), TFun(typ21,typ22) ->
      let x = new_id "x" in
      let typ12 = typ12 (Var x) in
      let typ22 = typ22 (Var x) in
      let env' = (x,typ11)::env in
      let t1 = coerce env' cond pts typ21 typ11 @@ Var x in
      let t2 = coerce env' cond pts typ12 typ22 @@ App(t, t1) in
      Fun(x, None, t2)
  | _ -> Format.printf "COERCE: %a, %a@." CEGAR_print.typ typ1 CEGAR_print.typ typ2; assert false

let coerce env cond pts typ1 typ2 =
  if false then Format.printf "COERCE: %a  ===>  %a@." CEGAR_print.typ typ1 CEGAR_print.typ typ2;
  coerce env cond pts typ1 typ2



let rec abstract_term env cond pbs t typ =
  match t with
  | Var x -> coerce env cond pbs (List.assoc x env) typ t
  | t when is_base_term env t ->
      let typ',src =
        match get_typ env t with
        | TBase(TInt,_) -> TBase(TInt, fun x -> [make_eq_int x t]), App(Const (Tuple 1), Const True)
        | TBase(TBool,_) -> TBase(TBool, fun x -> [make_eq_bool x t]), App(Const (Tuple 1), Const True)
        | TBase(TUnit,_) -> TBase(TUnit, fun x -> []), Const (Tuple 0)
        | _ -> assert false
      in
      coerce env cond pbs typ' typ src
  | Const c -> coerce env cond pbs (get_const_typ c) typ t
  | App(Const (RandInt _), t) -> App(abstract_term env cond pbs t (TFun(typ_int, fun _ -> typ)), Const (Tuple 0))
  | App(Const (Label n), t) -> App(Const (Label n), abstract_term env cond pbs t typ)
  | App(t1, t2) ->
      let typ' = get_typ env t1 in
      let typ1,typ2 =
        match typ' with
        | TFun(typ1,typ2) -> typ1, typ2 t2
        | _ -> assert false
      in
      let t1' = abstract_term env cond pbs t1 typ' in
      let t2' = abstract_term env cond pbs t2 typ1 in
      coerce env cond pbs typ2 typ (App(t1',t2'))
  | Let(x,t1,t2) ->
      let typ' = get_typ env t1 in
      let t1' = abstract_term env cond pbs t1 typ' in
      let env' = (x,typ')::env in
      let pbs' = abst_arg x typ' @@@ pbs in
      let t2' = abstract_term env' cond pbs' t2 typ in
      Let(x,t1',t2')
  | Fun _ -> assert false



let abstract_def env (f,xs,t1,e,t2) =
  let rec aux typ xs env =
    match xs with
    | [] -> typ, env
    | x::xs' ->
        let typ1,typ2 =
          match typ with
          | TFun(typ1,typ2) -> typ1, typ2 (Var x)
          | _ -> assert false
        in
        let env' = (x,typ1)::env in
        aux typ2 xs' env'
  in
  let typ,env' = aux (List.assoc f env) xs env in
  let pbs = List.rev_flatten_map (Fun.uncurry abst_arg) env' in
  let t2' = abstract_term env' [t1] pbs t2 typ in
  if e <> [] && t1 <> Const True
  then
    let g = new_id "f" in
    let fv = List.diff (get_fv t2') (List.map fst env) in
    let t = assume env' [] pbs t1 (make_app (Var g) @@ List.map _Var fv) in
    [g,fv,Const True,e,t2'; f,xs,Const True,[],t]
  else
    [f, xs, Const True, e, assume env' [] pbs t1 t2']




let abstract orig_fun_list prog =
  Format.printf "Warning: Abstraction for non-CPS programs is unmaintained.@.";
  let prog = make_arg_let prog in
  let labeled,prog = add_label prog in
  let () = if debug() then Format.printf "MAKE_ARG_LET:\n%a@." CEGAR_print.prog prog in
  ignore @@ Typing.infer prog;
  let defs = List.rev_flatten_map (abstract_def prog.env) prog.defs in
  let prog = {prog with env=[]} in
  if debug() then Format.printf "ABST:\n%a@." CEGAR_print.prog prog;
  labeled, Typing.infer prog




let abstract orig_fun_list force prog =
  let tmp = get_time() in
  let () =
    if !Flag.print_progress
    then Color.printf Color.Green "(%d-1) Abstracting ... @?" !Flag.cegar_loop
  in
  let labeled,abst =
    if List.mem ACPS prog.attr
    then CEGAR_abst_CPS.abstract orig_fun_list force prog
    else abstract orig_fun_list prog
  in
  let () = if debug() then Format.printf "Abstracted program::@\n%a@." CEGAR_print.prog abst in
  let () = if !Flag.print_progress then Color.printf Color.Green "DONE!@.@." in
  let () = add_time tmp Flag.time_abstraction in
  let abst',_,_ = CEGAR_trans.rename_prog abst in
  if !incr_wp_max && !prev_abst_defs = abst'.defs
  then raise NotRefined;
  incr_wp_max := false;
  prev_abst_defs := abst'.defs;
  labeled, abst
