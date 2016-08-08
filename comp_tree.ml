open Util
open Syntax
open Term_util
open Type
open Modular_syntax

module RT = Rose_tree

let debug () = List.mem "Comp_tree" !Flag.debug_module

type label =
  | App of fun_id * (id * value) list
  | Let of id * typed_term
  | Assume of typed_term
  | Spawn of id * id list option
  | Value of value
  | Fail
  | Bottom (* Need? *)
and t = node RT.t
and node =
  {nid : nid; (* ID of node *)
   val_env : val_env; (* Value environment *)
   var_env : var_env; (* Scope environment: var_env(x) is a set of variables which are visible from x *)
   label : label;
   ref_typ : Ref_type.t option; (* Need? *)
   ce_env : (tid * (id * int * int list)) list}
and nid = int
and var_env = (id * id list) list
and val_env = (id * value) list
and value = Closure of var_env * val_env * typed_term

and fun_id = id * id_kind
and id_kind =
  | ArgFun
  | Local
  | Thread of tid * int
  | TopLevel
and tid = int

(*
module Id =
  struct
    include Id
    let new_var_id' x = new_var_id x
    let new_var_id x = new_var_id x
  end
module Trans =
  struct
    include Trans
    let alpha_rename t = t
  end
 *)

let rec pr_bind depth fm (x,vl) =
  let Closure(var_env,val_env,t) = vl in
  if depth <= 0 then
    Format.fprintf fm "%a := @[[...]%a@]" Id.print x Print.term t
  else
    Format.fprintf fm "%a := @[%a%a@]" Id.print x (pr_env (depth-1)) val_env Print.term t
and pr_env depth fm env = List.print (pr_bind depth) fm env
let pr_env fm env = pr_env 0 fm env

let rec print_fun_id fm (f, kind) =
  match kind with
  | ArgFun -> Format.fprintf fm "$%a" Id.print f
  | Local -> Format.fprintf fm "%a" Id.print f
  | Thread(tid, n) -> Format.fprintf fm "%a:%a#%d" Id.print f print_tid tid n
  | TopLevel -> Format.fprintf fm "%a" Id.print f
and print_tid = Format.pp_print_int
and print_label fm label =
  match label with
  | App(f,map) when false ->
      Format.fprintf fm "@[App %a %a@]" print_fun_id f pr_env map
  | App(f,map) ->
      Format.fprintf fm "@[App %a ...@]" print_fun_id f
  | Let(f,t) ->
      Format.fprintf fm "@[Let %a =@ %a@]" Id.print f Print.term t
  | Assume t -> Format.fprintf fm "@[Assume %a@]" Print.term t
  | Spawn(f,gs) -> Format.fprintf fm "@[Spawn %a, %a@]" Id.print f (Option.print @@ List.print Id.print) gs
  | Value t -> Format.fprintf fm "@[Value %a@]" print_value t
  | Fail -> Format.fprintf fm "Fail"
  | Bottom -> Format.fprintf fm "Bottom"
and print_value fm (Closure(var_env,val_env,t)) =
  Format.fprintf fm "Value %a" Print.term t
and print_node fm {nid;var_env;val_env;label;ref_typ} =
  if false then
    Format.fprintf fm "%d,@ @[(*%a*)@],@ @[%a@],@ %a" nid (List.print Id.print) (List.map fst val_env) print_label label (Option.print Ref_type.print) ref_typ
  else
    Format.fprintf fm "%d,@ @[%a@],@ %a" nid print_label label (Option.print Ref_type.print) ref_typ
let rec print fm (Rose_tree.Node(node,ts)) =
  Format.fprintf fm "(@[<hov>%a,@ %a@])" print_node node (List.print print) ts

let make_fix f xs t =
  make_letrec [f, xs, t] @@ make_var f
let decomp_fix t =
  match t.desc with
  | Let(Recursive, [f, xs, t'], {desc=Var g}) when f = g -> Some (f, xs, t')
  | _ -> None
let is_fix t = decomp_fix t <> None

let get_arg_num = List.length -| Triple.snd -| Option.get -| decomp_fix


let counter = Counter.create ()
let make_new_ce_env ce =
  let aux (f,path) =
    f, Counter.gen counter, path
  in
  List.map aux ce

let add_tid_var = make_trans2 ()
let add_tid_var_term (f,l) t =
  let t' = add_tid_var.tr2_term_rec (f,l) t in
  match t.desc with
  | Var g when Id.same f g -> add_id l t'
  | _ -> t'
let () = add_tid_var.tr2_term <- add_tid_var_term
let add_tid_var = add_tid_var.tr2_term

let add_tid_if = make_trans2 ()
let add_tid_if_term (tid,env) t =
  match t.desc with
  | If _ ->
      add_id tid @@ add_tid_if.tr2_term_rec (tid,env) t
  | Let(flag, bindings, t1) ->
      let bindings' =
        let aux (g,xs,t) =
          g, xs, add_tid_if.tr2_term (List.assoc_default ~eq:Id.eq tid g env, env) t
        in
        List.map aux bindings
      in
      let t1' = add_tid_if.tr2_term (tid,env) t1 in
      make_let_f flag bindings' t1'
  | _ -> add_tid_if.tr2_term_rec (tid,env) t
let () = add_tid_if.tr2_term <- add_tid_if_term
let add_tid_if = add_tid_if.tr2_term

let get_tid t = get_id t

let term_of_value (Closure(_,_,t)) = t
let var_env_of_value (Closure(env,_,_)) = env
let val_env_of_value (Closure(_,env,_)) = env

let is_fail t =
  match t.desc with
  | App({desc=Event("fail",_)}, [_]) -> true
  | _ -> false

let rec arity fun_env val_env t =
  assert (is_value t);
  match t.desc with
  | Var f when Id.mem_assoc f fun_env -> List.length @@ fst @@ Id.assoc f fun_env
  | App(t1, ts) -> arity fun_env val_env t1 - List.length ts
  | _ -> assert false

let rec is_value fun_env val_env t =
  match t.desc with
  | BinOp(And, _, _) -> assert false
  | BinOp(Or, _, _) -> assert false
  | Var _ -> assert false
  | Const _ -> true
  | BinOp(_, t1, t2) -> is_value fun_env val_env t1 && is_value fun_env val_env t2
  | Not t -> is_value fun_env val_env t
  | Fun _ -> true
  | Event _ -> true
  | Let _ -> is_fix t
  | App(t1, ts) ->
      is_value fun_env val_env t1 &&
      arity fun_env val_env t < List.length ts &&
      List.for_all (is_value fun_env val_env) ts
  | _ -> false

let assoc_fun is_arg f var_env val_env =
  if is_arg && false then
    let ys,_ =  decomp_tfun @@ Id.typ f in
    let ys' = List.map Id.new_var_id ys in
    let ys'' = List.map Id.new_var_id ys in
(*
    let t = make_funs ys' @@ make_app (make_var f) @@ List.map make_var ys' in
 *)
    let f' = Id.new_var_id f in
    let var_env' = (f', Id.assoc f var_env)::var_env in
    let val_env' = (f', Id.assoc f val_env)::val_env in
    let t' = make_app (make_var f') @@ List.map make_var ys'' in
    Format.printf "    ASSOC_FUN: %a => %a@\n" Id.print f Id.print f';
    Format.printf "    ASSOC_FUN: %a => %a@\n" (List.print Id.print) ys (List.print Id.print) ys';
    Format.printf "    ASSOC_FUN: %a => %a@\n" (List.print Id.print) ys (List.print Id.print) ys'';
    var_env', val_env', (ys'', t')
  else
    let Closure(var_env_f, val_env_f, t) = Id.assoc f val_env in
    let ys,t' = decomp_funs t in
    let ys' = List.map Id.new_var_id ys in
    if !!debug then Format.printf "    ALPHA: %a => %a@\n" (List.print Id.print) ys (List.print Id.print) ys';
    let t'' = List.fold_right2 subst_var ys ys' t' in
    var_env_f, val_env_f, (ys', t'')

let ends_with_fail = RT.exists (fun {label} ->  label = Fail)

let spawn is_top nid env var_env val_env ce_env f children =
  let children' = List.filter ends_with_fail children in
  let label,ref_typ =
    if is_top then
      let aux (Rose_tree.Node({label},_)) =
        match label with
        | App((f, _), _) -> f
(*
        | Spawn(f, _) -> f
*)
        | _ -> assert false
      in
      Spawn(f, Some (List.map aux children')), None
    else
      Spawn(f, None), Some (Ref_type.Env.assoc f env)
  in
  RT.Node({nid; var_env; val_env; label; ref_typ; ce_env}, children')

let value_of var_env val_env t = Closure(var_env, val_env, t)
(*
let make_arg_map var_env val_env f xs ts =
  if !!debug then Format.printf "      MAM: %a %a@\n" (List.print Id.print) xs (List.print Print.term) ts;
  let eta t =
    let ys,t' = decomp_funs t in
    let zs,typ = decomp_tfun t'.typ in
    if zs = [] then
      [], [], Closure(var_env, val_env, t)
    else
      let zs' = List.map Id.new_var_id zs in
      let g = Id.new_var ~name:"f" @@ List.fold_right _TFun (ys@zs) typ in
      let t'' = make_funs (ys@zs') @@ make_app t' @@ List.map make_var zs' in
      let Closure(var_env_f,val_env_f,_) = Id.assoc f val_env in
      let var_env_g,val_env_g =
        let aux (renv,lenv) x t =
          let renv' = (x, List.map fst renv)::renv in
          let lenv' = (x, Closure(renv, lenv, t))::lenv in
          renv', lenv'
        in
        List.fold_left2 aux (var_env_f,val_env_f) xs ts
      in
      let var_env' = [g, List.map fst var_env_g] in
      let val_env' = [g, Closure(var_env, val_env, t'')] in
      let ws = ys @ zs in
      let ws' = List.map Id.new_var_id ws in
      var_env', val_env', Closure(var_env'@var_env, val_env'@val_env, make_funs ws' @@ make_app (make_var g) @@ List.map make_var ws')
  in
  let var_envs,val_envs,ts' = List.split3 @@ List.map eta ts in
  let var_env' = List.flatten var_envs in
  let val_env' = List.flatten val_envs in
  if !!debug then Format.printf "      MAM var_env': %a@\n" (List.print @@ Pair.print Id.print @@ List.print Id.print) var_env';
  if !!debug then Format.printf "      MAM val_env': %a@\n" pr_env val_env';
  var_env', val_env', List.combine xs ts'
(*
  let aux (env,acc) x t =
    let vl = value_of env t in
    (x,vl)::env, (x,vl)::acc
  in
  List.rev @@ snd @@ List.fold_left2 aux (env,[]) xs ts
 *)
 *)

let eta_expand t =
  let xs,t' = decomp_funs t in
  let ys,_ = decomp_tfun t'.typ in
  let ys' = List.map Id.new_var_id ys in
  match t'.desc with
  | Let(flag, bindings, t'') -> make_funs (xs@ys') @@ make_let_f flag bindings @@ make_app t'' @@ List.map make_var ys'
  | _ -> make_funs (xs@ys') @@ make_app t' @@ List.map make_var ys'

let make_arg_map var_env val_env _ xs ts =
  let value_of var_env val_env t = Closure(var_env, val_env, eta_expand t) in
  List.combine xs @@ List.map (value_of var_env val_env) ts

(*
let eta_and_lift = make_trans2 ()
let eta_and_lift_desc vars desc =
  match desc with
  | App(t, ts) ->
  | Let(flag, bindings, t) ->
      let aux (f,xs,t) =
        f,
      in
      Let(flag, List.map aux bindings, eta_and_lift_desc vars t)
  | Fun _ -> assert false
  | _ -> eta_and_lift_desc.tr_desc_rec vars desc
let () = eta_and_lift.tr2_term <- eta_and_lift_term
let eta_and_lift t = eta_and_lift.tr_term2 [] t
 *)

(* typ_env is unused? *)
(* 't' must be a CPS term *)
let rec from_term
          (cnt : Counter.t)
          (ext_funs : id list) (* UNUSED? *)
          (args : id list)
          (top_funs : id list)
          (top_fun_args : id list)
          (typ_env : Ref_type.Env.t)
          (fun_env : (id * (id list * typed_term)) list)
          (var_env : var_env)
          (val_env : val_env)
          (ce_set : ce_set)
          (extend : (id * int) list)
          (ce_env : (tid * (id * int * int list)) list)
          (t : typed_term)
        : t list =
  let f g = if !!debug then print_begin_end g else !!g in
  f (fun () ->
  let nid = Counter.gen cnt in
  if !!debug then Format.printf "TERM: %a@\n" Print.term t;
  if !!debug then Format.printf "  val_env: %a@\n" pr_env val_env;
  if !!debug then Format.printf "Dom(var_env): %a@\n" (List.print Id.print) @@ List.map fst var_env;
  if !!debug then Format.printf "Dom(val_env): %a@\n" (List.print Id.print) @@ List.map fst val_env;
  assert (List.Set.eq ~eq:Id.eq (List.map fst var_env) (List.map fst val_env));
  match t.desc with
  | Const Unit -> []
  | App({desc=Const(RandValue(TInt, true))}, [{desc=Const Unit}; {desc=Fun(x,t2)}]) ->
      let t2' = subst_var x (Id.new_var_id x) t2 in
      from_term cnt ext_funs args top_funs top_fun_args typ_env fun_env var_env val_env ce_set extend ce_env t2'
  | App({desc=Fun(x, t)}, [t2]) when is_base_typ t2.typ ->
      let x' = Id.new_var_id x in
      let t' = subst_var x x' t in
      let val_env' = (x', Closure(var_env, val_env, t2))::val_env in
      let var_env' = (x', List.map fst val_env)::var_env in
      from_term cnt ext_funs args top_funs top_fun_args typ_env fun_env var_env' val_env' ce_set extend ce_env t'
  | App({desc=Var f}, ts) when Id.mem_assoc f fun_env -> (* Top-level functions *)
      if !!debug then Format.printf "  APP2,%a@\n" Id.print f;
      let ys,t_f = Id.assoc f fun_env in
      if !!debug then Format.printf "    @[D:(%a %a = %a), A:(%a %a)@\n" Id.print f (List.print Id.print) ys Print.term t_f Id.print f (List.print Print.term) ts;
      assert (List.length ys = List.length ts);
      let children =
        let aux i ce =
          let new_ce_env = make_new_ce_env ce in
          let tid_env = List.map (fun (f,tid,path) -> f, tid) new_ce_env in
          if !!debug then Format.printf "    TID_ENV: %a@\n" (List.print @@ Pair.print Id.print Format.pp_print_int) tid_env;
          let tid = Id.assoc f tid_env in
          let f' = Id.new_var_id f in
          if !!debug then Format.printf "    ALPHA: %a => %a@\n" Id.print f Id.print f';
          let var_env' = (f', []) :: var_env in
          let val_env' =
            let t_f' =
              t_f
              |> add_tid_var (f, tid)
              |> add_tid_if (tid, tid_env)
              |> Trans.alpha_rename
              |> subst_var f f'
            in
            let var_env_f' = [f', []] in
            let rec val_env_f' = [f', Closure(var_env_f', val_env_f', make_funs ys t_f')] in
            val_env_f' @ val_env
          in
          let ce_env' = List.map (fun (f,tid,path) -> tid, (f, i, path)) new_ce_env @ ce_env in
          let t' = make_app (add_id tid @@ make_var f') ts in
          from_term cnt ext_funs args (f'::top_funs) top_fun_args typ_env fun_env var_env' val_env' ce_set extend ce_env' t'
        in
        let paths = List.assoc_all ~eq:Id.eq f ce_set in
        List.flatten_mapi aux paths
      in
      [spawn true nid typ_env var_env val_env ce_env f children]
  | App({desc=Var f} as t1, ts) when Id.mem_assoc f val_env ->
      if !!debug then Format.printf "  APP1: %a@\n" Print.term t;
      let var_env_f,val_env_f,(ys,t_f) = assoc_fun (Id.mem f args) f var_env val_env in
      if !!debug then Format.printf "    APP1 (ys -> t_f): %a, %d@\n" Print.term (make_funs ys t_f) (List.length ts);
      if !!debug then Format.printf "    APP1 var_env_f: %a@\n" (List.print @@ Pair.print Id.print @@ List.print Id.print) var_env_f;
      let arg_map = make_arg_map var_env val_env f ys ts in
      if !!debug then Format.printf "    APP1 arg_map: %a@\n" pr_env arg_map;
      let val_env' = List.rev arg_map @ val_env_f in
      if !!debug then Format.printf "    APP1 val_env': %a@\n" pr_env val_env';
      let vars_f = Id.assoc f var_env in
      if !!debug then Format.printf "    APP1 vars_f: %a@\n" (List.print Id.print) vars_f;
      let var_env',_ = List.fold_left (fun (acc,vars) x -> (x, vars)::acc, x::vars) (var_env_f,vars_f) ys in
      if !!debug then Format.printf "    APP1 var_env': %a@\n" (List.print @@ Pair.print Id.print @@ List.print Id.print) var_env';
      let fun_id =
        let kind =
          match get_id_option t1 with
          | None ->
              if Id.mem f args then
                ArgFun
              else
                Local
          | Some tid -> Thread(tid, Triple.snd @@ List.assoc tid ce_env)
        in
        f, kind
      in
      let args' = ys @ args in
      let node =
        let label = App(fun_id, arg_map) in
        let ref_typ = Ref_type.Env.assoc_option f typ_env in
        {nid; var_env; val_env; label; ref_typ; ce_env}
      in
      assert (List.Set.eq ~eq:Id.eq (List.map fst var_env') (List.map fst val_env'));
      let top_fun_args' = if Id.mem f top_funs then ys @ top_fun_args else top_fun_args in
      [RT.Node(node, from_term cnt ext_funs args' top_funs top_fun_args' typ_env fun_env var_env' val_env' ce_set extend ce_env t_f)]
  | If(t1, t2, t3) ->
      if !!debug then Format.printf "  IF t1: %a@\n" Print.term t1;
      let tid = get_id_option t in
      let aux extend' br ce_env' =
        let nid = Counter.gen cnt in
        let cond,t23 = if br = 0 then t1, t2 else make_not t1, t3 in
        if !!debug then Format.printf "    t23: %a@\n" Print.term t23;
        let node =
          let label = Assume cond in
          let ref_typ = None in
          {nid; var_env; val_env; label; ref_typ; ce_env}
        in
        RT.Node(node, from_term cnt ext_funs args top_funs top_fun_args typ_env fun_env var_env val_env ce_set extend' ce_env' t23)
      in
      let rs =
        match tid with
        | None ->
            [aux extend 0 ce_env; aux extend 1 ce_env]
        | Some tid ->
            if !!debug then Format.printf "    tid: %d@\n" tid;
            let (f,i,ce),ce_env' = List.decomp_assoc tid ce_env in
            if !!debug then Format.printf "    ce: %a@\n" (List.print Format.pp_print_int) ce;
            begin
              match ce with
              | [] when List.mem_assoc f extend ->
                  let extend' =
                    List.assoc_map ~eq:Id.eq f pred extend
                    |> snd
                    |> List.filter_out (fun (_,n) -> n = 0)
                  in
                  [aux extend' 0 ce_env; aux extend' 1 ce_env]
              | [] -> []
              | br::ce' ->
                  if !!debug then Format.printf "    CE[%d]: %a@\n" tid (List.print Format.pp_print_int) ce;
                  let ce_env'' = (tid,(f,i,ce'))::ce_env' in
                  [aux extend br ce_env'']
            end
      in
      List.filter ends_with_fail rs
  | App({desc=Fun _; typ} as t1, ts) ->
      let f = Id.new_var typ in
      make_app (make_var f) ts
      |> make_let [f, [], t1]
      |> from_term cnt ext_funs args top_funs top_fun_args typ_env fun_env var_env val_env ce_set extend ce_env
  | Let(flag, [f,[],({desc=Bottom} as t1)], _) ->
      from_term cnt ext_funs args top_funs top_fun_args typ_env fun_env var_env val_env ce_set extend ce_env t1
  | Let(flag, [f,xs,t1], t2) ->
      if !!debug then Format.printf "  LET@\n";
      if !!debug then Format.printf "    t: %a@\n" Print.term t;
      assert (xs <> []);
      let var_env' = (f, List.map fst val_env)::var_env in
      let rec val_env' = (f, Closure(var_env', val_env', make_funs xs @@ eta_expand t1))::val_env in
      let node =
        let label = Let(f, make_funs xs t1) in
        let ref_typ = None in
        {nid; var_env; val_env; label; ref_typ; ce_env}
      in
      [RT.Node(node, from_term cnt ext_funs args top_funs top_fun_args typ_env fun_env var_env' val_env' ce_set extend ce_env t2)]
  | _ when is_fail t ->
      let node =
        let label = Fail in
        let ref_typ = None in
        {nid; var_env; val_env; label; ref_typ; ce_env}
      in
      [RT.Node(node, [])]
  | _ when t.desc = Bottom ->
      let node =
        let label = Bottom in
        let ref_typ = None in
        {nid; var_env; val_env; label; ref_typ; ce_env}
      in
      [RT.Node(node, [])]
  | _ ->
      Format.printf "@.t: @[%a@." Print.term t;
      Format.printf "Dom(val_env): %a@." (List.print Id.print) @@ List.map fst val_env;
      Format.printf "Dom(fun_env): %a@." (List.print Id.print) @@ List.map fst fun_env;
      let f =
        (match t.desc with App({desc=Var f},_) -> f | _ -> assert false)
      in
      Format.printf "%a is in Dom(fun_env)?: %b@." Id.print f (Id.mem_assoc f fun_env);
      Format.printf "%a is in Dom(val_env)?: %b@." Id.print f (Id.mem_assoc f val_env);
      unsupported "Comp_tree.from_term")
let from_term typ_env fun_env ce_set extend t =
  let ext_funs = Ref_type.Env.dom typ_env in
  from_term (Counter.create()) ext_funs [] [] [] typ_env fun_env [] [] ce_set extend [] t



(* Dom(env) and Dom(fun_env) must be disjoint *)
let from_program env fun_env (ce_set:ce_set) extend t =
  if !!debug then Format.printf "@.CE_SET: %a@." print_ce_set ce_set;
  if !!debug then Format.printf "ENV: %a@." Ref_type.Env.print env;
  if !!debug then Format.printf "FUN_ENV: %a@." (List.print @@ Pair.print Id.print Print.term) @@ List.map (Pair.map_snd @@ Fun.uncurry make_funs) fun_env;
  if !!debug then Format.printf "from_program: %a@." Print.term t;
  t
  |*@> Format.printf "normalized: %a@.@." Print.term
  |> from_term env fun_env ce_set extend
  |> List.get
  |@!!debug&> Format.printf "comp_tree:@.%a@.@." print
