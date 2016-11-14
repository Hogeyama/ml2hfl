open Util
open Syntax
open Term_util
open Type
open Modular_common

module RT = Rose_tree

module Debug = Debug.Make(struct let check = make_debug_check __MODULE__ end)

type nlabel =
  | App of id * (id * value) list
  | Let of id * term
  | Assume of term
  | Branch of label option
  | Spawn of id * id
  | Fail
  | End
and t = node RT.t
and node =
  {nid : nid; (* ID of node *)
   val_env : val_env; (* Value environment *)
   var_env : var_env; (* Scope environment: var_env(x) is a set of variables which are visible from x *)
   ce_env : ce_env; (* Counterexamples *)
   nlabel : nlabel}
and nid = int
and var_env = (id * id list) list
and val_env = (id * value) list
and ce_env = (label * bool) list list
and value = Closure of var_env * val_env * ce_env option * term



let rec pr_bind depth fm (x,vl) =
  let Closure(var_env,val_env,ce_env,t) = vl in
  if depth <= 0 then
    match ce_env with
    | None -> Format.fprintf fm "%a := @[[...]%a@]" Id.print x Print.term t
    | Some ce_env -> Format.fprintf fm "%a := @[%a[...]%a@]" Id.print x (List.print print_ce) ce_env Print.term t
  else
    Format.fprintf fm "%a := @[%a%a@]" Id.print x (pr_env (depth-1)) val_env Print.term t
and pr_env depth fm env = List.print (pr_bind depth) fm env
let pr_env fm env = pr_env 0 fm env

let rec print_tid = Format.pp_print_int
and print_nlabel fm nlabel =
  match nlabel with
  | App(f,map) when false ->
      Format.fprintf fm "@[App %a %a@]" Id.print f pr_env map
  | App(f,map) ->
      Format.fprintf fm "@[App %a ...@]" Id.print f
  | Let(f,t) when false ->
      Format.fprintf fm "@[Let %a =@ %a@]" Id.print f Print.term t
  | Let(f,t) ->
      Format.fprintf fm "@[Let %a =@ ...@]" Id.print f
  | Assume t -> Format.fprintf fm "@[Assume %a@]" Print.term t
  | Branch None -> Format.fprintf fm "Branch"
  | Branch (Some label) -> Format.fprintf fm "Branch %d" label
  | Spawn(f,g) -> Format.fprintf fm "Spawn %a as %a" Id.print f Id.print g
  | Fail -> Format.fprintf fm "Fail"
  | End -> Format.fprintf fm "End"
and print_value fm (Closure(var_env,val_env,ce_env,t)) =
  Format.fprintf fm "Value %a" Print.term t
and print_node fm {nid;var_env;val_env;ce_env;nlabel} =
  if false then
    Format.fprintf fm "%d,@ @[(*%a*)@],@ @[%a@]" nid (List.print Id.print) (List.map fst val_env) print_nlabel nlabel
  else
    Format.fprintf fm "%d,@ @[%a@],@ %a" nid print_nlabel nlabel (List.print print_ce) ce_env
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

let term_of_value (Closure(_,_,_,t)) = t
let var_env_of_value (Closure(env,_,_,_)) = env
let val_env_of_value (Closure(_,env,_,_)) = env
let ce_env_of_value (Closure(_,_,env,_)) = env

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

let assoc_fun f var_env val_env ce_env =
  let Closure(var_env_f, val_env_f, ce_env_f, t) = Id.assoc f val_env in
  let ys,t' = decomp_funs t in
  let ys' = List.map Id.new_var_id ys in
  Debug.printf "    ALPHA: %a => %a@\n" (List.print Id.print) ys (List.print Id.print) ys';
  let t'' = List.fold_right2 subst_var ys ys' t' in
  var_env_f, val_env_f, ce_env_f, (ys', t'')

let ends = RT.exists (fun {nlabel} -> nlabel = Fail || nlabel = End)

let value_of var_env val_env ce_env t = Closure(var_env, val_env, ce_env, t)

let eta_expand t =
  let xs,t' = decomp_funs t in
  let ys,_ = decomp_tfun t'.typ in
  let ys' = List.map Id.new_var_id ys in
  match t'.desc with
  | Let(flag, bindings, t'') -> make_funs (xs@ys') @@ make_let_f flag bindings @@ make_app t'' @@ List.map make_var ys'
  | _ -> make_funs (xs@ys') @@ make_app t' @@ List.map make_var ys'

let make_arg_map var_env val_env ce_env _ xs ts =
  let value_of' t = value_of var_env val_env ce_env @@ eta_expand t in
  List.combine xs @@ List.map value_of' ts

let filter_extend extend = List.filter_out (fun (_,n) -> n <= 0) extend


(* 't' must be a CPS term *)
let rec from_term
          (cnt : Counter.t)  (* counter for nid *)
          (fun_env : (id * (term * ce list)) list)
          (var_env : var_env)  (* scope environment *)
          (val_env : val_env)  (* value environment *)
          (ce_env : ce_env)  (* counterexamples *)
          (t : term)  (* term to be reduced *)
        : t =
  let nid = Counter.gen cnt in
  Debug.printf "TERM: %a@\n" Print.term t;
  Debug.printf "  val_env: %a@\n" pr_env val_env;
  Debug.printf "  Dom(var_env): %a@\n" (List.print Id.print) @@ List.map fst var_env;
  Debug.printf "  Dom(val_env): %a@\n" (List.print Id.print) @@ List.map fst val_env;
  Debug.printf "  ce_env: %a@\n" (List.print @@ print_ce) ce_env;
  assert (List.Set.eq ~eq:Id.eq (List.map fst var_env) (List.map fst val_env));
  match t.desc with
  | Const Unit ->
      let node = {nid; var_env; val_env; ce_env; nlabel = End} in
      RT.Node(node, [])
  | App({desc=Const(RandValue(TInt, true))}, [{desc=Const Unit}; {desc=Fun(x,t2)}]) ->
      let t2' = subst_var x (Id.new_var_id x) t2 in
      from_term cnt fun_env var_env val_env ce_env t2'
  | App({desc=Fun(x, t)}, [t2]) when is_base_typ t2.typ ->
      let x' = Id.new_var_id x in
      let t' = subst_var x x' t in
      let val_env' = (x', Closure(var_env, val_env, None, t2))::val_env in
      let var_env' = (x', List.map fst val_env)::var_env in
      from_term cnt fun_env var_env' val_env' ce_env t'
  | App({desc=Var f}, ts) when Id.mem_assoc f fun_env -> (* Top-level functions *)
      Debug.printf "  APP2,%a@\n" Id.print f;
      let t_f,ce_env' = Id.assoc f fun_env in
      let f' = Id.new_var_id f in
      Debug.printf "    SPAWN: %a => %a@\n" Id.print f Id.print f';
      let var_env' = (f', []) :: var_env in
      let val_env' =
        let t_f' =
          t_f
          |> Trans.alpha_rename
          |> subst_var f f'
        in
        let var_env_f' = [f', []] in
        let rec val_env_f' = [f', Closure(var_env_f', val_env_f', None, t_f')] in
        val_env_f' @ val_env
      in
      let t' = make_app (make_var f') ts in
      let node = {nid; var_env; val_env; ce_env; nlabel=Spawn(f, f')} in
      let child =
        let f = f' in
        let t = t' in
        let var_env = var_env' in
        let val_env = val_env' in
        let nid = Counter.gen cnt in
        Debug.printf "  APP1: %a@\n" Print.term t;
        let var_env_f,val_env_f,ce_env_f,(ys,t_f) = assoc_fun f var_env val_env ce_env in
        Debug.printf "    APP1 (ys -> t_f): %a, %d@\n" Print.term (make_funs ys t_f) (List.length ts);
        Debug.printf "    APP1 var_env_f: %a@\n" (List.print @@ Pair.print Id.print @@ List.print Id.print) var_env_f;
        let arg_map = make_arg_map var_env val_env (Some ce_env) f ys ts in
        Debug.printf "    APP1 arg_map: %a@\n" pr_env arg_map;
        let val_env' = List.rev arg_map @ val_env_f in
        Debug.printf "    APP1 val_env': %a@\n" pr_env val_env';
        let vars_f = Id.assoc f var_env in
        Debug.printf "    APP1 vars_f: %a@\n" (List.print Id.print) vars_f;
        let var_env',_ = List.fold_left (fun (acc,vars) x -> (x, vars)::acc, x::vars) (var_env_f,vars_f) ys in
        Debug.printf "    APP1 var_env': %a@\n" (List.print @@ Pair.print Id.print @@ List.print Id.print) var_env';
        let node = {nid; var_env; val_env; ce_env; nlabel = App(f, arg_map)} in
        assert (List.Set.eq ~eq:Id.eq (List.map fst var_env') (List.map fst val_env'));
        RT.Node(node, [from_term cnt fun_env var_env' val_env' ce_env' t_f])
      in
      RT.Node(node, [child])
  | App({desc=Var f}, ts) when Id.mem_assoc f val_env ->
      Debug.printf "  APP1: %a@\n" Print.term t;
      let var_env_f,val_env_f,ce_env_f,(ys,t_f) = assoc_fun f var_env val_env ce_env in
      Debug.printf "    APP1 (ys -> t_f): %a, %d@\n" Print.term (make_funs ys t_f) (List.length ts);
      Debug.printf "    APP1 var_env_f: %a@\n" (List.print @@ Pair.print Id.print @@ List.print Id.print) var_env_f;
      let arg_ce_env =
        let xs = List.Set.inter ~eq:Id.eq (List.map fst val_env_f) (List.map fst val_env) in
        let ys = List.map fst fun_env in
        if List.Set.subset ~eq:(fun x y -> Id.name x = Id.name y) xs ys then
          Some ce_env
        else
          None
      in
      let arg_map = make_arg_map var_env val_env arg_ce_env f ys ts in
      Debug.printf "    APP1 arg_map: %a@\n" pr_env arg_map;
      let val_env' = List.rev arg_map @ val_env_f in
      Debug.printf "    APP1 val_env': %a@\n" pr_env val_env';
      let vars_f = Id.assoc f var_env in
      Debug.printf "    APP1 vars_f: %a@\n" (List.print Id.print) vars_f;
      let var_env',_ = List.fold_left (fun (acc,vars) x -> (x, vars)::acc, x::vars) (var_env_f,vars_f) ys in
      Debug.printf "    APP1 var_env': %a@\n" (List.print @@ Pair.print Id.print @@ List.print Id.print) var_env';
      let node = {nid; var_env; val_env; ce_env; nlabel = App(f, arg_map)} in
      assert (List.Set.eq ~eq:Id.eq (List.map fst var_env') (List.map fst val_env'));
      let ce_env' = Option.default ce_env ce_env_f in
      RT.Node(node, [from_term cnt fun_env var_env' val_env' ce_env' t_f])
  | If(t1, t2, t3) ->
      Debug.printf "  IF t1: %a@\n" Print.term t1;
      let tid = get_id_option t in
      let aux br ce_env' =
        let nid = Counter.gen cnt in
        let cond,t23 = if br then t1, t2 else make_not t1, t3 in
        Debug.printf "    t23: %a@\n" Print.term t23;
        let node = {nid; var_env; val_env; ce_env=ce_env'; nlabel=Assume cond} in
        RT.Node(node, [from_term cnt fun_env var_env val_env ce_env' t23])
      in
      let children =
        match tid with
        | None -> [aux true ce_env; aux false ce_env]
        | Some tid ->
            Debug.printf "    tid: %d@\n" tid;
            let ce_env1,ce_env2 = List.partition (function (tid',_)::_ -> tid = tid' | _ -> false) ce_env in
            let then_ce_env,else_ce_env =  List.partition (function (_,b)::_ -> b | _ -> assert false) ce_env1 in
            let child1 =
              if then_ce_env = [] then
                []
              else
                let then_ce_env' = List.map List.tl then_ce_env in
                [aux true (then_ce_env'@ce_env2)]
            in
            let child2 =
              if else_ce_env = [] then
                []
              else
                let else_ce_env' = List.map List.tl else_ce_env in
                [aux false (else_ce_env'@ce_env2)]
            in
            child1 @ child2
      in
      let node = {nid; val_env; var_env; ce_env; nlabel = Branch tid} in
      RT.Node(node, children)
  | App({desc=Fun _; typ} as t1, ts) ->
      let f = Id.new_var typ in
      make_app (make_var f) ts
      |> make_let [f, [], t1]
      |> from_term cnt fun_env var_env val_env ce_env
  | Let(flag, [f,[],({desc=Bottom} as t1)], _) ->
      from_term cnt fun_env var_env val_env ce_env t1
  | Let(flag, [f,xs,t1], t2) ->
      Debug.printf "  LET@\n";
      Debug.printf "    t: %a@\n" Print.term t;
      assert (xs <> []);
      let var_env' = (f, List.map fst val_env)::var_env in
      let rec val_env' = (f, Closure(var_env', val_env', None, make_funs xs @@ eta_expand t1))::val_env in
      let node = {nid; var_env; val_env; ce_env; nlabel = Let(f, make_funs xs t1)} in
      RT.Node(node, [from_term cnt fun_env var_env' val_env' ce_env t2])
  | _ when is_fail t ->
      let node = {nid; var_env; val_env; ce_env; nlabel = Fail} in
      RT.Node(node, [])
  | _ when t.desc = Bottom ->
      let node = {nid; var_env; val_env; ce_env; nlabel = End} in
      RT.Node(node, [])
  | Let(Nonrecursive, _, _) -> assert false
  | _ ->
      Format.printf "@.t: @[%a@." Print.term t;
      Format.printf "Dom(val_env): %a@." (List.print Id.print) @@ List.map fst val_env;
      let f = match t.desc with App({desc=Var f},_) -> f | _ -> assert false in
      Format.printf "%a is in Dom(val_env)?: %b@." Id.print f (Id.mem_assoc f val_env);
      unsupported "Comp_tree.from_term"
let from_term fun_env var_env val_env ce_env t =
  from_term (Counter.create()) fun_env var_env val_env ce_env t

let rec filter_ends (RT.Node(node,ts)) =
  let check (RT.Node({nlabel},ts)) =
    ts <> [] || nlabel = Fail || nlabel = End
  in
  let ts' =
    ts
    |> List.map filter_ends
    |> List.filter check
  in
  RT.Node(node, ts')

let from_program (fun_env: (id * (id list * term)) list) (ce_set:ce_set) t =
  Debug.printf "@.CE_SET: %a@." print_ce_set ce_set;
  Debug.printf "FUN_ENV: %a@." (List.print @@ Pair.print Id.print Print.term) @@ List.map (Pair.map_snd @@ Fun.uncurry make_funs) fun_env;
  Debug.printf "from_program: %a@." Print.term t;
  let fun_env' =
    List.map (fun (f,(xs,t)) -> f, (make_funs xs t, List.assoc_all ~eq:Id.eq f ce_set)) fun_env
  in
  let var_env = [] in
  let val_env = [] in
  let ce_env = [] in
  t
  |> from_term fun_env' var_env val_env ce_env
  |@> Debug.printf "@.@.comp_tree:@.%a@.@." print
  |*> filter_ends
