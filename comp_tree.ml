open Util
open Syntax
open Term_util
open Type
open Modular_syntax

module RT = Rose_tree

module Debug = Debug.Make(struct let check = make_debug_check __MODULE__ end)

type label =
  | App of id * (id * value) list
  | Let of id * term
  | Assume of term
  | Branch of id * id list
  | Fail
  | End
  | Empty_branch of id
and t = node RT.t
and node =
  {nid : nid; (* ID of node *)
   val_env : val_env; (* Value environment *)
   var_env : var_env; (* Scope environment: var_env(x) is a set of variables which are visible from x *)
   label : label;
   ce_env : (tid * (id * int * int list)) list}
and nid = int
and var_env = (id * id list) list
and val_env = (id * value) list
and value = Closure of var_env * val_env * term

and tid = int


let rec pr_bind depth fm (x,vl) =
  let Closure(var_env,val_env,t) = vl in
  if depth <= 0 then
    Format.fprintf fm "%a := @[[...]%a@]" Id.print x Print.term t
  else
    Format.fprintf fm "%a := @[%a%a@]" Id.print x (pr_env (depth-1)) val_env Print.term t
and pr_env depth fm env = List.print (pr_bind depth) fm env
let pr_env fm env = pr_env 0 fm env

let rec print_tid = Format.pp_print_int
and print_label fm label =
  match label with
  | App(f,map) when false ->
      Format.fprintf fm "@[App %a %a@]" Id.print f pr_env map
  | App(f,map) ->
      Format.fprintf fm "@[App %a ...@]" Id.print f
  | Let(f,t) when false ->
      Format.fprintf fm "@[Let %a =@ %a@]" Id.print f Print.term t
  | Let(f,t) ->
      Format.fprintf fm "@[Let %a =@ ...@]" Id.print f
  | Assume t -> Format.fprintf fm "@[Assume %a@]" Print.term t
  | Branch(f,gs) -> Format.fprintf fm "@[Branch %a, %a@]" Id.print f (List.print Id.print) gs
  | Fail -> Format.fprintf fm "Fail"
  | End -> Format.fprintf fm "End"
  | Empty_branch f -> Format.fprintf fm "Empty_branch %a" Id.print f
and print_value fm (Closure(var_env,val_env,t)) =
  Format.fprintf fm "Value %a" Print.term t
and print_node fm {nid;var_env;val_env;label} =
  if false then
    Format.fprintf fm "%d,@ @[(*%a*)@],@ @[%a@]" nid (List.print Id.print) (List.map fst val_env) print_label label
  else
    Format.fprintf fm "%d,@ @[%a@]" nid print_label label
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

let assoc_fun f var_env val_env =
  let Closure(var_env_f, val_env_f, t) = Id.assoc f val_env in
  let ys,t' = decomp_funs t in
  let ys' = List.map Id.new_var_id ys in
  Debug.printf "    ALPHA: %a => %a@\n" (List.print Id.print) ys (List.print Id.print) ys';
  let t'' = List.fold_right2 subst_var ys ys' t' in
  var_env_f, val_env_f, (ys', t'')

let ends = RT.exists (fun {label} -> label = Fail || label = End)

let spawn nid var_env val_env ce_env f children =
  let label =
    let aux (Rose_tree.Node({label},_)) =
      match label with
      | App(g, _) -> g
      | _ -> assert false
    in
    Branch(f, List.map aux children)
  in
  RT.Node({nid; var_env; val_env; label; ce_env}, children)

let value_of var_env val_env t = Closure(var_env, val_env, t)

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

let filter_extend extend = List.filter_out (fun (_,n) -> n <= 0) extend


(* 't' must be a CPS term *)
let rec from_term
          (cnt : Counter.t)  (* counter for nid *)
          (fun_env : (id * (id list * term)) list)  (* *)
          (var_env : var_env)  (* scope environment *)
          (val_env : val_env)  (* value environment *)
          (ce_set : ce_set)  (* counterexamples of top-level functions *)
          (ce_env : (tid * (id * int * int list)) list)  (* counterexamples of functions spawned *)
          (t : term)  (* term to be reduced *)
        : t list =
  let f g = if !!Debug.check then print_begin_end g else !!g in
  f (fun () ->
  let nid = Counter.gen cnt in
  Debug.printf "TERM: %a@\n" Print.term t;
  Debug.printf "  val_env: %a@\n" pr_env val_env;
  Debug.printf "Dom(var_env): %a@\n" (List.print Id.print) @@ List.map fst var_env;
  Debug.printf "Dom(val_env): %a@\n" (List.print Id.print) @@ List.map fst val_env;
  assert (List.Set.eq ~eq:Id.eq (List.map fst var_env) (List.map fst val_env));
  match t.desc with
  | Const Unit ->
      let node =
        let label = End in
        {nid; var_env; val_env; label; ce_env}
      in
      [RT.Node(node, [])]
  | App({desc=Const(RandValue(TInt, true))}, [{desc=Const Unit}; {desc=Fun(x,t2)}]) ->
      let t2' = subst_var x (Id.new_var_id x) t2 in
      from_term cnt fun_env var_env val_env ce_set ce_env t2'
  | App({desc=Fun(x, t)}, [t2]) when is_base_typ t2.typ ->
      let x' = Id.new_var_id x in
      let t' = subst_var x x' t in
      let val_env' = (x', Closure(var_env, val_env, t2))::val_env in
      let var_env' = (x', List.map fst val_env)::var_env in
      from_term cnt fun_env var_env' val_env' ce_set ce_env t'
  | App({desc=Var f}, ts) when Id.mem_assoc f fun_env -> (* Top-level functions *)
      Debug.printf "  APP2,%a@\n" Id.print f;
      let ys,t_f = Id.assoc f fun_env in
      Debug.printf "    @[D:(%a %a = %a), A:(%a %a)@\n" Id.print f (List.print Id.print) ys Print.term t_f Id.print f (List.print Print.term) ts;
      assert (List.length ys = List.length ts);
      let children =
        let aux i ce =
          let new_ce_env = make_new_ce_env ce in
          let tid_env = List.map (fun (f,tid,path) -> f, tid) new_ce_env in
          Debug.printf "    TID_ENV: %a@\n" (List.print @@ Pair.print Id.print Format.pp_print_int) tid_env;
          let tid = Id.assoc f tid_env in
          let f' = Id.new_var_id f in
          Debug.printf "    ALPHA: %a => %a@\n" Id.print f Id.print f';
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
          from_term cnt fun_env var_env' val_env' ce_set ce_env' t'
        in
        let paths = List.assoc_all ~eq:Id.eq f ce_set in
        List.flatten_mapi aux paths
      in
      [spawn nid var_env val_env ce_env f children]
  | App({desc=Var f}, ts) when Id.mem_assoc f val_env ->
      Debug.printf "  APP1: %a@\n" Print.term t;
      let var_env_f,val_env_f,(ys,t_f) = assoc_fun f var_env val_env in
      Debug.printf "    APP1 (ys -> t_f): %a, %d@\n" Print.term (make_funs ys t_f) (List.length ts);
      Debug.printf "    APP1 var_env_f: %a@\n" (List.print @@ Pair.print Id.print @@ List.print Id.print) var_env_f;
      let arg_map = make_arg_map var_env val_env f ys ts in
      Debug.printf "    APP1 arg_map: %a@\n" pr_env arg_map;
      let val_env' = List.rev arg_map @ val_env_f in
      Debug.printf "    APP1 val_env': %a@\n" pr_env val_env';
      let vars_f = Id.assoc f var_env in
      Debug.printf "    APP1 vars_f: %a@\n" (List.print Id.print) vars_f;
      let var_env',_ = List.fold_left (fun (acc,vars) x -> (x, vars)::acc, x::vars) (var_env_f,vars_f) ys in
      Debug.printf "    APP1 var_env': %a@\n" (List.print @@ Pair.print Id.print @@ List.print Id.print) var_env';
      let node =
        let label = App(f, arg_map) in
        {nid; var_env; val_env; label; ce_env}
      in
      assert (List.Set.eq ~eq:Id.eq (List.map fst var_env') (List.map fst val_env'));
      [RT.Node(node, from_term cnt fun_env var_env' val_env' ce_set ce_env t_f)]
  | If(t1, t2, t3) ->
      Debug.printf "  IF t1: %a@\n" Print.term t1;
      let tid = get_id_option t in
      let aux br ce_env' =
        let nid = Counter.gen cnt in
        let cond,t23 = if br = 0 then t1, t2 else make_not t1, t3 in
        Debug.printf "    t23: %a@\n" Print.term t23;
        let node =
          let label = Assume cond in
          {nid; var_env; val_env; label; ce_env}
        in
        RT.Node(node, from_term cnt fun_env var_env val_env ce_set ce_env' t23)
      in
      begin
        match tid with
        | None ->
            [aux 0 ce_env; aux 1 ce_env]
        | Some tid ->
            Debug.printf "    tid: %d@\n" tid;
            let (f,i,ce),ce_env' = List.decomp_assoc tid ce_env in
            Debug.printf "    ce: %a@\n" (List.print Format.pp_print_int) ce;
            begin
              match ce with
              | [] ->
                  let node =
                    let label = Empty_branch f in
                    {nid; var_env; val_env; label; ce_env}
                  in
                  [RT.Node(node, [])]
              | br::ce' ->
                  Debug.printf "    CE[%d]: %a@\n" tid (List.print Format.pp_print_int) ce;
                  let ce_env'' = (tid,(f,i,ce'))::ce_env' in
                  [aux br ce_env'']
            end
      end
  | App({desc=Fun _; typ} as t1, ts) ->
      let f = Id.new_var typ in
      make_app (make_var f) ts
      |> make_let [f, [], t1]
      |> from_term cnt fun_env var_env val_env ce_set ce_env
  | Let(flag, [f,[],({desc=Bottom} as t1)], _) ->
      from_term cnt fun_env var_env val_env ce_set ce_env t1
  | Let(flag, [f,xs,t1], t2) ->
      Debug.printf "  LET@\n";
      Debug.printf "    t: %a@\n" Print.term t;
      assert (xs <> []);
      let var_env' = (f, List.map fst val_env)::var_env in
      let rec val_env' = (f, Closure(var_env', val_env', make_funs xs @@ eta_expand t1))::val_env in
      let node =
        let label = Let(f, make_funs xs t1) in
        {nid; var_env; val_env; label; ce_env}
      in
      [RT.Node(node, from_term cnt fun_env var_env' val_env' ce_set ce_env t2)]
  | _ when is_fail t ->
      let node =
        let label = Fail in
        {nid; var_env; val_env; label; ce_env}
      in
      [RT.Node(node, [])]
  | _ when t.desc = Bottom ->
      let node =
        let label = End in
        {nid; var_env; val_env; label; ce_env}
      in
      [RT.Node(node, [])]
  | Let(Nonrecursive, _, _) -> assert false
  | _ ->
      Format.printf "@.t: @[%a@." Print.term t;
      Format.printf "Dom(val_env): %a@." (List.print Id.print) @@ List.map fst val_env;
      Format.printf "Dom(fun_env): %a@." (List.print Id.print) @@ List.map fst fun_env;
      let f = match t.desc with App({desc=Var f},_) -> f | _ -> assert false in
      Format.printf "%a is in Dom(fun_env)?: %b@." Id.print f (Id.mem_assoc f fun_env);
      Format.printf "%a is in Dom(val_env)?: %b@." Id.print f (Id.mem_assoc f val_env);
      unsupported "Comp_tree.from_term")
let from_term fun_env ce_set t =
  from_term (Counter.create()) fun_env [] [] ce_set [] t


let rec filter_ends (RT.Node(node,ts)) =
  let check (RT.Node({label},ts)) =
    ts <> [] || label = Fail || label = End
  in
  let ts' =
    ts
    |> List.map filter_ends
    |> List.filter check
  in
  RT.Node(node, ts')

(* Dom(env) and Dom(fun_env) must be disjoint *)
let from_program fun_env (ce_set:ce_set) t =
  Debug.printf "@.CE_SET: %a@." print_ce_set ce_set;
  Debug.printf "FUN_ENV: %a@." (List.print @@ Pair.print Id.print Print.term) @@ List.map (Pair.map_snd @@ Fun.uncurry make_funs) fun_env;
  Debug.printf "from_program: %a@." Print.term t;
  let comp_tree =
    t
    |> from_term fun_env ce_set
    |> List.get
    |@> Debug.printf "comp_tree:@.%a@.@." print
  in
  let reached_empty_branch =
    comp_tree
    |> RT.filter_map_label (function {label = Empty_branch f} -> Some f | _ -> None)
    |> List.unique ~cmp:Id.eq
  in
  let comp_tree' =
    filter_ends comp_tree
    |@> Debug.printf "comp_tree':@.%a@.@." print
  in
  reached_empty_branch, comp_tree'
