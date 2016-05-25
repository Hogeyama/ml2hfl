open Util
open Syntax
open Term_util
open Type

module RT = Rose_tree

let debug () = List.mem "Comp_tree" !Flag.debug_module

type label =
  | App of fun_id * (id * value) list
  | Let of id * typed_term
  | Assume of typed_term
  | Spawn of id * id list option
  | Value of value
  | Fail
and t = node RT.t
and node =
  {nid : nid;
   val_env : (id * value) list;
   label : label;
   ref_typ : Ref_type.t option;
   ce_env : (tid * (int * int list)) list}
and nid = int

and value = Closure of (id * value) list * typed_term

and fun_id = id * id_kind
and id_kind =
  | Local
  | Thread of tid * int
  | TopLevel
and tid = int


let rec print_fun_id fm (f, kind) =
  match kind with
  | Local -> Format.fprintf fm "%a" Id.print f
  | Thread(tid, n) -> Format.fprintf fm "%a:%a#%d" Id.print f print_tid tid n
  | TopLevel -> Format.fprintf fm "%a" Id.print f
and print_tid = Format.pp_print_int
and print_label fm label =
  match label with
  | App(f,map) ->
      let pr fm (x,vl) = Format.fprintf fm "%a" Id.print x in
      Format.fprintf fm "App %a %a" print_fun_id f (List.print pr) map
  | Let(f,t) ->
      Format.fprintf fm "Let %a := %a" Id.print f Print.term t
  | Assume t -> Format.fprintf fm "Assume %a" Print.term t
  | Spawn(f,gs) -> Format.fprintf fm "Spawn %a, %a" Id.print f (Option.print @@ List.print Id.print) gs
  | Value t -> Format.fprintf fm "Value %a" print_value t
  | Fail -> Format.fprintf fm "Fail"
and print_value fm (Closure(env,t)) =
  Format.fprintf fm "Value %a" Print.term t
and print_node fm {nid;val_env;label;ref_typ} =
  Format.fprintf fm "%d,@ @[%a@],@ @[%a@],@ %a" nid (List.print Id.print) (List.map fst val_env) print_label label (Option.print Ref_type.print) ref_typ
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
let new_tid () = Counter.gen counter

let add_tid = make_trans2 ()
let add_tid_term (f,l) t =
  let t' = add_tid.tr2_term_rec (f,l) t in
  match t.desc with
  | Var g when Id.same f g -> add_id l t'
  | If(t1, t2, t3) -> add_id l t'
  | _ -> t'
let () = add_tid.tr2_term <- add_tid_term
let add_tid = add_tid.tr2_term
let get_tid t = get_id t

let term_of_value (Closure(_,t)) = t
let env_of_value (Closure(env,_)) = env

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

let assoc_fun f env =
  let Closure(env', t) = Id.assoc f env in
  env', decomp_funs t

let spawn is_top nid env val_env ce_env f children =
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
      Spawn(f, Some (List.map aux children)), None
    else
      Spawn(f, None), Some (Id.assoc f env)
  in
  RT.Node({nid; val_env; label; ref_typ; ce_env}, children)

let eta_expand t =
  let xs,t' = decomp_funs t in
  let ys,_ = decomp_tfun t'.typ in
  let ys' = List.map Id.new_var_id ys in
  make_funs (xs@ys') @@ make_app t' @@ List.map make_var ys'

let value_of val_env t = Closure(val_env, eta_expand t)

(* 't' must be a CPS term *)
let rec from_term
          (cnt : Counter.t)
          (ext_funs : id list)
          (typ_env : (id * Ref_type.t) list)
          (fun_env : (id * (id list * typed_term)) list)
          (val_env : (id * value) list)
          (ce_set : (id * int list) list)
          (ce_env : (tid * (int * int list)) list)
          (t : typed_term)
        : t option =
  let f = if !!debug then fun f -> print_begin_end f else (!!) in
  f (fun () ->
  let nid = Counter.gen cnt in
  Format.printf "TERM: %a@\n" Print.term t;
  match t.desc with
  | Const Unit -> None
  | App({desc=Const(RandValue(TInt, true))}, [{desc=Const Unit}; {desc=Fun(x,t2)}]) ->
      from_term cnt ext_funs typ_env fun_env val_env ce_set ce_env t2
  | App({desc=Fun(_, t)}, [{desc=App({desc=Const(RandValue _)}, [{desc=Const Unit}])}]) ->
      from_term cnt ext_funs typ_env fun_env val_env ce_set ce_env t
  | App({desc=Var f}, ts) when Id.mem_assoc f typ_env && not @@ List.mem_assoc f val_env ->
      if !!debug then Format.printf "APP3, %a@\n" Id.print f;
      let n = List.length ts in
      let ref_typ = List.assoc_option ~cmp:Id.eq f typ_env in
      let children =
        let aux typ =
          let xtyps =
            typ
            |> Ref_type.decomp_funs n
            |> Triple.snd
          in
          let xtyps' = List.filter (fst |- Id.typ |- is_fun_typ) xtyps in
          let xs = List.map (Id.new_var_id -| fst) xtyps in
          Format.printf "xs: %a@\n" (List.print Id.print) xs;
          Format.printf "ts: %a@\n" (List.print Print.term) ts;
          let arg_map = List.fold_left2 (fun env y t -> (y, value_of env t)::env) [] xs ts in
          let aux' (g,typ) =
            Format.printf "Id.typ g: %a@\n" Print.typ @@ Id.typ g;
            let m = Type.arity @@ Id.typ g in
            let _,ytyps,_ = try Ref_type.decomp_funs m typ with _ -> unsupported "Comp_tree.from_term" in
            let xs = List.map fst ytyps in
            let t' = make_app (make_var g) @@ List.map make_var xs in
            Format.printf "t': %a@\n" Print.term t';
            Format.printf "t'.typ: %a@\n" Print.typ t'.typ;
            Format.printf "typ: %a@\n" Ref_type.print typ;
            let typ_env' = (g,typ) :: ytyps @ typ_env in
            from_term cnt ext_funs typ_env' fun_env arg_map ce_set ce_env t'
          in
          let children' = List.filter_map aux' xtyps' in
          let arg_map = List.fold_left2 (fun env y t -> env @ [y, value_of env t]) [] xs ts in
          let label = App((f,TopLevel), arg_map) in
          RT.Node({nid; val_env; label; ref_typ; ce_env}, children')
        in
        let typs = List.assoc_all ~cmp:Id.eq f typ_env in
        List.map aux typs
      in
      Format.printf "children: %a@\n" (List.print print) children;
      Some (spawn false nid typ_env val_env ce_env f children)
  | App({desc=Var f}, ts) when Id.mem_assoc f fun_env ->
      if !!debug then Format.printf "APP2,%a@\n" Id.print f;
      let ys,t_f = Id.assoc f fun_env in
      Format.printf "D:(%a %a = %a), A:(%a %a)@\n" Id.print f (List.print Id.print) ys Print.term t_f Id.print f (List.print Print.term) ts;
      assert (List.length ys = List.length ts);
      let tid = new_tid () in
      if !!debug then Format.printf "tid: %d@\n" tid;
      let children =
        let aux i path =
          let f' = Id.new_var_id f in
          let val_env' =
            let t_f' = Trans.alpha_rename t_f in
            let t_f'' = add_tid (f',tid) @@ subst_var f f' t_f' in
            [f', value_of val_env @@ make_funs ys t_f'']
          in
          let ce_env' = (tid,(i,path))::ce_env in
          let t' = make_app (add_id tid @@ make_var f') ts in
          from_term cnt ext_funs typ_env fun_env val_env' ce_set ce_env' t'
        in
        let paths = List.assoc_all ~cmp:Id.eq f ce_set in
        List.filter_mapi aux paths
      in
      Some (spawn true nid typ_env val_env ce_env f children)
  | App({desc=Var f} as t1, ts) when Id.mem_assoc f val_env ->
      if !!debug then Format.printf "APP1: %a@\n" Print.term t;
      let val_env',(ys,t_f) = assoc_fun f val_env in
      let ys' = List.map Id.new_var_id ys in
      if !!debug then Format.printf "APP1 (ys' -> t_f): %a, %d@\n" Print.term (make_funs ys' t_f) (List.length ts);
      let t_f' = List.fold_right2 subst_var ys ys' t_f in
      let arg_map = List.fold_left2 (fun env y t -> env @ [y, value_of (env@val_env') t]) [] ys' ts in
      let val_env'' = arg_map @ val_env' in
      let fun_id =
        let kind =
          match get_id_option t1 with
          | None -> Local
          | Some tid -> Thread(tid, fst @@ List.assoc tid ce_env)
        in
        f, kind
      in
      let label = App(fun_id, arg_map) in
      let ref_typ = List.assoc_option ~cmp:Id.eq f typ_env in
      Some (RT.Node({nid; val_env; label; ref_typ; ce_env},
                    Option.to_list @@ from_term cnt ext_funs typ_env fun_env val_env'' ce_set ce_env t_f'))
  | If(t1, t2, t3) ->
      let tid = get_tid t in
      let (i,ce),ce_env' = List.decomp_assoc tid ce_env in
      if !!debug then Format.printf "IF: %a, %a@\n" (List.print Format.pp_print_int) ce Print.term t1;
      begin
        match ce with
        | [] -> None
        | br::ce' ->
            if !!debug then Format.printf "CE[%d]: %a@\n" tid (List.print Format.pp_print_int) ce;
            let cond,t23 = if br = 0 then t1, t2 else make_not t1, t3 in
            let ce_env'' = (tid,(i,ce'))::ce_env' in
            if !!debug then Format.printf "t23: %a@\n" Print.term t23;
            let label = Assume cond in
            let ref_typ = None in
            Some (RT.Node({nid; val_env; label; ref_typ; ce_env},
                          Option.to_list @@ from_term cnt ext_funs typ_env fun_env val_env ce_set ce_env'' t23))
      end
  | Let(flag, [f,xs,t1], t2) ->
      if !!debug then Format.printf "LET@\n";
      assert (xs <> []);
      let val_env' = (f, Closure(val_env, make_funs xs t1))::val_env in
      let label = Let(f, make_funs xs t1) in
      let ref_typ = None in
      Some (RT.Node({nid; val_env; label; ref_typ; ce_env},
                    Option.to_list @@ from_term cnt ext_funs typ_env fun_env val_env' ce_set ce_env t2))
  | _ ->
      if !!debug then Format.printf "@.%a@." Print.term t;
      unsupported "Comp_tree.from_term")
let from_term typ_env fun_env ce_set t =
  let ext_funs = List.map fst typ_env in
  from_term (Counter.create()) ext_funs typ_env fun_env [] ce_set [] t





let rec is_atom t =
  match t.desc with
  | App({desc=Const(RandValue _)}, [_]) -> true
  | App({desc=Event(_, _)}, [_]) -> true
  | BinOp(_, t1, t2) -> is_atom t1 && is_atom t2
  | _ -> false

let normalize t =
  t
  |> Trans.normalize_let ~is_atom
  |> Trans.flatten_let

(* Dom(env) and Dom(fun_env) must be disjoint *)
let from_program env fun_env (ce_set: (id * int list) list) t =
  if !!debug then Format.printf "@.CE_SET: %a@." (List.print @@ Pair.print Id.print @@ List.print Format.pp_print_int) ce_set;
  if !!debug then Format.printf "ENV: %a@." (List.print @@ Pair.print Id.print Ref_type.print) env;
  if !!debug then Format.printf "FUN_ENV: %a@." (List.print @@ Pair.print Id.print Print.term) @@ List.map (Pair.map_snd @@ Fun.uncurry make_funs) fun_env;
  if !!debug then Format.printf "from_program: %a@." Print.term t;
  t
  |*> normalize
  |*@> Format.printf "normalized: %a@.@." Print.term
  |> from_term env fun_env ce_set
  |> Option.get
  |@> Format.printf "%a@.@." print
