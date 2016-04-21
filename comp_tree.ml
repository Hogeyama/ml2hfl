open Util
open Syntax
open Term_util
open Type

module RT = Rose_tree

let debug () = List.mem "Comp_tree" !Flag.debug_module

type label =
  | App of fun_id * id list * (id * typed_term) list
  | Let of id * id list * typed_term
  | Assume of typed_term
  | Spawn of id * (id * tid) list
  | Fail
and t = (nid * label) RT.t
and nid = int

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
  | App(f,env,map) ->
      let pr fm (x,t) = Format.fprintf fm "%a := %a" Id.print x Print.term t in
      Format.fprintf fm "%a, %a |- %a" print_fun_id f (List.print Id.print) env (List.print pr) map
  | Let(f,env,t) ->
      let pr fm (x,var_env,t) = Format.fprintf fm "%a%a := %a" Id.print x (List.print Id.print) var_env Print.term t in
      Format.fprintf fm "%a" pr (f,env,t)
  | Assume t -> Format.fprintf fm "Assume %a" Print.term t
  | Spawn(f,tids) -> Format.fprintf fm "Spawn %a, %a" Id.print f (List.print @@ Pair.print Id.print print_tid) tids
  | Fail -> Format.fprintf fm "Fail"
and print_idlabel fm (nid,label) =
  Format.fprintf fm "%d, %a" nid print_label label
let rec print fm (Rose_tree.Node(l,ts)) =
  Format.fprintf fm "(@[<hov>%a,@ %a@])" print_idlabel l (List.print print) ts

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

let is_fail t =
  match t.desc with
  | App({desc=Event("fail",_)}, [_]) -> true
  | _ -> false

let normalize_val_env x env ys t =
  let zs,t' = decomp_funs t in
  let ws =
    t
    |> decomp_var
    |> Option.map (Id.typ |- decomp_tfun |- fst)
    |> Option.default []
  in
  let t'' = make_app t' @@ List.map make_var ws in
  x, (env, (ys@zs@ws, t''))

let rec from_term cnt fun_env var_env val_env ce_set ce_env t : t list =
  let f = if !!debug then fun f -> print_begin_end f else (!!) in
  f (fun () ->
  let nid = Counter.gen cnt in
  match t.desc with
  | BinOp(And, _, _) -> assert false
  | BinOp(Or, _, _) -> assert false
  | Const _
  | Var _
  | BinOp _
  | Not _
  | Fun _
  | Event _
  | Bottom -> assert false(*[RT.leaf (Term t)]*)
  | _ when is_fail t -> [RT.leaf (nid, Fail)]
  | App({desc=Var f} as t1, ts) when Id.mem_assoc f val_env ->
      if !!debug then Format.printf "[APP1: %a@\n" Print.term t;
      let var_env',(ys,t_f) = Id.assoc f val_env in
      if ys = [] then
        (assert (Option.is_some @@ decomp_var t_f);
         assert false(*
         [RT.Node((depth, Term t), from_term fun_env var_env val_env ce_set ce_env @@ make_app t_f ts)]*))
      else
        let ys' = List.map Id.new_var_id ys in
        if !!debug then Format.printf "[APP1: %a, %d@\n" Print.term t_f (List.length ts);
        let t_f' = List.fold_right2 subst_var ys ys' t_f in
        let var_env'',arg_map = List.fold_left2 (fun (env,map) y t -> y::env, map@[y,env,t]) (var_env',[]) ys' ts in
        let val_env' = List.map (fun (y,env,t) -> normalize_val_env y env [] t) arg_map @ val_env in
        let arg_map' = List.map Triple.take13 arg_map in
        let fun_id =
          let kind =
            match get_id_option t1 with
            | None -> Local
            | Some tid -> Thread(tid, fst @@ List.assoc tid ce_env)
          in
          f, kind
        in
        [RT.Node((nid, App(fun_id, var_env', arg_map')), from_term cnt fun_env var_env'' val_env' ce_set ce_env t_f')]
  | App({desc=Var f}, ts) when Id.mem_assoc f fun_env ->
      if !!debug then Format.printf "[APP2,%a@\n" Id.print f;
      let ys,t_f = Id.assoc f fun_env in
      let var_env' = [] in
      assert (List.length ys = List.length ts);
      let tid = new_tid () in
      if !!debug then Format.printf "[tid: %d@\n" tid;
      let aux i path =
        let f' = Id.new_var_id f in
        let t_f' = Trans.alpha_rename t_f in
        let t_f'' = add_tid (f',tid) @@ subst_var f f' t_f' in
        let t' = make_app (add_id tid @@ make_var f') ts in
        let val_env' = normalize_val_env f' var_env' ys t_f''::val_env in
        let ce_env' = (tid,(i,path))::ce_env in
        from_term cnt fun_env var_env' val_env' ce_set ce_env' t'
      in
      let paths = List.assoc_all ~cmp:Id.eq f ce_set in
      let children = List.flatten_mapi aux paths in
      let tids =
        let get_thread (Rose_tree.Node((_,label),_)) =
          match label with
          | App((g, Thread(tid,_)), _, _) -> g, tid
          | _ -> assert false
        in
        List.map get_thread children
      in
      [RT.Node((nid, Spawn(f,tids)), children)]
  | App _ ->
      assert false
  | If(t1, t2, t3) ->
      let tid = get_tid t in
      let (i,ce),ce_env' = List.decomp_assoc tid ce_env in
      if !!debug then Format.printf "[IF: %a, %a@\n" (List.print Format.pp_print_int) ce Print.term t1;
      begin
        match ce with
        | [] -> []
        | br::ce' ->
            if !!debug then Format.printf "[CE[%d]: %a@\n" tid (List.print Format.pp_print_int) ce;
            let cond,t23 = if br = 0 then t1, t2 else make_not t1, t3 in
            let ce_env'' = (tid,(i,ce'))::ce_env' in
            if !!debug then Format.printf "[t23: %a@\n" Print.term t23;
            [RT.Node((nid, Assume cond), from_term cnt fun_env var_env val_env ce_set ce_env'' t23)]
      end
  | Let(flag, [f,xs,t1], t2) ->
      let val_env' = normalize_val_env f var_env xs t1::val_env in
      [RT.Node((nid, Let(f, var_env, make_funs xs t1)), from_term cnt fun_env var_env val_env' ce_set ce_env t2)]
  | _ ->
      if !!debug then Format.printf "%a@." Print.term t;
      unsupported "Comp_tree.from_term"
  )
let from_term fun_env ce_set t = from_term (Counter.create()) fun_env [] [] ce_set [] t



let from_program fun_env (ce_set: (id * int list) list) main =
  let xs = [Id.new_var ~name:"v0" TInt] in (**TODO**)
  let args = List.map make_var xs in
  make_app (make_var main) args
  |@> (fun t -> assert (not @@ is_fun_typ t.typ))
  |> from_term fun_env ce_set
  |> List.get
  |@> Format.printf "%a@.@." print
  |> Pair.pair xs
