open ExtList
open ExtString

type s =
  Call of (Term.s * int) * Term.t
| Arg of (Term.s * Term.t) list
| Ret of Term.s * Term.t
| Error
type t = Node of int * Term.t * (s * t) list ref

type u =
  Sub of (Term.s * Term.t) list
| Guard of Term.t
| Cnode of bool * (Term.s * int) * u list
| Nop

let gen =
  let cnt = ref 0 in
  fun () -> cnt := !cnt + 1; !cnt

let ret_args f uid arity = 
  Term.make_var2 (Term.T(f, uid, arity)),
  List.mapi (fun i _ -> Term.make_var2 (Term.T(f, uid, i))) (List.make arity ())

let breadth_first = true

let expand_tree prog env wl =
  match wl with
    [] -> env, wl
  | n::wl ->
				  (match n with
				    Node(uid, t, cs) ->
				      let _ = assert (!cs = []) in
          (try
            let ctx, red = Term.redex_of (Prog.type_of prog) t in
            let env, gns =
		            match red with
                Term.Const(_, Const.Event(id)) when id = "fail" ->
                  env, [Error, Node(gen (), Term.Error([]), ref [])]
		            | Term.App(_, _, _) ->
                  let uid = gen () in
		                let Term.Var(a, f), args = Term.fun_args red in
				              let ret, fargs = ret_args f uid (Type.arity (Prog.type_of prog f)) in
				              let tt = Term.Ret([], ret, Term.Call([], Term.Var(a, f), fargs)) in
				              let faargs1, faargs2 = List.partition
				                (fun (Term.Var(_, x), _) ->
				                  match Prog.type_of prog x with
                        Type.Fun(_, _) -> false
                      | _ -> true)
				                (List.combine fargs args)
                  in
						            (fun x ->
                    try
                      env x
                    with Not_found ->
                      try
                        Util.find_map
                          (fun (Term.Var(_, y), aarg) ->
                            if x = y then aarg else raise Not_found)
                          faargs2
                      with Not_found ->
                        assert false),
				              [Arg(List.map (fun (Term.Var(_, farg), aarg) -> farg, aarg) faargs1),
				              Node(uid, ctx tt, ref [])]
		            | Term.Call(_, Term.Var(_, g), args) ->
                  (match g with
                    Term.V(f) ->
										            let fdefs = Prog.fdefs_of prog f in
																      env,
                      List.mapi
																        (fun i fd ->
										                let sub x = List.assoc x (List.combine (List.map (fun arg -> Term.V(arg)) fd.Fdef.args) args) in
																		  				  Call((g, uid), Term.subst sub fd.Fdef.guard),
																				  		  Node(gen (), ctx (Term.subst sub fd.Fdef.body), ref []))
										              fdefs
                  | Term.T(_, _, _) ->
                      let f = try env g with Not_found -> assert false in
                      env, [Call((g, uid), Term.make_true), Node(gen (), ctx (Term.apply f args), ref [])])
		            | Term.Ret(_, Term.Var(a, ret), t) ->
		                env, [Ret(ret, t), Node(gen (), ctx (Term.Var(a, ret)), ref [])]
            in
            let _ = cs := gns in
										  env, if breadth_first then wl @ (List.map snd gns) else (List.map snd gns) @ wl
          with Not_found ->
            env, wl))

let node_name uid t = (String.of_int uid) ^ ": " ^ Term.string_of t

let eq_xts xts = 
  List.map (fun (x, t) -> Term.eq (Term.make_var2 x) t) xts

let save_as_dot filename rt wl =
  let f s =
    match s with
      Call(_, t) -> Term.string_of t
    | Arg(xts) -> Term.string_of (Term.band (eq_xts xts))
    | Ret(x, t) -> Term.string_of (Term.eq (Term.make_var2 x) t)
    | Error -> ""
  in
  let rec traverse (s, l) (Node(uid, t, cs)) =
    let s' = node_name uid t in
    (s, s', l)::
    (List.concat
      (List.map
        (fun (g, n) ->
          traverse
            (s', "[label = \"" ^ (f g) ^ "\"]")
            n)
        !cs))
  in
  let es = List.unique (traverse ("", "") rt) in
  let vs = List.unique (List.concat (List.map (fun (x, y, _) -> [x, ""; y, ""]) es)) in
  let es = List.filter (fun (x, _, _) -> x <> "") es in
  let vs = List.filter (fun (x, _) -> x <> "") vs in
  let vs = List.map
    (fun (x, _) ->
      try
        List.find (fun (Node(uid, t, _)) -> x = node_name uid t) wl;
        (x, "[style = dashed]")
      with Not_found ->
        (x, ""))
    vs
  in
  Util.save_as_dot filename vs es

let rec paths_of (Node(_, _, cs)) =
  Util.concat_map (fun (g, n) -> let ps = paths_of n in if ps = [] then [[g]] else List.map (fun p -> g::p) ps) !cs

let rec pr_path ppf p =
  let pr ppf s =
    match s with
      Call(_, t) ->
        Format.fprintf ppf "[@[<hov>%a.@," Term.pr t
    | Arg(xts) ->
        Format.fprintf ppf "%a@," Term.pr (Term.band (eq_xts xts))
    | Ret(x, t) -> 
        Format.fprintf ppf "%a@]]@," Term.pr (Term.eq (Term.make_var2 x) t)
    | Error ->
        Format.fprintf ppf "error"
  in
  Format.fprintf ppf "%a" (Util.pr_list pr "") p

let tree_of_error_path p =
  let rec f x g xts trs p =
		  match p with
		    [] ->
        [], Cnode(true, x, Guard(g)::Sub(xts)::trs)
	   | s::p ->
        (match s with
				      Call(y, g') ->
				        let p', tr = f y g' [] [] p in
            f x g xts (tr::trs) p'
				    | Arg(xts') ->
				        f x g (xts' @ xts) trs p
				    | Ret(y, t) -> 
				        p, Cnode(false, x, Guard(g)::Sub((y, t)::xts)::trs)
				    | Error ->
            let _ = assert (p = []) in
            p, Cnode(true, x, Guard(g)::Sub(xts)::trs))
  in
  let Call(x, t)::p = p in
  snd (f x t [] [] p)

let rec pr_tree ppf tr =
  match tr with
    Sub(xts) ->
      Format.fprintf ppf "%a" Term.pr (Term.band (eq_xts xts))
  | Guard(t) ->
      Format.fprintf ppf "%a" Term.pr t
  | Cnode(b, x, trs) ->
      if b then
        Format.fprintf ppf "{@[<v>%a@]}" (Util.pr_list pr_tree ", @,") trs
      else
        Format.fprintf ppf "[@[<v>%a@]]" (Util.pr_list pr_tree ", @,") trs
  | Nop ->
      Format.fprintf ppf "nop"

let rec leaf_of op tr =
  match tr with
    Sub(_)
  | Guard(_)
  | Nop ->
      raise Not_found
  | Cnode(op', x, trs) ->
      if op = op' && List.for_all (function Sub(_) | Guard(_) -> true | _ -> false) trs then
        (fun tr -> tr), tr
      else
        let res = Util.partition_map
          (fun tr -> try `L(leaf_of op tr) with Not_found -> `R(tr))
          trs
        in
        (match res with
          [], _ ->
            raise Not_found
(* more than two closed trees may be returned *)
        | (ctx, tr)::zz, trs ->
            (fun tr -> Cnode(op', x, (List.map (fun (ctx, tr) -> ctx tr) zz) @ (ctx tr)::trs)),
            tr)

let rec callers_of uid tr =
		let rec f ctx tr =
		  match tr with
		    Sub(_)
		  | Guard(_)
    | Nop ->
		      raise Not_found
		  | Cnode(op, (x, uid'), trs) ->
		      if uid = uid' then
		        []
		      else
		        (ctx, tr)::
		        (Util.find_map
		          (fun (ctx', tr) -> f (fun tr -> ctx (Cnode(op, (x, uid'), ctx' tr))) tr)
		          (Util.ctx_elem trs))
  in
  f (fun tr -> tr) tr

let rec term_of (x, uid) tr =
  let rec f tr =
		  match tr with
		    Sub(xts) ->
		      xts, []
		  | Guard(t) ->
		      [], [t]
		  | Cnode(_, _, trs) ->
		      let xtss, tss = List.split (List.map f trs) in
        List.concat xtss, List.concat tss
		  | Nop ->
		      [], []
  in
  let xts, ts = f tr in
  let xts1, xts2 = List.partition (function (Term.T(x', uid', _), _) -> x = x' && uid = uid' | _ -> false) xts in
  let t = Term.band (eq_xts xts1 @ ts) in
  let sub x = List.assoc x xts2 in
  Util.fixed_point (Term.subst sub) (fun t1 t2 -> Term.equiv t1 t2) t

let do_widen = true

let rec infer sums eptr =
(**)
  let _ = Format.printf "error path tree:@.  %a@." pr_tree eptr in
(**)
  try
				let ctx, tr = leaf_of true eptr in
		  let y, uid = match tr with Cnode(true, (y, uid), _) -> Format.printf "computing a precondition of <%a:%d>:@.  @[<v>" Term.pr_var y uid; y, uid in
				let t1 = term_of (y, uid) (ctx Nop) in
				let t2 = term_of (y, uid) tr in
				let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr t1 Term.pr t2 in
		  let interp = Term.interpolate t1 t2 in
				let _ = Format.printf "interp_out: %a@]@." Term.pr interp in
		  infer (`Pre((y, uid), interp)::sums) (ctx (Guard(Term.bnot (interp))))
  with Not_found ->
    try
						let ctx, tr = leaf_of false eptr in
				  let y, uid = match tr with Cnode(false, (y, uid), _) -> Format.printf "computing a postcondition of <%a:%d>:@.  @[<v>" Term.pr_var y uid; y, uid in

      let sub y uid x =
        match x with
		        Term.V(_) ->
		          Term.make_var2 x
		      | Term.T(y', uid', arg) ->
		          if y = y' && uid = uid' then
		            Term.make_var2 (Term.T(y', 0(*???*), arg))
		          else
		            Term.make_var2 x
      in
      let sub_inv y uid x =
        match x with
		        Term.V(_) ->
		          Term.make_var2 x
		      | Term.T(y', uid', arg) ->
		          if y = y' && uid' = 0 then
		            Term.make_var2 (Term.T(y', uid, arg))
		          else
		            Term.make_var2 x
      in

						let t1 = term_of (y, uid) tr in
						let t2 = term_of (y, uid) (ctx Nop) in
      let tts =
        (Term.subst (sub y uid) t1, Term.subst (sub y uid) t2)::
        List.rev
          (List.map
            (fun (ctx, (Cnode(_, (y, uid), _) as tr)) ->
              Term.subst (sub y uid) (term_of (y, uid) tr),
              Term.subst (sub y uid) (term_of (y, uid) (ctx Nop)))
            (List.filter
              (fun (_, Cnode(_, (x, _), _)) -> x = y)
              (callers_of uid eptr)))
      in
      let _, _, tss1, tss2 = List.fold_left
        (fun (ts1, ts2, tss1, tss2) (t1, t2) ->
          t1::ts1, t2::ts2, tss1 @ [t1::ts1], tss2 @ [t2::ts2])
        ([], [], [], [])
        tts in
      let t1' = Term.subst (sub_inv y uid) (Term.widen (List.map Term.bor tss1)) in
      let t2' = Term.subst (sub_inv y uid) (Term.widen (List.map Term.bor tss2)) in

				  let interp =
        try
          if do_widen then
            try
				    						let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr t1' Term.pr t2' in
				          Term.interpolate t1' t2'
            with Term.No_interpolant ->
              if Term.equiv t2 t2' then
                raise Term.No_interpolant
              else
                Term.interpolate t1' t2
          else
            raise Term.No_interpolant
        with Term.No_interpolant ->
          if do_widen && Term.equiv t1 t1' then
            raise Term.No_interpolant
          else
      						let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr t1 Term.pr t2 in
            Term.interpolate t1 t2
      in
						let _ = Format.printf "interp_out: %a@]@." Term.pr interp in
				  infer (`Post((y, uid), interp)::sums) (ctx (Guard(interp)))
    with Not_found ->
      sums
