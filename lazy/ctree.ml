open ExtList
open ExtString

type s = Call of (Term.s * int) * Term.t | Arg of Term.t | Ret of Term.t | Error
type t = Node of int * Term.t * (s * t) list ref

type u = Term of Term.t | Cnode of bool * (Term.s * int) * u list

let gen =
  let cnt = ref 0 in
  fun () -> cnt := !cnt + 1; !cnt

let ret_args f uid arity = 
  Term.Var([], Term.T(f, uid, arity)),
  List.mapi (fun i _ -> Term.Var([], Term.T(f, uid, i))) (List.make arity ())

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
				              [Arg(Term.band (List.map (fun (farg, aarg) -> Term.eq farg aarg) faargs1)),
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
		            | Term.Ret(_, ret, t) ->
		                env, [Ret(Term.eq ret t), Node(gen (), ctx ret, ref [])]
            in
            let _ = cs := gns in
										  env, if breadth_first then wl @ (List.map snd gns) else (List.map snd gns) @ wl
          with Not_found ->
            env, wl))

let node_name uid t = (String.of_int uid) ^ ": " ^ Term.string_of t

let save_as_dot filename rt wl =
  let f s =
    match s with
      Call(_, t)
    | Arg(t)
    | Ret(t) -> Term.string_of t
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
        List.find (fun (Node(uid, t, _)) -> x = node_name uid t) wl; (x, "[style = dashed]")
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
    | Arg(t) ->
        Format.fprintf ppf "%a@," Term.pr t
    | Ret(t) -> 
        Format.fprintf ppf "%a@]]@," Term.pr t
    | Error ->
        Format.fprintf ppf "error"
  in
  Format.fprintf ppf "%a" (Util.pr_list pr "") p

let tree_of_error_path p =
  let rec f x ts trs p =
		  match p with
		    [] ->
        [], Cnode(true, x, Term(Term.band ts)::trs)
	   | s::p ->
        (match s with
				      Call(y, t) ->
				        let p', tr = f y [t] [] p in
            f x ts (tr::trs) p'
				    | Arg(t) ->
				        f x (t::ts) trs p
				    | Ret(t) -> 
				        p, Cnode(false, x, Term(Term.band (t::ts))::trs)
				    | Error ->
            let _ = assert (p = []) in
            p, Cnode(true, x, Term(Term.band ts)::trs))
  in
  let Call(x, t)::p = p in
  snd (f x [t] [] p)

let rec pr_tree ppf tr =
  match tr with
    Term(t) ->
      Format.fprintf ppf "%a" Term.pr t
  | Cnode(b, x, trs) ->
      if b then
        Format.fprintf ppf "{@[<v>%a@]}" (Util.pr_list pr_tree ", @,") trs
      else
        Format.fprintf ppf "[@[<v>%a@]]" (Util.pr_list pr_tree ", @,") trs

let rec leaf_of op tr =
  match tr with
    Term(_) ->
      raise Not_found
  | Cnode(op', x, trs) ->
      if op = op' && List.for_all (function (Term(_)) -> true | _ -> false) trs then
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
		    Term(_) ->
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

let rec term_of tr =
  match tr with
    Term(t) ->
      t
  | Cnode(_, _, trs) ->
      Term.band (List.map term_of trs)

let do_widen = false

let rec infer sums eptr =
(**)
  let _ = Format.printf "error path tree:@.  %a@." pr_tree eptr in
(**)
  try
				let ctx, tr = leaf_of true eptr in
		  let y, uid = match tr with Cnode(true, (y, uid), _) -> Format.printf "computing a precondition of <%a:%d>:@.  @[<v>" Term.pr_var y uid; y, uid in
				let t1 = term_of (ctx (Term(Term.make_true))) in
				let t2 = Term.bnot (term_of tr) in
				let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr t1 Term.pr t2 in
		  let interp = Term.interpolate t1 t2 in
				let _ = Format.printf "interp_out: %a@]@." Term.pr interp in
		  infer (`Pre((y, uid), interp)::sums) (ctx (Term(Term.bnot (interp))))
  with Not_found ->
    try
						let ctx, tr = leaf_of false eptr in
				  let y, uid = match tr with Cnode(false, (y, uid), _) -> Format.printf "computing a postcondition of <%a:%d>:@.  @[<v>" Term.pr_var y uid; y, uid in
						let t1 = term_of tr in

      let sub y uid x =
        match x with
		        Term.V(_) ->
		          Term.Var([], x)
		      | Term.T(y', uid', arg) ->
		          if y = y' && uid = uid' then
		            Term.Var([], Term.T(y', 0(*???*), arg))
		          else
		            Term.Var([], x)
      in
      let sub_inv y uid x =
        match x with
		        Term.V(_) ->
		          Term.Var([], x)
		      | Term.T(y', uid', arg) ->
		          if y = y' && uid' = 0 then
		            Term.Var([], Term.T(y', uid, arg))
		          else
		            Term.Var([], x)
      in
      let ts =
        (Term.subst (sub y uid) t1)::
        List.rev
          (List.map
            (fun (_, (Cnode(_, (y, uid), _) as tr)) -> Term.subst (sub y uid) (term_of tr))
            (List.filter
              (fun (_, Cnode(_, (x, _), _)) -> x = y)
              (callers_of uid eptr)))
      in
      let tts = snd (List.fold_left (fun (ts, tss) t -> t::ts, tss @ [t::ts]) ([], []) ts) in
      let t1' = Term.subst (sub_inv y uid) (Term.widen (List.map Term.bor tts)) in

						let t2 = Term.bnot (term_of (ctx (Term(Term.make_true)))) in
				  let interp =
        try
          if do_widen then
		    						let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr t1' Term.pr t2 in
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
				  infer (`Post((y, uid), interp)::sums) (ctx (Term(interp)))
    with Not_found ->
      sums
