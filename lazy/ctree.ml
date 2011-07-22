open ExtList
open ExtString

type s =
  Call of (Var.t * int) * Term.t
| Arg of (Var.t * Term.t) list
| Ret of Var.t * Term.t
| Error
type t = Node of (int * int list) * Term.t * (s * t) list ref

type u = { is_end: unit -> bool; get: unit -> t list; next: unit -> t; update: t list -> unit }

let gen =
  let cnt = ref 0 in
  fun () -> cnt := !cnt + 1; !cnt

let ret_args f uid arity = 
  Term.make_var2 (Var.T(f, uid, arity)),
  List.mapi (fun i _ -> Term.make_var2 (Var.T(f, uid, i))) (List.make arity ())

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
  let rec traverse (s, l) (Node((uid, _), t, cs)) =
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
        List.find (fun (Node((uid, _), t, _)) -> x = node_name uid t) wl;
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

let bf_strategy rt =
  let wlr = ref [rt] in
  { is_end = (fun () -> !wlr = []);
    get = (fun () -> !wlr);
    next = (fun () -> match !wlr with [] -> raise Not_found | n::wl -> wlr := wl; n);
    update = (fun wl -> wlr := !wlr @ wl) }

let df_strategy rt =
  let wlr = ref [rt] in
  { is_end = (fun () -> !wlr = []);
    get = (fun () -> !wlr);
    next = (fun () -> match !wlr with [] -> raise Not_found | n::wl -> wlr := wl; n);
    update = (fun wl -> wlr := wl @ !wlr) }

let cex_strategy ces rt =
  let filt rts = List.filter (fun (Node((_, p), _, _)) -> List.exists (fun ce -> Util.prefix p ce) ces) rts in
  let wlr = ref (filt [rt]) in
  { is_end = (fun () -> !wlr = []);
    get = (fun () -> !wlr);
    next = (fun () -> match !wlr with [] -> raise Not_found | n::wl -> wlr := wl; n);
    update = (fun wl -> wlr := (filt wl) @ !wlr) }

let expand prog env (Node((uid, p), t, cs)) =
		let _ = assert (!cs = []) in
  try
    let ctx, red = Term.redex_of (Prog.type_of prog) t in
    let env, gns =
		    match red with
        Term.Const(_, Const.Event(id)) when id = "fail" ->
          env, [Error, Node((gen (), p), Term.Error([]), ref [])]
		    | Term.App(_, _, _) ->
          let uid = gen () in
		        let Term.Var(a, f), args = Term.fun_args red in
				      let ret, fargs = ret_args f uid (SimType.arity (Prog.type_of prog f)) in
				      let tt = Term.Ret([], ret, Term.Call([], Term.Var(a, f), fargs)) in
          let faargs =
            try
              List.combine fargs args
            with _ -> begin
              List.iter2
                (fun farg arg -> Format.printf "%a = %a@." Term.pr farg Term.pr arg) fargs args;
              assert false
            end
          in
				      let faargs1, faargs2 = List.partition
				        (fun (Term.Var(_, x), _) ->
				          match Prog.type_of prog x with
                SimType.Fun(_, _) -> false
              | _ -> true)
				        faargs
          in
(*
          let pr ppf (t1, t2) = Format.fprintf ppf "%a: %a" Term.pr t1 Term.pr t2 in
          let _ = Format.printf "faargs2: %a@." (Util.pr_list pr ", ") faargs2 in
*)
						    (fun x ->
            try
              Util.find_map
                (fun (Term.Var(_, y), aarg) ->
                  if Var.equiv x y then aarg else raise Not_found)
                faargs2
            with Not_found ->
              env x),
				      [Arg(List.map (fun (Term.Var(_, farg), aarg) -> farg, aarg) faargs1),
				      Node((uid, p), ctx tt, ref [])]
		    | Term.Call(_, Term.Var(_, g), args) ->
          (match g with
            Var.V(f) ->
										    let fdefs = Prog.fdefs_of prog f in
														env,
              List.mapi
																(fun i fd ->
                  let fargs = List.map (fun arg -> Var.V(arg)) fd.Fdef.args in
								          let faargs =
								            try
								              List.combine fargs args
								            with _ -> begin
								              List.iter2
								                (fun farg arg -> Format.printf "%a = %a@." Var.pr farg Term.pr arg) fargs args;
								              assert false
								            end
								          in
										        let sub x = List.assoc x faargs in
																		Call((g, uid), Term.subst sub fd.Fdef.guard),
																		Node((gen (), p @ [i]), ctx (Term.subst sub fd.Fdef.body), ref []))
										      fdefs
          | Var.T(_, _, _) ->
              let f = try env g with Not_found -> assert false in
              env, [Call((g, uid), Term.make_true), Node((gen (), p), ctx (Term.apply f args), ref [])])
		    | Term.Ret(_, Term.Var(a, ret), t) ->
		        env, [Ret(ret, t), Node((gen (), p), ctx (Term.Var(a, ret)), ref [])]
    in
    let _ = cs := gns in
				env, List.map snd gns
  with Not_found -> (*no redex found*)
    env, []

let rec manual prog rt strategy =
  let rec loop old_eps env =
		  if strategy.is_end () then
		    List.filter (fun p -> List.last p = Error) (paths_of rt)
		  else
		    let _ = save_as_dot "ctree.dot" rt (strategy.get ()) in
		    let eps = List.filter (fun p -> List.last p = Error) (paths_of rt) in
		    if old_eps = eps then
						    let env, wl = expand prog env (strategy.next ()) in
          let _ = strategy.update wl in
						    loop eps env
		    else
						  let _ =
						    Format.printf "error paths:@.";
						    List.iter (fun ep -> Format.printf "  %a@." pr_path ep) eps
						  in
						  let _ = Format.printf "expand the computation tree ? (y/n): %!" in
						  let inp = read_line () in
						  if inp = "y" then
						    let env, wl = expand prog env (strategy.next ()) in
          let _ = strategy.update wl in
						    loop eps env
						  else if inp = "n" then
		        eps
						  else
						    loop eps env
  in
  loop []
				(fun x ->
				  let _ = Format.printf "%a not found@." Var.pr x in
				  assert false)

let auto prog rt strategy =
  let rec loop env =
		  if strategy.is_end () then
		    List.filter (fun p -> List.last p = Error) (paths_of rt)
		  else
      let env, wl = expand prog env (strategy.next ()) in
      let _ = strategy.update wl in
				  loop env
  in
  loop
				(fun x ->
				  let _ = Format.printf "%a not found@." Var.pr x in
				  assert false)
