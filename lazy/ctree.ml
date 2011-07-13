open ExtList
open ExtString

type s =
  Call of (Var.t * int) * Term.t
| Arg of (Var.t * Term.t) list
| Ret of Var.t * Term.t
| Error
type t = Node of int * Term.t * (s * t) list ref

let gen =
  let cnt = ref 0 in
  fun () -> cnt := !cnt + 1; !cnt

let ret_args f uid arity = 
  Term.make_var2 (Var.T(f, uid, arity)),
  List.mapi (fun i _ -> Term.make_var2 (Var.T(f, uid, i))) (List.make arity ())

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
				              Node(uid, ctx tt, ref [])]
		            | Term.Call(_, Term.Var(_, g), args) ->
                  (match g with
                    Var.V(f) ->
										            let fdefs = Prog.fdefs_of prog f in
																      env,
                      List.mapi
																        (fun i fd ->
										                let sub x = List.assoc x (List.combine (List.map (fun arg -> Var.V(arg)) fd.Fdef.args) args) in
																		  				  Call((g, uid), Term.subst sub fd.Fdef.guard),
																				  		  Node(gen (), ctx (Term.subst sub fd.Fdef.body), ref []))
										              fdefs
                  | Var.T(_, _, _) ->
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
