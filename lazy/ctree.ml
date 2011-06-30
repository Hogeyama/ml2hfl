open ExtList

type t = Node of int list * Term.t * (Term.t * t) list ref

let ret_args f p arity = 
  Term.Var([Attr.Arg(0); Attr.Path(p)], Id.get f),
  List.mapi (fun i _ -> Term.Var([Attr.Arg(i + 1); Attr.Path(p)], Id.get f)) (List.make arity ())

let expand_tree prog wl =
  match wl with
    [] -> wl
  | n::wl ->
				  (match n with
				    Node(p, t, cs) ->
				      let _ = assert (!cs = []) in
          (try
            let ctx, red = Term.redex_of t in
            let gns =
		            match red with
                Term.Const(_, Const.Event(id)) when Id.equal id (Id.make "fail") ->
                  [Term.make_true, Node(p @ [0], Term.Error([]), ref [])]
		            | Term.App(_, _, _) ->
                  let p = p @ [0] in
		                let Term.Var(a, f), args = Term.fun_args red in
                  let arity = Attr.arity a in
                  let ret, fargs = ret_args f p arity in
                  let tt = Term.Ret([], ret, Term.Call([], Term.Var(a, f), fargs)) in
		                [Term.logand (List.map2 (fun farg arg -> Term.eq farg arg) fargs args),
                  Node(p, ctx tt, ref [])]
		            | Term.Call(_, Term.Var(_, f), args) ->
						            let fdefs = Prog.fdefs f prog in
												      List.mapi
												        (fun i fd ->
						                let sub x = List.assoc x (List.combine fd.Fdef.args args) in
														  				  Term.subst sub fd.Fdef.guard,
																  		  Node(p @ [i], ctx (Term.subst sub fd.Fdef.body), ref []))
						              fdefs
		            | Term.Ret(_, ret, t) ->
		                [Term.eq ret t, Node(p @ [0], ctx ret, ref [])]
            in
										  let _ = cs := gns in
										  (List.map snd gns) @ wl
          with Not_found ->
            wl))

let node_name p t = (List.fold_left (fun s i -> s ^ (if s = "" then "" else (*"."*)"") ^ string_of_int i) "" p) ^ ": " ^ Term.string_of t

let save_as_dot filename rt wl =
  let rec traverse (s, l) (Node(p, t, cs)) =
    let s' = node_name p t in
    (s, s', l)::
    (List.concat
      (List.map
        (fun (g, n) ->
          traverse
            (s', "[label = \"" ^ (Term.string_of g) ^ "\"]")
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
        List.find (fun (Node(p, t, _)) -> x = node_name p t) wl; (x, "[style = dashed]")
      with Not_found ->
        (x, ""))
    vs
  in
  Util.save_as_dot filename vs es
