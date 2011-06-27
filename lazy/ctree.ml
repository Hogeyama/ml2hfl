open ExtList

type 'a t = Node of 'a Term.t * ('a Term.t * 'a t) list ref

let expand_tree prog wl =
  match wl with
    [] -> wl
  | n::wl ->
				  (match n with
				    Node(t, cs) ->
				      let _ = assert (!cs = []) in
				      (match t with
				        Term.Var(_, _)
				      | Term.Event(_, _)
								  | Term.True(_)
								  | Term.False(_)
								  | Term.And(_, _, _)
								  | Term.Or(_, _, _)
								  | Term.Not(_, _)
								  | Term.Lt(_, _, _)
								  | Term.Eq(_, _, _)
								  | Term.Int(_, _)
								  | Term.Add(_, _, _)
								  | Term.Mul(_, _, _)
								  | Term.Minus(_, _) -> wl
				      | Term.App(_, _, _) -> begin
				          let f, args = Term.fun_args t in
				          (match f with
				            Term.Var(_, f) ->
				              let fdefs = Prog.get_fdefs f.Id.id prog in
				              let gns =
						              List.map
						                (fun fd ->
				                    let args1, args2 = Util.split_at args (List.length fd.Fdef.args) in
						                  fd.Fdef.guard,
						                  Node(Term.apply (Term.subst (List.combine (List.map (fun a -> a.Id.id) fd.Fdef.args) args1) fd.Fdef.body) args2, ref []))
						                fdefs
				              in
				              cs := gns;
				              (List.map snd gns) @ wl)
				        end))

let save_as_dot filename rt wl =
  let rec traverse (s, l) (Node(t, cs)) =
    let s' = Term.string_of t in
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
  let vs = List.map (fun (x, _) -> try List.find (fun (Node(t, _)) -> x = Term.string_of t) wl; (x, "[style = dashed]") with Not_found -> (x, "")) vs in
  Util.save_as_dot filename vs es
