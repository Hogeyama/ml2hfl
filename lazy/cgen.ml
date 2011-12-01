open ExtList
open ExtString

type t = { name: Var.t * int; closed: bool; pre: Term.t; post: Term.t option; subst: (Var.t * Term.t * SimType.t) list list; ret: (Var.t * int) option (* for refinement type inference only *) }
type tree = Node of t * tree list
type path = Top | Path of path * tree list * t * tree list
type location = Loc of tree * path

let make name closed pre post subst =
  Node({ name = name; closed = closed; pre = pre; post = post; subst = subst; ret = None }, [])
let get (Node(nd, _)) = nd
let set (Node(_, trs)) nd = Node(nd, trs)

let zipper tr = Loc(tr, Top)
let up (Loc(tr, p)) =
  match p with
    Top -> raise Not_found
  | Path(up, trs1, nd, trs2) -> Loc(Node(nd, trs1 @ tr::trs2), up)
let down (Loc(tr, p)) x =
  match tr with
    Node(nd, trs) ->
      let trs1, tr', trs2 =
        try
          Util.split_with (fun tr -> (get tr).name = x) trs
        with Not_found ->
          assert false
      in
      Loc(tr', Path(p, trs1, nd, trs2))
let rec root (Loc(tr, p) as l) =
  match p with
    Top -> tr
  | _ -> root (up l)
let insert_down (Loc(Node(nd, trs), p)) tr =
  Loc(tr, Path(p, trs, nd, []))

let find_leaf tr =
  let rec aux (Loc(Node(nd, trs), p) as loc) =
    match List.rev trs with
      [] ->
        loc
    | tr'::trs' ->
        aux (Loc(tr', Path(p, List.rev trs', nd, [])))
  in
  aux (zipper tr)

(*
let rec up_until x_uid (Loc(tr, p)) =
  match p with
    Top -> assert false
  | Path(up, trs1, nd, trs2) ->
      if nd.name = x_uid then
        Loc(Node(nd, trs1 @ tr::trs2), up)
      else
        up_until x_uid (Loc(Node(nd, trs1 @ tr::trs2), up))
*)

let rec pr ppf tr =
  match tr with
    Node(nd, trs) ->
				  let _ =
						  Format.fprintf ppf "@[<v><%a:%d>"
						    Var.pr (fst nd.name)
						    (snd nd.name)
				  in
				  let _ =
								let _ = Format.fprintf ppf "@,  @[<v>" in
	  			  let _ = Format.fprintf ppf "%a" Term.pr nd.pre in
        let _ =
						    if nd.subst <> [] then
            let tmp = if List.length trs = List.length nd.subst then nd.subst else Util.init nd.subst in
            let _ =
				          List.iter2
				            (fun tr xttys ->
														    Format.fprintf ppf ", @,{%a}, @,%a"
		                  (Util.pr_list Term.pr ", @,") (List.map CompTree.eq_xtty xttys)
		                  pr tr)
				            trs
				            tmp
            in
            if List.length trs <> List.length nd.subst then
              let _ = assert nd.closed in
              Format.fprintf ppf ", @,%a"
                (Util.pr_list Term.pr ", @,") (List.map CompTree.eq_xtty (List.last nd.subst))
        in
	  			  let _ = match nd.post with None -> () | Some(t) -> Format.fprintf ppf ", @,%a" Term.pr t in
        Format.fprintf ppf "@]"
				  in
				  if nd.closed then
        let x, id =
								  match nd.ret with
		          None -> nd.name
		        | Some(x, id) -> x, id
        in
						  Format.fprintf ppf "@,</%a:%d>@]"
								  Var.pr x
								  id

exception FeasibleErrorTrace of tree

let rec path_set_open p =
		match p with
		  Top -> Top
		| Path(up, trs1, nd, trs2) ->
      Path(path_set_open up, trs1, { nd with closed = false }, trs2)





let rec ancestor_of (x, uid) (x', uid') =
  (x = x' && uid = uid') ||
  (match x' with
    Var.V(_) -> false
  | Var.T(x', uid', _) -> ancestor_of (x, uid) (x', uid'))

let term_of_nodes nds =
  List.map (fun nd -> match nd.post with None -> nd.pre | Some(t) -> Term.band [nd.pre; t]) nds,
  Util.concat_map (fun nd -> List.concat nd.subst) nds

let raw (ts, xttys) = Term.band (List.map CompTree.eq_xtty xttys @ ts)

(* elim as many variables as possible that do not satisfy p *)
let qelim p (ts, xttys) =
  let xttys1, xttys2 = List.partition (fun (x, _, _) -> p x) xttys in
  let sub x = List.assoc x (List.map (fun (x, t, _) -> x, t) xttys2) in
  Util.fixed_point
    (fun t ->
      (*Format.printf "%a@." Term.pr t;*)
      Term.subst sub t)
    (fun t1 t2 -> Term.equiv t1 t2)
    (Term.band (List.map CompTree.eq_xtty xttys1 @ ts))

let rec nodes_of_tree (Node(nd, trs)) =
  nd::Util.concat_map nodes_of_tree trs
let rec nodes_of_path p =
  match p with
    Top -> []
  | Path(up, trs1, nd, trs2) ->
      nodes_of_path up @ Util.concat_map nodes_of_tree trs1 @ [nd] @ Util.concat_map nodes_of_tree trs2

let rec terms_of_path p =
  match p with
    Top -> Term.ttrue, Term.ttrue
  | Path(up, trs1, nd, trs2) ->
      let t1, t2 = terms_of_path up in
      let xttyss1, xttyss2 = Util.split_at nd.subst (List.length trs1 + 1) in
      let xttys1 = List.concat xttyss1 in
      let xttys2 = List.concat xttyss2 in
      Term.band ([t1; raw (term_of_nodes (Util.concat_map nodes_of_tree trs1)); nd.pre] @ (List.map CompTree.eq_xtty xttys1)),
      Term.band ([t2; raw (term_of_nodes (Util.concat_map nodes_of_tree trs2))] @ (match nd.post with None -> [] | Some(t) -> [t]) @ (List.map CompTree.eq_xtty xttys2))

let rec rec_calls_of x (Loc(tr, p) as loc) =
  let trs, ps = 
		  try
				  rec_calls_of x (up loc)
		  with Not_found ->
		    [], []
  in
		if fst (get tr).name = x then
		  tr::trs, p::ps
		else
		  trs, ps
