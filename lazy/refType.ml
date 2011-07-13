open ExtList

(* return type must be base *)
type t = Unit | Bool | Int | Fun of (Term.t * t list * Term.t * t) list

let rec pr ppf rty =
  match rty with
    Unit ->
      Format.fprintf ppf "unit"
  | Bool ->
      Format.fprintf ppf "bool"
  | Int ->
      Format.fprintf ppf "int"
  | Fun(xs) ->
      let pr_aux ppf (pre, rtys, post, ty) =
				    (match pre with
          Term.Const(_, Const.True) ->
		          Format.fprintf ppf "(@[<hv>%a@] ->\ "
						        (Util.pr_list pr " *@ ") rtys
        | _ ->
		          Format.fprintf ppf "({@[<hov>@[<hv>%a@] |@ %a@]} ->\ "
						        (Util.pr_list pr " *@ ") rtys
						        Term.pr pre);
				    (match post with
          Term.Const(_, Const.True) ->
		          Format.fprintf ppf "%a)"
						        pr ty
        | _ ->
				        Format.fprintf ppf "{@[<hov>%a |@ %a@]})"
								      pr ty
								      Term.pr post)
      in
      let _ = assert (xs <> []) in
      Format.fprintf ppf "@[<hv>%a@]" (Util.pr_list pr_aux " /\\@ ") xs

let rec of_type ty =
		match ty with
		  Type.Unit -> Unit
		| Type.Bool -> Bool
		| Type.Int -> Int
		| Type.Fun(_, _) ->
	     let args, ret = Type.args_ret ty in
      Fun([Term.make_true, List.map of_type args, Term.make_true, of_type ret])

let of_summaries types sums =
  let rec refine (x, uid) ty =
		  let merge x ty =
				  match ty with
				    Type.Unit -> Unit
				  | Type.Bool -> Bool
				  | Type.Int -> Int
				  | Type.Fun(_, _) ->
								  let tys = List.map
								    (fun uid -> refine (x, uid) ty)
								    (List.unique
              (List.filter_map
                (function `Pre((y, uid), _) | `Post((y, uid), _) ->
                  if x = y then Some(uid) else None | _ -> None)
                sums))
		        in
          if tys = [] then
  		        of_type ty
          else
  		        Fun(Util.concat_map (function Fun(xs) -> xs | _ -> assert false) tys)
		  in
    let sums = List.filter (function `Pre((x', uid'), _) | `Post((x', uid'), _) -> x = x' && uid = uid') sums in
		  match ty with
		    Type.Unit -> Unit
		  | Type.Bool -> Bool
		  | Type.Int -> Int
		  | Type.Fun(_, _) ->
        try(*???*)
		        let args, ret = Type.args_ret ty in
				      Fun(List.map
				        (function
              `Pre((_, uid), pre) ->
						          pre,
                List.mapi (fun i arg -> merge (Var.T(x, uid, i)) arg) args,
						          Term.make_true,
                merge (Var.T(x, uid, List.length args)) ret
				        | `Post((_, uid), post) ->
						          Term.make_true,
                List.mapi (fun i arg -> merge (Var.T(x, uid, i)) arg) args,
						          post,
                merge (Var.T(x, uid, List.length args)) ret
            | _ -> assert false)
				        sums)
        with Not_found ->
          assert false (*Fun([])*)
  in
  List.map
    (fun (f, uid) ->
      (f, uid), refine (Var.V(f), uid) (List.assoc f types))
    (List.unique (List.filter_map (function `Pre((Var.V(f), uid), _) | `Post((Var.V(f), uid), _) -> Some(f, uid) | _ -> None) sums))
