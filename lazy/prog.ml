open ExtList

type t = { attr: Attr.t; fdefs: Fdef.t list; types: (Id.t * Type.t) list; main: Id.t }

let pr ppf prog =
  Format.fprintf ppf "@[<v>%a@]@." (Util.pr_list Fdef.pr "@ ") prog.fdefs

let fdefs_of prog id =
  List.find_all (fun fdef -> fdef.Fdef.name = id) prog.fdefs

let rec type_of prog x =
  match x with
    Var.V(id) ->
      List.assoc id prog.types
  | Var.T(x, _, arg) ->
      let rec f ty i =
        let _ = assert (i >= 0) in
        match ty, i with
          Type.Fun(ty, _), 0 -> ty
        | Type.Fun(_, ty), _ -> f ty (i - 1)
        | _, _ -> assert false
      in
      f (type_of prog x) arg

(*
let arities prog x =
  List.length (List.find (fun fdef -> Var.V(fdef.Fdef.name) = x) prog.fdefs).Fdef.args

let set_arity am prog =
  { prog with fdefs = List.map (Fdef.set_arity am) prog.fdefs }
*)