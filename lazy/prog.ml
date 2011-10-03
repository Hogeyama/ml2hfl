open ExtList

type t = { attr: Attr.t; fdefs: Fdef.t list; types: (Idnt.t * SimType.t) list; main: Idnt.t }

let pr ppf prog =
  Format.fprintf ppf "@[<v>%a@]@." (Util.pr_list Fdef.pr "@ ") prog.fdefs

let fdefs_of prog id =
  let res = List.find_all (fun fdef -> fdef.Fdef.name = id) prog.fdefs in
  if res = [] then begin
    Format.printf "function \"%a\" not defined@." Idnt.pr id;
    assert false
  end else
    res

(* support type look-up for structured variables *)
let rec type_of prog x =
  match x with
    Var.V(id) ->
      (try List.assoc id prog.types with Not_found -> assert false)
  | Var.T(x, _, arg) ->
      let rec f ty i =
        let _ = assert (i >= 0) in
        match ty, i with
          SimType.Fun(ty, _), 0 -> ty
        | SimType.Fun(_, ty), _ -> f ty (i - 1)
        | SimType.Unit, 0 | SimType.Bool, 0 | SimType.Int, 0 -> ty
        | _, _ -> begin Format.printf "%a: %a, %d@." Var.pr x SimType.pr ty i; assert false end
      in
      f (type_of prog x) arg

(*
let arities prog x =
  List.length (List.find (fun fdef -> Var.V(fdef.Fdef.name) = x) prog.fdefs).Fdef.args

let set_arity am prog =
  { prog with fdefs = List.map (Fdef.set_arity am) prog.fdefs }
*)

(* x is a structured variable *)
let is_base prog x =
  match type_of prog x with
    SimType.Fun(_, _) -> false
  | _ -> true
