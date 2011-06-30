open ExtList

type t = { attr: Attr.t; fdefs: Fdef.t list; main: Id.t }

let pr ppf prog =
  List.iter (fun fdef -> Format.fprintf ppf "%a@." Fdef.pr fdef) prog.fdefs

let fdefs id prog =
  List.find_all (fun fdef -> Id.equal fdef.Fdef.name id) prog.fdefs

let args id prog =
  let fdef = List.find (fun fdef -> Id.equal fdef.Fdef.name id) prog.fdefs in
  fdef.Fdef.args

let arities prog id = List.length (List.find (fun fdef -> Id.equal fdef.Fdef.name id) prog.fdefs).Fdef.args

let set_arity am prog =
  { prog with fdefs = List.map (Fdef.set_arity am) prog.fdefs }
