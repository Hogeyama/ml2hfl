open ExtList

type 'a t = { attr: 'a; fdefs: 'a Fdef.t list; main: 'a Id.t }

let pr ppf prog =
  List.iter (fun fdef -> Format.fprintf ppf "%a@." Fdef.pr fdef) prog.fdefs

let get_fdefs id prog =
  List.find_all (fun fdef -> fdef.Fdef.name.Id.id = id) prog.fdefs

let get_args id prog =
  let fdef = List.find (fun fdef -> fdef.Fdef.name.Id.id = id) prog.fdefs in
  fdef.Fdef.args
