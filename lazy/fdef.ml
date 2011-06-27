open ExtList

(* args.length > 0, a.name=b.name=>a.args.length=b.args.length, guard is boolean expression without function calls *)
type 'a t = { attr: 'a; name: 'a Id.t; args: 'a Id.t list; guard: 'a Term.t; body: 'a Term.t }

let pr ppf fdef =
  Format.fprintf ppf "%a %a when %a = %a"
    Id.pr fdef.name
    (Util.pr_list Id.pr " ") fdef.args
    Term.pr fdef.guard
    Term.pr fdef.body
