open ExtList

(** Function definitions *)

(** args.length > 0,
    a.name=b.name=>a.args.length=b.args.length,
    guard is boolean expression without function calls *)
type t = { attr: Attr.t; name: Idnt.t; args: Idnt.t list; guard: Term.t; body: Term.t }

let pr ppf fdef =
  Format.fprintf ppf "%a %a | %a = %a"
    Idnt.pr fdef.name
    (Util.pr_list Idnt.pr " ") fdef.args
    Term.pr fdef.guard
    Term.pr fdef.body

(*
let set_arity am fdef =
  { fdef with body = Term.set_arity am fdef.body}
*)
