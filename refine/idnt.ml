open ExtList

(** Identifiers *)

type t = Id of string

let pr ppf (Id(id)) =
  Format.fprintf ppf "%s" id

let make id = Id(id)

let new_id =
  let cnt = ref 0 in
  fun () -> cnt := !cnt + 1; Id("x" ^ (string_of_int !cnt))

let string_of (Id(id)) = id
