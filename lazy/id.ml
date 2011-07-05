open ExtList

type t = string

let pr ppf id =
  Format.fprintf ppf "%s" id

let make id = id

let new_id =
  let cnt = ref 0 in
  fun () -> cnt := !cnt + 1; "$" ^ (string_of_int !cnt)
