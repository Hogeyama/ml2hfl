open ExtList

type t = string

let pr ppf id =
  Format.fprintf ppf "%s" id

let make id = id

let get id = id

let equal id1 id2 = id1 = id2
