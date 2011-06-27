open ExtList

type 'a t = { attr: 'a; id: string }

let pr ppf x =
  Format.fprintf ppf "%s" x.id

let make id = { attr = []; id = id }
