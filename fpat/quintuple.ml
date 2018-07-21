open Util

(** Quintuples *)

let make x1 x2 x3 x4 x5 = (x1, x2, x3, x4, x5)

let of_list [x1; x2; x3; x4; x5] = (x1, x2, x3, x4, x5)
let list_of (x1, x2, x3, x4, x5) = [x1; x2; x3; x4; x5]

let fst (x1, x2, x3, x4, x5) = x1
let snd (x1, x2, x3, x4, x5) = x2
let trd (x1, x2, x3, x4, x5) = x3
let fth (x1, x2, x3, x4, x5) = x4
let fifth (x1, x2, x3, x4, x5) = x5

let get12 (x1, x2, x3, x4, x5) = (x1, x2)
let get13 (x1, x2, x3, x4, x5) = (x1, x3)
let get14 (x1, x2, x3, x4, x5) = (x1, x4)
let get15 (x1, x2, x3, x4, x5) = (x1, x5)
let get21 (x1, x2, x3, x4, x5) = (x2, x1)
let get23 (x1, x2, x3, x4, x5) = (x2, x3)
let get24 (x1, x2, x3, x4, x5) = (x2, x4)
let get25 (x1, x2, x3, x4, x5) = (x2, x5)
let get31 (x1, x2, x3, x4, x5) = (x3, x1)
let get32 (x1, x2, x3, x4, x5) = (x3, x2)
let get34 (x1, x2, x3, x4, x5) = (x3, x4)
let get35 (x1, x2, x3, x4, x5) = (x3, x5)
let get41 (x1, x2, x3, x4, x5) = (x4, x1)
let get42 (x1, x2, x3, x4, x5) = (x4, x2)
let get43 (x1, x2, x3, x4, x5) = (x4, x3)
let get45 (x1, x2, x3, x4, x5) = (x4, x5)
let get51 (x1, x2, x3, x4, x5) = (x5, x1)
let get52 (x1, x2, x3, x4, x5) = (x5, x2)
let get53 (x1, x2, x3, x4, x5) = (x5, x3)
let get54 (x1, x2, x3, x4, x5) = (x5, x4)

let get123 (x1, x2, x3, x4, x5) = (x1, x2, x3)
let get124 (x1, x2, x3, x4, x5) = (x1, x2, x4)
let get125 (x1, x2, x3, x4, x5) = (x1, x2, x5)
let get132 (x1, x2, x3, x4, x5) = (x1, x3, x2)
let get134 (x1, x2, x3, x4, x5) = (x1, x3, x4)
let get135 (x1, x2, x3, x4, x5) = (x1, x3, x5)
let get142 (x1, x2, x3, x4, x5) = (x1, x4, x2)
let get143 (x1, x2, x3, x4, x5) = (x1, x4, x3)
let get145 (x1, x2, x3, x4, x5) = (x1, x4, x5)
let get152 (x1, x2, x3, x4, x5) = (x1, x5, x2)
let get153 (x1, x2, x3, x4, x5) = (x1, x5, x3)
let get154 (x1, x2, x3, x4, x5) = (x1, x5, x4)
let get213 (x1, x2, x3, x4, x5) = (x2, x1, x3)
let get214 (x1, x2, x3, x4, x5) = (x2, x1, x4)
let get215 (x1, x2, x3, x4, x5) = (x2, x1, x5)
let get231 (x1, x2, x3, x4, x5) = (x2, x3, x1)
let get234 (x1, x2, x3, x4, x5) = (x2, x3, x4)
let get235 (x1, x2, x3, x4, x5) = (x2, x3, x5)
let get241 (x1, x2, x3, x4, x5) = (x2, x4, x1)
let get243 (x1, x2, x3, x4, x5) = (x2, x4, x3)
let get245 (x1, x2, x3, x4, x5) = (x2, x4, x5)
let get251 (x1, x2, x3, x4, x5) = (x2, x5, x1)
let get253 (x1, x2, x3, x4, x5) = (x2, x5, x3)
let get254 (x1, x2, x3, x4, x5) = (x2, x5, x4)
let get312 (x1, x2, x3, x4, x5) = (x3, x1, x2)
let get314 (x1, x2, x3, x4, x5) = (x3, x1, x4)
let get315 (x1, x2, x3, x4, x5) = (x3, x1, x5)
let get321 (x1, x2, x3, x4, x5) = (x3, x2, x1)
let get324 (x1, x2, x3, x4, x5) = (x3, x2, x4)
let get325 (x1, x2, x3, x4, x5) = (x3, x2, x5)
let get341 (x1, x2, x3, x4, x5) = (x3, x4, x1)
let get342 (x1, x2, x3, x4, x5) = (x3, x4, x2)
let get345 (x1, x2, x3, x4, x5) = (x3, x4, x5)
let get351 (x1, x2, x3, x4, x5) = (x3, x5, x1)
let get352 (x1, x2, x3, x4, x5) = (x3, x5, x2)
let get354 (x1, x2, x3, x4, x5) = (x3, x5, x4)
let get412 (x1, x2, x3, x4, x5) = (x4, x1, x2)
let get413 (x1, x2, x3, x4, x5) = (x4, x1, x3)
let get415 (x1, x2, x3, x4, x5) = (x4, x1, x5)
let get421 (x1, x2, x3, x4, x5) = (x4, x2, x1)
let get423 (x1, x2, x3, x4, x5) = (x4, x2, x3)
let get425 (x1, x2, x3, x4, x5) = (x4, x2, x5)
let get431 (x1, x2, x3, x4, x5) = (x4, x3, x1)
let get432 (x1, x2, x3, x4, x5) = (x4, x3, x2)
let get435 (x1, x2, x3, x4, x5) = (x4, x3, x5)
let get451 (x1, x2, x3, x4, x5) = (x4, x5, x1)
let get452 (x1, x2, x3, x4, x5) = (x4, x5, x2)
let get453 (x1, x2, x3, x4, x5) = (x4, x5, x3)
let get512 (x1, x2, x3, x4, x5) = (x5, x1, x2)
let get513 (x1, x2, x3, x4, x5) = (x5, x1, x3)
let get514 (x1, x2, x3, x4, x5) = (x5, x1, x4)
let get521 (x1, x2, x3, x4, x5) = (x5, x2, x1)
let get523 (x1, x2, x3, x4, x5) = (x5, x2, x3)
let get524 (x1, x2, x3, x4, x5) = (x5, x2, x4)
let get531 (x1, x2, x3, x4, x5) = (x5, x3, x1)
let get532 (x1, x2, x3, x4, x5) = (x5, x3, x2)
let get534 (x1, x2, x3, x4, x5) = (x5, x3, x4)
let get541 (x1, x2, x3, x4, x5) = (x5, x4, x1)
let get542 (x1, x2, x3, x4, x5) = (x5, x4, x2)
let get543 (x1, x2, x3, x4, x5) = (x5, x4, x3)

let map f1 f2 f3 f4 f5 (x1, x2, x3, x4, x5) =
  (f1 x1, f2 x2, f3 x3, f4 x4, f5 x5)
let map_fst f (x1, x2, x3, x4, x5) = (f x1, x2, x3, x4, x5)
let map_snd f (x1, x2, x3, x4, x5) = (x1, f x2, x3, x4, x5)
let map_trd f (x1, x2, x3, x4, x5) = (x1, x2, f x3, x4, x5)
let map_fth f (x1, x2, x3, x4, x5) = (x1, x2, x3, f x4, x5)
let map_fifth f (x1, x2, x3, x4, x5) = (x1, x2, x3, x4, f x5)

let fold f (x1, x2, x3, x4, x5) = f x1 x2 x3 x4 x5
let lift f (x1, x2, x3, x4, x5) = (f x1, f x2, f x3, f x4, f x5)
let unfold f1 f2 f3 f4 f5 x = (f1 x, f2 x, f3 x, f4 x)

let pr epr1 epr2 epr3 epr4 epr5 ppf (x1, x2, x3, x4, x5) =
  Format.fprintf ppf
    "(@[<hov>%a,@ %a,@ %a,@ %a,@ %a@])"
    epr1 x1 epr2 x2 epr3 x3 epr4 x4 epr5 x5
