open Util
open Combinator

(* key-value pair about pole information: idnt of pvar for the key, and bool for the value *)
type t = (Idnt.t * bool) list

let pr_pole ppf pole =
  let str = if pole then "minimize" else "Maximize" in
  Format.fprintf ppf "@[<v>%s@]" str

let pr_elem ppf (id, pole) =
  Format.fprintf ppf "@[<hov>%a : %a@]" Idnt.pr id pr_pole pole

let pr ppf li =
  Format.fprintf ppf "@[<hov>%a@]" (List.pr pr_elem "@,") li

let pole_of idnt poles =
  try List.assoc idnt poles with Not_found -> true
