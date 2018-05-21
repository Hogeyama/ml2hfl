open Util

(** Equation theories *)

let pr_elem ppf (t1, t2) =
  Format.fprintf ppf "@[<hov2>%a =@ %a@]" Term.pr t1 Term.pr t2
let pr ppf = Format.fprintf ppf "@[<v>%a@]" (List.pr pr_elem "@,")
