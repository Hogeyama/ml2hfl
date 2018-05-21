open Combinator
open Util

(** Top-level ML expressions *)

type 'a t =
  | Let of Id.t * 'a MLExp.t
  | Type of Id.t * TypEnv.t

let aconv tls =
  List.map (function (Let(id, exp)) -> Let(id, MLExp.aconv [] [] exp) | x -> x) tls

let pr_elem ppf = function
  | Let(x, exp) ->
     Format.fprintf
       ppf
       "@[<2>let %s =@ %a@]"
       x
       MLExp.pr exp
  | Type(b, _) -> ()(* TODO *)

let pr ppf tls =
  Format.fprintf
    ppf
    "@[<v>%a@]"
    (Util.pr_list pr_elem "@ ") tls
