open Util
open Combinator

(** Games on infinite graphs *)

type t =
  | Safety of bool * (Pred.t * Pred.t) * bool * (Pred.t * Pred.t) option
  | Liveness of bool * (Pred.t * Pred.t) * bool * (Pred.t * Pred.t) option
  (** the first component is considered as the initial states
      @todo allow users to specify the initial states *)
  | WeakRecurrence of bool * (Idnt.t * (Pred.t * Pred.t)) list *
                      Idnt.t list * (Idnt.t * Idnt.t) list *
                      bool
  | AFMC of Formula.t

let infer = function
  | Safety(_, _, infer, _)
  | Liveness(_, _, infer, _) -> infer
  | WeakRecurrence(_, _, _, _, infer) -> infer
  | _ -> assert false
let is_angel = function
  | Safety(b, _, _, _)
  | Liveness(b, _, _, _)
  | WeakRecurrence(b, _, _, _, _) -> b
  | _ -> assert false
let is_demon = is_angel >> not
let is_safety = function
  | Safety(_, _, _, _) -> true
  | Liveness(_, _, _, _)
  | WeakRecurrence(_, _, _, _, _) -> false
  | _ -> assert false

let get_pred_must_win = function
  | Safety(_, (p, _), _, _)
  | Liveness(_, (p, _), _, _) -> p
  | WeakRecurrence(_, _, _, _, _) -> assert false
  | _ -> assert false

let get_pred_may_lose = function
  | Safety(_, (_, p), _, _)
  | Liveness(_, (_, p), _, _) -> p
  | WeakRecurrence(_, _, _, _, _) -> assert false
  | _ -> assert false

let get_comp_pred = function
  | Safety(_, _, _, pp)
  | Liveness(_, _, _, pp) -> pp
  | WeakRecurrence(_, _, _, _, _) -> assert false
  | _ -> assert false

let a_or_d b = if b then "angel" else "demon"

let pr_ord ppf (x1, x2) = Format.fprintf ppf "(%a <= %a)" Idnt.pr x1 Idnt.pr x2

let pr ppf = function
  | Safety(b, (pred, _), _, _) ->
    Format.fprintf ppf "@[<v>a safety game for %s with@," (a_or_d b);
    Format.fprintf ppf "the safety condition %a@]" Pred.pr pred
  | Liveness(b, (pred, _), _, _) ->
    Format.fprintf ppf "@[<v>a liveness game for %s with@," (a_or_d b);
    Format.fprintf ppf "the goal condition %a@]" Pred.pr pred
  | WeakRecurrence(b, penv, rset, ord, _) ->
    Format.fprintf ppf "@[<v>a weak recurrence game for %s:@," (a_or_d b);
    Format.fprintf ppf "  components: %a@,"
      PredSubst.pr (List.map (Pair.map_snd fst) penv);
    Format.fprintf ppf "  recurrence set: %a@," Idnt.pr_list rset;
    Format.fprintf ppf "  order: @[<v>%a@]@]" (List.pr pr_ord ",@ ") ord
  | AFMC(phi) ->
    Format.fprintf ppf "@[<v>an AFMC game with an objective:@,  %a@]" Formula.pr phi
