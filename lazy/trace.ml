open ExtList
open ExtString

(** Traces *)

(** Elements of traces *)
type s =
  Call of (Var.t * int) * Term.t(*Guard*)
| Arg of (Var.t * Term.t * SimType.t) list
| Ret of Var.t * Term.t * SimType.t
| Nop
| Error

(** Trace expressions *)
type t = Elm of s | Seq of t list | Br of t * t

(** {6 Basic functions} *)

let pr_elem ppf elm =
  match elm with
    Call(_, t) ->
      Format.fprintf ppf "[@[<hov>%a.@," Term.pr t
  | Arg(xttys) ->
      Format.fprintf ppf "%a@," Term.pr (Formula.band (List.map Formula.eq_xtty xttys))
  | Ret(x, t, ty) -> 
      Format.fprintf ppf "%a@]]@," Term.pr (Formula.eq_xtty (x, t, ty))
  | Nop ->
      Format.fprintf ppf "nop"
  | Error ->
      Format.fprintf ppf "error"

let pr ppf trace =
  Format.fprintf ppf "%a" (Util.pr_list pr_elem "") trace

let rec pr_exp ppf trexp =
  match trexp with
  | Elm(elm) ->
      Format.fprintf ppf "%a" pr_elem elm
  | Seq(trexps) ->
      Format.fprintf ppf "%a" (Util.pr_list pr_exp "") trexps
  | Br(trexp1, trexp2) ->
      Format.fprintf ppf "(%a | %a)" pr_exp trexp1 pr_exp trexp2

let rec function_calls_of tr =
  match tr with
    [] ->
      []
  | s::tr' ->
      (match s with
        Call(x, _) ->
          x::function_calls_of tr'
      | _ -> function_calls_of tr')

let trace_exp_of tr =
  Seq(List.map (fun elm -> Elm(elm)) tr)
