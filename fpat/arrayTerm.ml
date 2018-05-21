open Util
open Combinator
open Term

(** Array term expressions *)


(** {6 Auxiliary constructors}*)

let mk_array l = mk_app (mk_const (Const.Array (List.length l))) l
let mk_aget a n = mk_app (mk_const Const.AGet) [a;  n]
let mk_aset a n m e = mk_app (mk_const Const.ASet) [a; n; m; e]

(** {6 Auxiliary destructors} *)

let let_array e f =
  match fun_args e with
  | Const(Const.Array n), ts ->
    let ts1, ts2 = List.split_nth n ts in
    f ts1 ts2
  | _ -> assert false

let let_aset e f =
  match fun_args e with
  | Const(Const.ASet), a::n::m::e::es -> f a n m e es
  | _ -> assert false

let let_aget e f = 
  match fun_args e with
  | Const(Const.AGet), a::n::es -> f a n es
  | _ -> assert false

