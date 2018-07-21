open Util
open Combinator
open Term

(** Linear relations with rational coefficients *)

let of_formula = Formula.term_of >> fun_args >> function
  | Const(c), [t1; t2] when Const.is_ibrel c ->
    c, IntTerm.sub t1 t2 |> CunTerm.to_lin_rat_exp
  | _ -> invalid_arg "LinRationalRel.of_formula"

let lin_int_rel_of (c, (kxs, k)) =
  let lcm = Integer.lcm_list (snd k :: List.map (fst >> snd) kxs) in
  (Const.real_to_int c,
   (List.map (Pair.map_fst (fun k -> fst k * lcm / snd k)) kxs,
    fst k * lcm / snd k))
