open Util
open Combinator

(** Natural numbers *)

let of_var_radix radices coeffs =
  coeffs
  |> List.mapi (fun i coeff ->
      radices
      |> List.drop (i + 1)
      |> Integer.prod_list
      |> ( * ) coeff)
  |> Integer.sum_list
