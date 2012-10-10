let rec make_term (u:unit) : (int list) -> int =

match Random.int 0 with
      0 ->
        let x = Random.int 0 in
          (function [] -> 0 | _ -> x)
    | 1 ->
        let x = Random.int 0 in
          (function [] -> 1 | _ -> x)
    | _ ->
        let t1 = make_term () in
          (function [] -> 2 | _::ns -> t1 ns)


let rec sumConstants f =
  match f [] with
      0 -> f [1]
    | 1 -> assert false
    | _ ->
        let f1 ns = f (1::ns) in
          sumConstants f1

let rec groundTerm f =
  match f [] with
      0 -> true, f
    | 1 -> false, f
    | i ->
        let f1 ns = f (1::ns) in
        let b1, f1' = groundTerm f1 in
        let f' ns =
          match ns with
              [] -> i
            | _::ns' -> f1' ns'
        in
          b1, f'

let main (m:unit) =
  let m = make_term () in
  let b,m' = groundTerm m in
    if b then sumConstants m' else 0
