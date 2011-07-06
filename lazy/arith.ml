open ExtList
open ExtString

let mul m nxs = List.map (fun (n, x) -> m * n, x) nxs

let minus nxs = mul (-1) nxs

let canonize nxs =
  let res = Util.classify (fun (_, x1) (_, x2) -> Var.equiv x1 x2) nxs in
  List.map
    (function ((n, x)::nxs) ->
      (List.fold_left (fun n1 n2 -> n1 + n2) n (List.map fst nxs), x)
    | _ -> assert false)
    res

let coeff nxs x = Util.find_map (fun (n, y) -> if x = y then n else raise Not_found) nxs

let equiv nxs1 nxs2 =
  let nxs1 = canonize nxs1 in
  let nxs2 = canonize nxs2 in
  let xs1 = List.sort (List.map snd nxs1) in
  let xs2 = List.sort (List.map snd nxs2) in
  xs1 = xs2 &&
  (List.for_all (fun x -> coeff nxs1 x = coeff nxs2 x) xs1)
