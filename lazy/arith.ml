open ExtList
open ExtString

(** Linear arithmetic expressions *)

let pr ppf (nxs, n) =
  let _ =
    match nxs with
      (n, x)::nxs ->
        let _ =
          if n = 1 then
            Format.fprintf ppf "%a" Var.pr x
          else if n = -1 then
            Format.fprintf ppf "-%a" Var.pr x
          else
            Format.fprintf ppf "%d %a" n Var.pr x
        in
				    List.iter
				      (fun (n, x) ->
            if n > 0 then
  				        let _ = Format.fprintf ppf " + " in
              let _ = if n <> 1 then Format.fprintf ppf "%d " n in
              Format.fprintf ppf "%a" Var.pr x
            else if n < 0 then
  				        let _ = Format.fprintf ppf " - " in
              let _ = if -n <> 1 then Format.fprintf ppf "%d " (-n) in
              Format.fprintf ppf "%a" Var.pr x)
				      nxs
    | _ -> ()
  in
  if n > 0 then
    let _ = if nxs <> [] then Format.fprintf ppf " + " in
    Format.fprintf ppf "%d" n
  else if n < 0 then
    let _ = if nxs <> [] then Format.fprintf ppf " - " in
    Format.fprintf ppf "%d" (-n)

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


(** {6 Functions on linear atomic integer formulas} *)

(** let aif use only geq *)
let canonize_aif (c, nxs, n) =
  match c with
    Const.Leq ->
      minus nxs, -n
  | Const.Geq ->
      nxs, n
  | Const.Lt ->
      minus nxs, -(n + 1)
  | Const.Gt ->
      nxs, n - 1
  | _ -> assert false

let pr_caif ppf (nxs, n) =
  Format.fprintf ppf "%a >= 0" pr (nxs, n)
