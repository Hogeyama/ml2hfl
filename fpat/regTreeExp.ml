open Util
open Combinator

type t =
  Nil
| Var of Idnt.t
| Label of Idnt.t * t
| Concat of t * t
| Alter of t * t
| Kleene of t
| Option of t

let canonize t =
  let rec aux t =
    match t with
      Nil | Var(_) | Label(_, _) | Concat(_, _) | Kleene(_) | Option(_) -> [t]
      | Alter(t1, t2) ->
         let x = (aux t1) @ (aux t2) in
         let x' = List.unique x in
         (*let _ = assert (x = x') in*)
         x'
  in
  let xs = aux t in
  let xs = List.sort xs in
  match xs with
    [] -> failwith "canonize"
  | [x] -> x
  | x::xs -> List.fold_left (fun x y -> Alter(x, y)) x xs

let rec pr ppf t =
  match t with
    Nil ->
      Format.fprintf ppf "()"
  | Var(id) ->
      Format.fprintf ppf "%a" Idnt.pr id
  | Label(id, t) ->
      Format.fprintf ppf "%a[%a]" Idnt.pr id pr t
  | Concat(t1, t2) ->
      Format.fprintf ppf "(%a, %a)" pr t1 pr t2
  | Alter(t1, t2) ->
      Format.fprintf ppf "(%a | %a)" pr t1 pr t2
  | Kleene(t) ->
      Format.fprintf ppf "%a*" pr t
  | Option(t) ->
      Format.fprintf ppf "%a?" pr t

let gen_nt =
  let cnt = ref 0 in
  fun () ->
    cnt := !cnt + 1;
    "%N"^(string_of_int !cnt)

let rec elim_kleene t sub =
  match t with
    Nil ->
      Nil, sub
  | Var(id) ->
      Var(id), sub
  | Label(id, t) ->
      let t, sub = elim_kleene t sub in
      Label(id, t), sub
  | Concat(t1, t2) ->
      let t1, sub = elim_kleene t1 sub in
      let t2, sub = elim_kleene t2 sub in
      Concat(t1, t2), sub
  | Alter(t1, t2) ->
      let t1, sub = elim_kleene t1 sub in
      let t2, sub = elim_kleene t2 sub in
      Alter(t1, t2), sub
  | Kleene(t) ->
      let t, sub = elim_kleene t sub in
      (try
        Var(List.assoc t sub), sub
      with Not_found ->
        let id = Idnt.make (gen_nt ()) in
        Var(id), sub @ [t, id])
  | Option(t) ->
      let t, sub = elim_kleene t sub in
      Option(t), sub

let elim_kleene_env env =
  let sub, env =
    List.fold_left
      (fun (sub, env) (id, t) ->
        let t, sub = elim_kleene t sub in
        sub, env @ [id, t])
      ([], [])
      env in
  env @ (List.map (fun (t, id) -> id, Alter(Concat(t, Var(id)), Nil)) sub)

let rec elim_option t sub =
  match t with
    Nil ->
      Nil, sub
  | Var(id) ->
      Var(id), sub
  | Label(id, t) ->
      let t, sub = elim_option t sub in
      Label(id, t), sub
  | Concat(t1, t2) ->
      let t1, sub = elim_option t1 sub in
      let t2, sub = elim_option t2 sub in
      Concat(t1, t2), sub
  | Alter(t1, t2) ->
      let t1, sub = elim_option t1 sub in
      let t2, sub = elim_option t2 sub in
      Alter(t1, t2), sub
  | Kleene(t) ->
      let t, sub = elim_option t sub in
      Kleene(t), sub
  | Option(t) ->
      let t, sub = elim_option t sub in
      (try
        Var(List.assoc t sub), sub
      with Not_found ->
        let id = Idnt.make (gen_nt ()) in
        Var(id), sub @ [t, id])

let elim_option_env env =
  let sub, env =
    List.fold_left
      (fun (sub, env) (id, t) ->
        let t, sub = elim_option t sub in
        sub, env @ [id, t])
      ([], [])
      env in
  env @ (List.map (fun (t, id) -> id, Alter(t, Nil)) sub)



let add wl wl' t =
  let t = canonize t in
  match t with
    Var(id) ->
      id, wl
  | _ ->
     (try
         let id, _ = List.find (fun (id, t') -> t = t') (wl @ wl') in
         id, wl
       with Not_found ->
         let id = Idnt.make (gen_nt ()) in
         id, wl @ [id, t])

let rec to_ta env t wl wl' rho =
  match t with
    Nil ->
      TreeAutomaton.Leaf, wl, wl'
  | Var(id) ->
      if List.mem t rho then
        TreeAutomaton.Emp, wl, wl'
      else
        to_ta env (List.assoc id env) wl wl' (t::rho)
  | Label(id1, t) ->
      let id3, wl = add wl wl' Nil in
      let id2, wl = add wl wl' t in
      TreeAutomaton.Label(id1, id2, id3), wl, wl'
  | Concat(Nil, t) ->
      to_ta env t wl wl' rho
  | Concat(Var(id), t1) ->
      if List.mem t rho then
        TreeAutomaton.Emp, wl, wl'
      else
        to_ta env (Concat(List.assoc id env, t1)) wl wl' (t::rho)
  | Concat(Label(id1, t1), t2) ->
      let id3, wl = add wl wl' t2 in
      let id2, wl = add wl wl' t1 in
      TreeAutomaton.Label(id1, id2, id3), wl, wl'
  | Concat(Concat(t1, t2), t3) ->
      to_ta env (Concat(t1, Concat(t2, t3))) wl wl' rho
  | Concat(Alter(t1, t2), t3) ->
      let u1, wl, wl' = to_ta env (Concat(t1, t3)) wl wl' rho in
      let u2, wl, wl' = to_ta env (Concat(t2, t3)) wl wl' rho in
      TreeAutomaton.Alter(u1, u2), wl, wl'
  | Concat(Kleene(_), _) ->
      failwith ""
  | Alter(t1, t2) ->
      let u1, wl, wl' = to_ta env t1 wl wl' rho in
      let u2, wl, wl' = to_ta env t2 wl wl' rho in
      TreeAutomaton.Alter(u1, u2), wl, wl'
  | Kleene(t) ->
      failwith ""

let rec to_ta_env env wl wl' rs =
(*
  List.iter (fun (id, u) -> Format.printf "%s -> %a@," id TreeAutomaton.pr u) rs;
  List.iter (fun (id, t) -> Format.printf "%s -> %a@," id pr t) wl;
  Format.printf "@,";
*)
  match wl with
    [] -> rs
  | (id, t)::wl ->
      let u, wl, wl' = to_ta env t wl ((id, t)::wl') [] in
      to_ta_env env wl wl' (rs @ [id, u])

