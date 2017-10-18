open Util
open Syntax
open Term_util
open Type

type 'a r = S of 'a | N of int

let rec trans f i n t =
  let r =
    if i = n
    then f t
    else None
  in
  match r with
  | Some t' -> S t'
  | None ->
    match t.desc with
    | Const _
    | Var _
    | Event _
    | Record _
    | Field _
    | SetField _
    | Nil
    | Cons _
    | Constr _
    | Match _
    | Raise _
    | TryWith _
    | Bottom
    | BinOp _
    | Not _
    | Label _ -> N (i+1)
(*
    | Pair(t1,t2) ->
        begin
          match trans f (i+1) n t1 with
          | S t1' -> S (make_pair t1' t2)
          | N i' ->
              match trans f i' n t2 with
              | S t2' -> S (make_pair t1 t2')
              | N i'' -> N i''
        end
    | Fst t1 ->
        begin
          match trans f (i+1) n t1 with
          | S t1' -> S (make_fst t1')
          | N i' -> N i'
        end
    | Snd t1 ->
        begin
          match trans f (i+1) n t1 with
          | S t1' -> S (make_snd t1')
          | N i' -> N i'
        end
 *)
    | Fun(x,t1) ->
        begin
          match trans f (i+1) n t1 with
          | S t1' -> S (make_fun x t1')
          | N i' -> N i'
        end
    | App(t1,ts) ->
        let rec aux i ts =
          match ts with
          | [] -> N i
          | t::ts' ->
              match trans f i n t with
              | S t' -> S (t'::ts')
              | N i' ->
                  match aux i' ts' with
                  | S ts'' -> S (t::ts'')
                  | N i'' -> N i''
        in
        begin
          match aux (i+1) (t1::ts) with
          | S ts' -> S (make_app (List.hd ts') (List.tl ts'))
          | N i' -> N i'
        end
    | If(t1,t2,t3) ->
        begin
          match trans f (i+1) n t1 with
          | S t1' -> S (make_if t1' t2 t3)
          | N i' ->
              match trans f i' n t2 with
              | S t2' -> S (make_if t1 t2' t3)
              | N i'' ->
                  match trans f i'' n t3 with
                  | S t3' -> S (make_if t1 t2 t3')
                  | N i''' -> N i'''
        end
    | Let(defs, t) ->
        let rec aux i defs =
          match defs with
          | [] -> N i
          | (g,t)::defs' ->
              match trans f i n t with
              | S t' -> S ((g,t')::defs')
              | N i' ->
                  match aux i' defs' with
                  | S defs'' -> S ((g,t)::defs'')
                  | N i'' -> N i''
        in
        begin
          match aux (i+1) defs with
          | S defs' -> S (make_let defs' t)
          | N i' ->
              match trans f i' n t with
              | S t' -> S (make_let defs t')
              | N i'' -> N i''
        end
    | _ -> assert false

let trans f n t = trans f 0 n t

let trans1 n t =
  let f t = Some (make_term t.typ) in
  trans f n t

let trans2 n t =
  let f t =
    match t.desc with
    | If(t1,t2,t3) -> Some t2
    | _ -> None
  in
  trans f n t

let trans3 n t =
  let f t =
    match t.desc with
    | If(t1,t2,t3) -> Some t3
    | _ -> None
  in
  trans f n t

let trs = [trans3; trans2; trans1]

let compose tr1 tr2 i t =
  match tr2 i t with
  | N i' -> tr1 i' t
  | S t' -> tr1 i t'

let repeat_trial check t =
  Format.printf "%a@." Print.term t;
  let rec aux tr i t =
    match tr i t with
    | S t' when not (same_term t t') && check t' -> Format.printf"SLICED %d@." i;aux tr i t'
    | N i' when i' >= i -> t
    | _ -> aux tr (i+1) t
  in
  assert (check t);
  let t' = List.fold_left (fun t tr -> aux tr 0 t) t trs in
  assert (check t');
  Format.printf "%a@." Print.term t';
 t'
