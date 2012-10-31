(*
datatype 'a list =
  Nil(0) | {n:nat} Cons(n+1) of 'a * 'a list(n)
*)
type list = Nil | Cons of int * list

(*
datatype 'a listlist =
  Listnil(0) | {m:nat}{n:nat} Listcons(m+n) of 'a list(m) * 'a listlist(n)
*)
type listlist = Listnil | Listcons of list * listlist

let rec merge order l1 l2 =
  match l1, l2 with
    Nil, l2 -> l2
  | l1, Nil -> l1
  | (Cons(h1, t1) as l1), (Cons(h2, t2) as l2) ->
      if order h1 h2 then Cons(h1, merge order t1 l2) else Cons(h2, merge order l1 t2)
(*
withtype ('a -> 'a -> bool) -> {m:nat}{n:nat} 'a list(m) -> 'a list(n) -> 'a list(m+n)
*)

(*
This style is not working yet:

let merge order =
  merge_rec where rec merge_rec = fun
    Nil l2 -> l2
  | l1 Nil -> l1
  | (cons(h1, t1) as l1) (cons(h2, t2) as l2) ->
      if order h1 h2 then cons(h1, merge_rec t1 l2) else cons(h2, merge_rec l1 t2)
  withtype {m:nat}{n:nat} 'a list(m) -> 'a list(n) -> 'a list(m+n)
;;
*)


let sorting order l =
  let rec initlist = function
      Nil -> Listnil
    | Cons(_, Nil) as singleton -> Listcons(singleton, Listnil)
    | Cons(e1, Cons(e2, rest)) -> (* Notice the type annotation below: a real lesson *)
        Listcons((if order e1 e2 then Cons(e1, Cons(e2, Nil)) else Cons(e2, Cons(e1, Nil))(* : 'a list(2)*)),
                 initlist rest)
(*
  withtype {n:nat} 'a list(n) -> 'a listlist(n)
*)
  in
  let rec merge2 = function
      Listcons(l1, Listcons(l2, rest)) -> Listcons(merge order l1 l2, merge2 rest)
    | Listcons(_, Listnil) as x -> x
    | Listnil as x -> x
(*
  withtype {n:nat} 'a listlist(n) -> 'a listlist(n)
*)
  in
  let rec mergeall = function
      Listnil -> Nil
    | Listcons(l, Listnil) -> l
    | Listcons(_, Listcons(_, _)) as llist -> mergeall (merge2 llist)
(*
  withtype {n:nat} 'a listlist(n) -> 'a list(n)
*)
  in
  mergeall(initlist l)
(*
withtype ('a -> 'a -> bool) -> {n:nat} 'a list(n) -> 'a list(n)
*)
