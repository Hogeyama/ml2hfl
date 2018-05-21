open Util
open Combinator

(** Regular expressions *)

type 'a t =
  | EmptySet
  | Null
  | Token of 'a
  | Repeat of 'a t
  | Concatenation of 'a t * 'a t
  | Selection of 'a t * 'a t

let rec match_regex p xs =
  match p with
  | EmptySet -> false
  | Null -> xs = []
  | Token(y) -> xs = [y]
  | Repeat(q) ->
     let rec iter ys zs =
       if match_regex q ys then
         if zs = [] then
           true
         else
           if iter [] zs then
             true
           else
             iter (ys @ [List.hd zs]) (List.tl zs)
       else
         if zs = [] then
           false
         else
           iter (ys @ [List.hd zs]) (List.tl zs) in
     iter [] xs
  | Concatenation(q, r) ->
     let rec iter ys zs =
       if match_regex q ys && match_regex r zs then
         true
       else
         if zs = [] then
           false
         else
           iter (ys @ [List.hd zs]) (List.tl zs)
     in
     iter [] xs
  | Selection(q, r) ->
     match_regex q xs || match_regex r xs


(** @test simple *)
let test () =
  match_regex
    (Repeat(Selection(Concatenation(Token(1), Token(2)), Token(3))))
    [1; 2; 3; 2; 3; 3; 1; 2]
