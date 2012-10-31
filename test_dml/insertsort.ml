type order = LESS | EQUAL | GREATER

let sortRange(arr, start, n, cmp) =
  let item i = arr.(i) (*withtype {i:nat | i < size } int(i) -> 'a*) in
  let swap (i,j) =
    let tmp = item i in arr.(i) <- item j; arr.(j) <- tmp
(*
  withtype {i:nat}{j:nat | i < size /\ j < size } int(i) * int(j) -> unit
*)
  in
  let rec vecswap (i,j,n) = if n = 0 then () else begin swap(i,j); vecswap(i+1,j+1,n-1) end
(*
  withtype {i:nat}{j:nat}{n:nat | i+n <= size /\ j+n <= size } int(i) * int(j) * int(n) -> unit
*)
  in

  let insertSort (start, n) =
    let limit = start+n in
    let rec outer i =
      if i >= limit then ()
      else let rec inner j =
             if j <= start then outer(i+1)
             else let j' = j - 1 in
               match cmp(item j',item j) with
                 GREATER -> (swap(j,j'); inner j')
               | _  -> outer(i+1)
(*
           withtype {j:nat | j < size } int(j) -> unit
*)
      in inner i
(*
    withtype {i:nat} int(i) -> unit
*)
    in outer(start+1)
(*
  withtype {start:nat}{n:nat | start+n <= size } int(start) * int(n) -> unit
*)
  in
  insertSort (start, n)
(*
withtype {start:nat}{n:nat | start+n <= size }
         'a vect(size) * int(start) * int(n) * ('a * 'a -> order) -> unit
*)

let sorting arr cmp = sortRange(arr, 0, Array.length arr, cmp); arr
(*
withtype {size:nat} 'a vect(size) -> ('a * 'a -> order) -> 'a vect(size)
*)

(* sorted checks if a list is well-sorted *)
let sorted cmp arr =
  let len = Array.length arr in
  let rec s (v,i) =
    let v' = arr.(i) in
      match cmp(v,v') with
        GREATER -> false
      | _ -> if (i+1) = len then true else s(v',i+1)
(*
  withtype {i:nat | i < size } 'a * int(i) -> bool
*)
  in
  if len <= 1 then true else s(arr.(0),1)
(*
withtype ('a * 'a -> order) -> 'a vect(size) -> bool
*)
