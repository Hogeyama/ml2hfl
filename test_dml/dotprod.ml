let dotprod v1 v2 = begin
  let rec loop n sum i =
(*
    withtype int(n) -> int -> {i:nat | i <= n} int (i) -> int
*)
    if i = n then sum else loop n (sum + (v1.(i) * v2.(i) : int)) (i+1)
  in
    loop (Array.length v1) 0 0
end
(*
withtype int vect(n) -> int vect(n) -> int
*)


let dotprod v1 v2 = begin
  let sum = ref 0 in
    for i = 0 to pred (Array.length v1) do
      sum := v1.(i) * v2.(i) + !sum
    done;
    !sum
end
(*
withtype {n:nat} int vect(n) -> int vect(n) -> int
*)
