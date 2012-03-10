(* verification failed *)
(** explicit parameter version **)

let rec make (n:int) = fun i -> if i < n then 1 else 0

let rec tail (n:int) s = fun i -> s (i + 1)

let rec concat (n1:int) s1 (n2:int) s2 =
  if s1 0 = 0 then
    g
  else
    fun i ->
      if i = 0 then
        s1 0
      else
        concat (n1 - 1) (tail n1 s1) n2 s2 (i - 1)

let main (n1:int) n2 =
  if n1 >= 0 && n2 >= 0 then
		  let s1 = mkstr n1 in
		  let s2 = mkstr n2 in
		  let s = concat n1 s1 n2 s2 in
		  assert (s (n1 + n2) = 0)

(** types **
make: n:nat -> string[n]
tail: l:nat -> string[l] -> string[l-1]
concat: l1:nat -> string[l1] -> l2:nat -> string[l2] -> string[l1+l2]
where
 string[len] is an abbreviation of (i:nat -> {u:nat| (i<len => u=1)/\(i>=len => u=0)})
**)
