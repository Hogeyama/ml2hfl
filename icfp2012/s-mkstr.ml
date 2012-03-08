(** explicit parameter version **)

let rec mkstr (n:int) = fun i -> if i<n then 1 else 0

let rec tail (l:int) s = fun i -> s(i+1)

let rec concat (l1:int) f (l2:int) g =
 if f 0 = 0 then g
 else fun i -> if i=0 then f(0) else concat (l1-1) (tail l1 f) l2 g (i-1)

let main (i:int) j =
  if i >= 0 && j >= 0 then
		  let s1 = mkstr i in
		  let s2 = mkstr j in
		  let s = concat i s1 (i+j) (concat i s1 j s2) in
		     assert (s (i+i+j) = 0)

(** types **
mkstr: n:nat -> string[n]
tail: l:nat -> string[l] -> string[l-1]
concat: l1:nat -> string[l1] -> l2:nat -> string[l2] -> string[l1+l2]
where
 string[len] is an abbreviation of (i:nat -> {u:nat| (i<len => u=1)/\(i>=len => u=0)})
**)
