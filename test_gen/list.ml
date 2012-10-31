(* CPS.mlがfunをサポートしていないので失敗?*)
let mynil c n = n
let mycons x y c n = c x (y c n)
let rec mk_list n =
  if n=0 then mynil else mycons n (mk_list (n-1))
let fold l c n = l c n
let mymap f l c n = l (fun x -> fun z -> c (f x) z) n
let succ x n = n+1
let length l = fold l succ 0
let main n = assert (length (mk_list n) >= n)
