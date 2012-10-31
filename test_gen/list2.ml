(*Bool’l‚Ìabstraction‚ÌƒoƒO‚Åprogress‚µ‚È‚¢‚±‚Æ‚ª‚ ‚é  new predicate not found*)
let mynil c n = n
let mycons x y c n = c x (y c n)
let rec mk_list n =
  if n=0 then mynil else mycons n (mk_list (n-1))
let fold l c n = l c n
let succ x n = n+1
let length l = fold l succ 0
let g p x b = (p x) && b
let forall p l = l (g p) true
let pos n = n>=0
let main n =
  assert (forall pos (mk_list n))
