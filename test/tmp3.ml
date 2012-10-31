let h k = () in
let f g x = h g; g x in
let b x = x in
 assert (f b n>=n)
