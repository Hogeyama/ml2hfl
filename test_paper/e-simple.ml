let f n k = if n >= 0 then () else k 0 in
let g n = assert (n = 0) in
  f n g
