let lamp x = x in
let f =
  let id x = x in
  let unused = id fail in
    lamp
in
  f ()
