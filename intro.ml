let ff x g = g x in
let hh y = assert (y>0) in
  if n>0 then ff n hh else ()
