let ign (u:unit) = ()
let double (g:unit -> unit) (u:unit) = g (g u)
let main (u:unit) = double (double (double ign)) ()
