let ign (u:unit) = ()
let app (g:unit -> unit) (u:unit) = g u
let main (u:unit) = app (app (app ign)) ()
