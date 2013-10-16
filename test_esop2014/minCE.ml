let id (u:unit) = u
let f (g : unit -> unit) (u:unit) = g u
let main (u:unit) = f (f id) u
