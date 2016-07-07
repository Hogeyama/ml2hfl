let twice f = f 0; f 0
let main _ = twice (fun x -> assert (x >= 0))
