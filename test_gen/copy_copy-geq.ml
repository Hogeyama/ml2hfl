(* Function composition‚Å‚Q”Ô–Ú‚Ìmap‚Ìcontext‚ª
n=0‚Æ‚¢‚¤specific‚É‚È‚Á‚Ä‚µ‚Ü‚Á‚Ä‚¢‚é‚½‚ßŽ¸”s *)
let rec copy x = if x = 0 then 0 else 1 + copy (x - 1)
let check x y = assert (x >= y)
let main n = check (copy (copy n)) n
