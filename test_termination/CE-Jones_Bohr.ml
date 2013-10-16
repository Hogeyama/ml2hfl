(* @from Call-by-value termination in the untyped lambda-calculus
         Jones and Bohr
         LMCS-4 2008
         AND
         Termination analysis and call graph construction for higher-order functional programs
         Sereni
         ICFP 2007 *)
let main (u:unit) =
  (fun a -> a (fun b -> a (fun (c:unit->unit) (d:unit) -> d))) (fun e -> e (fun (f:unit) -> f))
