type t = HCCS.t -> PredSubst.t * TermSubst.t
let ref_solver = ref (fun _ -> assert false : t)

let ext_of_string = ref (fun _ -> assert false : string -> t)
let of_string_dyn str = !ext_of_string str
