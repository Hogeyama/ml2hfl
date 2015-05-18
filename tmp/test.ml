
let rank2 = ref 9

let arg_spec = [
  "-rank", Arg.Set_int rank2, "";
]

let usage _ = ()

let () = Arg.parse arg_spec usage ""
