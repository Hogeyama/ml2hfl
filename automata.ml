
open Array
open Format






let rec print_state fm s = fprintf fm "q%d" s
let rec print_states fm = function
    [] -> ()
  | s::ss -> fprintf fm " %a%a" print_state s print_states ss
let rec print_buchi fm = function
    [] -> ()
  | (s,l,ss)::rest ->
      match ss with
          [] -> fprintf fm "%a %s -> .\n%a" print_state s l print_buchi rest
        | _ -> fprintf fm "%a %s ->%a.\n%a" print_state s l print_states ss print_buchi rest









