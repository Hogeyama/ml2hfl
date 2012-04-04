open ExtList
open ExtString
open CompTree

(** Expanding computation trees *)

(** traversal strategies *)
type 'a s = { is_end: unit -> bool; get: unit -> 'a list; pick: unit -> 'a; add: 'a list -> unit }

let is_end wlr () = !wlr = []
let get wlr () = !wlr
(** breadth first strategy *)
let bf_strategy ct =
  let wlr = ref [ct] in
  { is_end = is_end wlr; get = get wlr;
    pick = (fun () -> match !wlr with [] -> raise Not_found | n::wl -> wlr := wl; n);
    add = (fun wl -> wlr := !wlr @ wl) }

(** depth first strategy *)
let df_strategy ct =
  let wlr = ref [ct] in
  { is_end = is_end wlr; get = get wlr;
    pick = (fun () -> match !wlr with [] -> raise Not_found | n::wl -> wlr := wl; n);
    add = (fun wl -> wlr := wl @ !wlr) }

(** strategy using a filter *)
let filter_strategy filter ct =
  let wlr = ref (filter [ct]) in
  { is_end = is_end wlr; get = get wlr;
    pick = (fun () -> match !wlr with [] -> raise Not_found | n::wl -> wlr := wl; n);
    add = (fun wl -> wlr := (filter wl) @ !wlr) }

let manual = ref false
let expand_until_new_error_trace_found prog ct strategy =
  let rec loop fenv =
    if strategy.is_end () then
      ()
    else
      let _ = if !Global.debug > 0 then save_as_dot "compTree.dot" ct (strategy.get ()) in
      let fenv, wl = expand_node prog fenv (strategy.pick ()) in
      let _ = strategy.add wl in
						let rec lp () =
						  let _ = Format.printf "expand the computation tree ? (y/n): %!" in
						  let inp = read_line () in
						  if inp = "y" then
						    true
						  else if inp = "n" then
						    false
						  else
						    lp ()
      in
      if List.exists (fun ct -> match ct.term with Term.Error(_) -> true | _ -> false) wl && (not !manual || lp ()) then
        ()
      else
        loop fenv
  in
  loop emp_fun_env

let expand_all prog ct strategy =
  let rec loop fenv =
    if strategy.is_end () then
      ()
    else
      let fenv, wl = expand_node prog fenv (strategy.pick ()) in
      let _ = strategy.add wl in
      loop fenv
  in
  loop emp_fun_env
