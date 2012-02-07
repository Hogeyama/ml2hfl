open ExtList
open ExtString
open CompTree

(** Expanding computation trees *)

(** expansion strategies *)
type s = { is_end: unit -> bool; get: unit -> t list; next: unit -> t; update: t list -> unit }

(** breadth first strategy *)
let bf_strategy ct =
  let wlr = ref [ct] in
  { is_end = (fun () -> !wlr = []);
    get = (fun () -> !wlr);
    next = (fun () -> match !wlr with [] -> raise Not_found | n::wl -> wlr := wl; n);
    update = (fun wl -> wlr := !wlr @ wl) }

(** depth first strategy *)
let df_strategy ct =
  let wlr = ref [ct] in
  { is_end = (fun () -> !wlr = []);
    get = (fun () -> !wlr);
    next = (fun () -> match !wlr with [] -> raise Not_found | n::wl -> wlr := wl; n);
    update = (fun wl -> wlr := wl @ !wlr) }

(** strategy using a given counterexample *)
let cex_strategy cex ct =
  let filt cts = List.filter (fun ct -> Util.is_prefix ct.path cex) cts in
  let wlr = ref (filt [ct]) in
  { is_end = (fun () -> !wlr = []);
    get = (fun () -> !wlr);
    next = (fun () -> match !wlr with [] -> raise Not_found | n::wl -> wlr := wl; n);
    update = (fun wl -> wlr := (filt wl) @ !wlr) }


(** ToDo: refactor the function *)
let expand_until_new_error_trace_found prog ct strategy =
  let rec loop fenv =
    if strategy.is_end () then
      ()
    else
      let _ = if Flags.debug then save_as_dot "compTree.dot" ct (strategy.get ()) in
      let fenv, wl = expand_node prog fenv (strategy.next ()) in
      let _ = strategy.update wl in
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
      if List.exists (fun ct -> match ct.term with Term.Error(_) -> true | _ -> false) wl (*&& not (lp ())*) then
        ()
      else
        loop fenv
  in
  loop
    (fun x ->
      let _ = Format.printf "\"%a\" not found@." Var.pr x in
      assert false)
