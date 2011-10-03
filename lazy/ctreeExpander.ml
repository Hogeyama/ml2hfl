open ExtList
open ExtString

type t = { is_end: unit -> bool; get: unit -> Ctree.t list; next: unit -> Ctree.t; update: Ctree.t list -> unit }

let bf_strategy rt =
  let wlr = ref [rt] in
  { is_end = (fun () -> !wlr = []);
    get = (fun () -> !wlr);
    next = (fun () -> match !wlr with [] -> raise Not_found | n::wl -> wlr := wl; n);
    update = (fun wl -> wlr := !wlr @ wl) }

let df_strategy rt =
  let wlr = ref [rt] in
  { is_end = (fun () -> !wlr = []);
    get = (fun () -> !wlr);
    next = (fun () -> match !wlr with [] -> raise Not_found | n::wl -> wlr := wl; n);
    update = (fun wl -> wlr := wl @ !wlr) }

let cex_strategy cex rt =
  let filt rts = List.filter (fun (Ctree.Node((_, p), _, _)) -> Util.prefix p cex) rts in
  let wlr = ref (filt [rt]) in
  { is_end = (fun () -> !wlr = []);
    get = (fun () -> !wlr);
    next = (fun () -> match !wlr with [] -> raise Not_found | n::wl -> wlr := wl; n);
    update = (fun wl -> wlr := (filt wl) @ !wlr) }


(* ToDo: refactor the function *)
let expand_until_new_error_path_found prog rt strategy =
  let rec loop fenv =
    if strategy.is_end () then
      ()
    else
      let _ = if Flag.debug then Ctree.save_as_dot "ctree.dot" rt (strategy.get ()) in
      let fenv, wl = Ctree.expand_node prog fenv (strategy.next ()) in
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
      if List.exists (function (Ctree.Node(_, Term.Error(_), _)) -> true | _ -> false) wl (*&& not (lp ())*) then
        ()
      else
        loop fenv
  in
  loop
    (fun x ->
      let _ = Format.printf "\"%a\" not found@." Var.pr x in
      assert false)
