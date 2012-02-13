open ExtList
open ExtString
open Zipper

(** Call trees *)

(** {6 Type} *)

type 'a t = { name: Var.t * int;
              closed: bool;
              ret: (Var.t * int) option;
              data: 'a }

(** {6 Functions on call trees} *)

let make name closed data =
  Zipper.make { name = name; closed = closed; ret = None; data = data } []

let rec path_set_open p =
		match p with
		  Top -> Top
		| Path(up, trs1, nd, trs2) ->
      Path(path_set_open up, trs1, { nd with ret = None(**ToDo*); closed = false }, trs2)

let rec pr ppf tr =
  match tr with
    Node(nd, trs) ->
				  let _ = Format.fprintf ppf "@[<v>%a@," Var.pr_x_uid nd.name in
				  let _ =
						  if trs <> [] then
  								let _ = Format.fprintf ppf "  @[<v>" in
				      let _ = Format.fprintf ppf "%a" (Util.pr_list pr "@,") trs in
          Format.fprintf ppf "@]"
				  in
				  if nd.closed then
        let x, id =
								  match nd.ret with
		          None -> nd.name
		        | Some(x, id) -> x, id
        in
						  Format.fprintf ppf "</%a@@%d>@]"
								  Var.pr x
								  id
