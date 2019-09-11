open Util

module Debug = Debug.Make(struct let check = Flag.Debug.make_check __MODULE__ end)

type t =
  {size : int;
   nodes : int list; (* must not be negative *)
   edges : (int * int) list}

let from_edges edges =
  let nodes = List.unique @@ List.flatten_map Pair.to_list edges in
  let size = 1 + List.fold_left max 0 nodes in
  {size; nodes; edges}

let save_as_dot filename ?(name_of=string_of_int) ?(attribute_of_node=Fun.const "") ?(attribute_of_edge=Fun.const "") graph =
  let oc = open_out filename in
  let ocf = Format.formatter_of_out_channel oc in
  Format.fprintf ocf "@[<v>digraph flow {@ ";
  List.iter (fun x -> Format.fprintf ocf "  \"%s\" %s@ " (name_of x) (attribute_of_node x)) graph.nodes;
  List.iter (fun (x,y) -> Format.fprintf ocf "  \"%s\" -> \"%s\" %s@ " (name_of x) (name_of y) (attribute_of_edge (x,y))) graph.edges;
  Format.fprintf ocf "}@]@?";
  close_out oc

(* Kosaraju's algorithm *)
let scc graph =
  let out_neighbor = Array.make graph.size [] in
  let in_neighbor = Array.make graph.size [] in
  let visited = Array.make graph.size false in
  let root = Array.make graph.size (-1) in
  List.iter (fun (x,y) -> out_neighbor.(x) <- y::out_neighbor.(x); in_neighbor.(y) <- x::in_neighbor.(y)) graph.edges;
  let rec visit acc x =
    if not visited.(x) then
      begin
        visited.(x) <- true;
        x :: List.fold_left visit acc out_neighbor.(x)
      end
    else
      acc
  in
  let rec assign x r =
    if root.(x) < 0 then
      begin
        root.(x) <- r;
        List.iter (assign -$- r) in_neighbor.(x)
      end
  in
  graph.nodes
  |> List.fold_left visit []
  |> List.iter (Fun.copy assign);
  root

(* Dijkstra's algorithm *)
(* `dist x y < 0` means disconnected *)
let shortest_to {size;nodes;edges} dist goal =
  let d = Array.make size (-1) in
  let in_neighbor = Array.make size [] in
  List.iter (fun (x,y) -> in_neighbor.(y) <- x::in_neighbor.(y)) edges;
  d.(goal) <- 0;
  let rec take m xs1 xs2 =
    match xs2 with
    | [] -> m, xs1
    | x::xs2' ->
        let m',xs1' =
          if d.(x) >= 0 then
            match m with
            | None -> Some x, xs1
            | Some m' when d.(x) < d.(m') -> Some x, m'::xs1
            | _ -> m, x::xs1
          else
            m, x::xs1
        in
        take m' xs1' xs2'
  in
  let rec go i xs =
    if xs <> [] then
      let y,ys = take None [] xs in
      match y with
      | None -> ()
      | Some y ->
          let update z =
            let d_zy = dist z y in
            if d.(z) < 0 || d_zy >= 0 && d.(z) > d.(y) + d_zy then
              d.(z) <- d.(y) + d_zy
          in
          List.iter update in_neighbor.(y);
          go (i+1) ys
  in
  go goal nodes;
  d

(* graph must be DAG *)
let paths_to {size;nodes;edges} goal =
  let paths = Array.make size None in
  let out_neighbor = Array.make size [] in
  List.iter (fun (x,y) -> out_neighbor.(x) <- y::out_neighbor.(x)) edges;
  paths.(goal) <- Some [[]];
  let rec go x =
    match paths.(x) with
    | None ->
        let ps =
          out_neighbor.(x)
          |> List.map (Pair.add_right go)
          |> List.flatten_map (fun (y,ps) -> List.map (List.cons y) ps)
        in
        paths.(x) <- Some ps;
        ps
    | Some ps -> ps
  in
  ignore @@ List.map go nodes;
  fun x -> Option.default [] paths.(x)

(* graph must be DAG *)
let hops_to {size;nodes;edges} goal =
  let hops = Array.make size (-2) in
  let out_neighbor = Array.make size [] in
  List.iter (fun (x,y) -> out_neighbor.(x) <- y::out_neighbor.(x)) edges;
  hops.(goal) <- 0;
  let rec go i prev x =
    if hops.(x) = -2 then
      let l =
        out_neighbor.(x)
        |> List.map (go (i+1) x)
        |> List.fold_left max (-1)
      in
      let l = if l < 0 then -1 else l + 1 in
      hops.(x) <- l;
      l
    else
      hops.(x)
  in
  ignore @@ List.map (go 0 (-1)) nodes;
  hops
