open Util
open Syntax

module Debug = Debug.Make(struct let check = Flag.Debug.make_check __MODULE__ end)

module List = Core.List

module IdKey = struct
  include ID
  let t_of_sexp _ = assert false
  let sexp_of_t _ = assert false
end

module Scc = struct
  open Core
  module Set = Set.Make(IdKey)
  module Map = Map.Make(IdKey)
  let replace : 'a Map.t -> key:id -> data:'a -> 'a Map.t =
    fun map ~key ~data ->
      let map = Map.remove map key in
      Map.add_exn map ~key ~data
  type graph = Set.t Map.t
  let pp_graph ppf (g : graph) =
    let pp_node ppf (x, ys) =
      Format.fprintf ppf "@[<h>%a -> %a@]" Print.id x Print.(list id) (Set.to_list ys)
    in
    Fmt.pf ppf "@[<v>%a@]" (Fmt.list pp_node) (Map.to_alist g)

  let rg : graph -> graph = fun g ->
    Map.fold g ~init:Map.empty ~f:begin fun ~key ~data:set map ->
      let map' =
        if Map.mem map key
        then map
        else Map.add_exn map ~key ~data:Set.empty
      in
      Set.fold set ~init:map' ~f:begin fun map v ->
        let data =
          match Map.find map v with
          | Some s -> Set.add s key
          | None   -> Set.singleton key
        in
        replace map ~key:v ~data
      end
    end
  let rec dfs : graph -> Set.t -> id list -> graph * id list =
    fun g ls r ->
      Set.fold ls ~init:(g,r) ~f:begin fun (g,r) x ->
        match Map.find g x with
        | None -> g, r
        | Some s ->
            let g3, r3 = dfs (Map.remove g x) (Set.remove s x) r in
            g3, x::r3
      end
  let rec rdfs : graph -> id -> id list -> graph * id list =
    fun g v ls ->
      match Map.find g v with
      | None   -> g, ls
      | Some s ->
          Set.fold s ~init:(Map.remove g v, v :: ls) ~f:begin fun (rg,ls) v ->
            rdfs rg v ls
          end
  let scc g =
    let rG = rg g in
    let map, vs = dfs g (Set.of_list @@ Map.keys g) [] in
    let _, ls =
      List.fold vs ~init:(rG, []) ~f:begin fun (rg,ls) v ->
        let rg2, l = rdfs rg v [] in
        if Int.equal (List.length l) 0
        then rg2, ls
        else rg2, l::ls
      end
    in
    ls
end
module IdSet = Scc.Set
module IdMap = Scc.Map

type def = (id * (id list * term))

let enumurate xs = List.zip_exn xs (List.init (List.length xs) ~f:(fun x -> x))

let reshape : def list -> (id * id list * term) list =
  List.map ~f:(fun (f,(xs,body)) -> (f,xs,body))

let rereshape : (id * id list * term) list -> def list =
  List.map ~f:(fun (f,xs,body) -> (f,(xs,body)))

let inline : def list * term -> def list * term =
  fun (defs, main) ->
    let main_var = Id.new_var ~name:"main" Type.Ty.unit in
    let main_def = (main_var, [], main) in
    let defs = reshape defs in
    let functions = IdSet.of_list @@ List.map defs ~f:(fun (f,_,_) -> f) in
    let dep_graph : Scc.graph =
      IdMap.of_alist_exn @@ List.map (main_def :: defs) ~f:begin fun (f,_xs,body) ->
        let dep =
          List.filter (get_fv body) ~f:(IdSet.mem functions)
        in
        f, IdSet.of_list dep
      end
    in
    Debug.eprintf "DependencyGraph:@.%a@." Scc.pp_graph dep_graph;
    let mutual_recursives =
      Scc.scc dep_graph
      |> List.filter ~f:(fun x -> List.length x > 1)
      |> List.concat
      |> IdSet.of_list
    in
    Debug.eprintf "Mutually Recursive: %a@." Print.(list id) @@
      IdSet.to_list mutual_recursives
    ;
    let defs, inlinables =
      List.partition_tf defs ~f:begin fun (f,_,body) ->
        let is_rec =
            IdSet.mem mutual_recursives f ||
            IdSet.(mem (of_list (get_fv body)) f)
        in
        let used =
          List.count (main_def::defs) ~f:begin fun (_,_,body) ->
            IdSet.(mem (of_list (get_fv body)) f)
          end
        in
        Debug.eprintf "used(%a) = %d@." Print.id f used;
        is_rec && used > 1
      end
    in
    Debug.eprintf "Inlinable: %a@." Print.(list id) @@
      List.map inlinables ~f:(fun (f,_,_) -> f)
    ;
    let inlinables =
      let topological_ord =
        Scc.rdfs dep_graph main_var []
        |> snd
        |> List.rev
        |> enumurate
        |> IdMap.of_alist_exn
      in
      let value (v,_,_) =
        Core.Option.value ~default:0 @@ IdMap.find topological_ord v
      in
      List.sort inlinables ~compare:begin fun x y ->
        Int.compare (value x) (value y)
      end
    in
    Debug.eprintf "Inlinable: %a@." Print.(list id) @@
      List.map inlinables ~f:(fun (f,_,_) -> f)
    ;
    let inlinables =
      List.fold_left inlinables ~init:inlinables ~f:begin fun fs (f,xs,body) ->
        List.map fs ~f:begin fun (f',xs',body') ->
          (f', xs', Trans.inlined_f ~defs:[f,xs,body] body' |> Trans.beta_reduce)
        end
      end
    in
    let defs =
      List.map defs ~f:begin fun (f,xs,body) ->
        (f, xs, Trans.inlined_f ~defs:inlinables body |> Trans.beta_reduce)
      end
    in
    let main = Trans.inlined_f ~defs:inlinables main |> Trans.beta_reduce in
    (rereshape defs, main)

(*
type def = (id * (id list * term))

let set_of_list xs = IdSet.of_enum @@ List.enum xs

let reshape : def list -> (id * id list * term) list =
  List.map (fun (f,(xs,body)) -> (f,xs,body))

let rereshape : (id * id list * term) list -> def list =
  List.map (fun (f,xs,body) -> (f,(xs,body)))

let inline : def list * term -> def list * term =
  fun (defs, main) ->
    let defs = reshape defs in
    let is_trivially_inlinable (f,xs,body) =
      let fvs = get_fv body in
      let closed = IdSet.(subset (set_of_list (get_fv body)) (set_of_list xs)) in
      let used =
        List.count (fun body -> IdSet.(mem f (set_of_list @@ get_fv body)))
            (main :: List.map (fun (_,_,body) -> body) defs)
      in
      Debug.eprintf "f: %a@." Print.id f;
      Debug.eprintf "xs  : %a@." Print.(list id) xs;
      Debug.eprintf "fvs : %a@." Print.(list id) fvs;
      Debug.eprintf "used: %d@." used;
      closed || used <= 1
      (* used=2でinline可能なケースもある気がするんだけどなあ
       * let rec g y = g (y+1)
       * let f x = g (x+1)
       * let rec h  z  = h  (f z )
       * let rec h' z' = h' (f z')
       * *)
    in
    let rec go (defs,main) =
      let inlinables, defs = List.partition is_trivially_inlinable defs in
      match inlinables with
      | [] -> defs,main
      | _ ->
          Debug.eprintf "Inline %a@." Print.(list id) (List.map (fun(f,_,_) -> f) inlinables);
          let inline t = Trans.beta_reduce (Trans.inlined_f ~defs:inlinables t) in
          let defs =
            List.Labels.map defs ~f:begin fun (f,xs,body) ->
              f, xs, inline body
            end
          in
          let main = inline main in
          go (defs, main)
    in
    let defs,main = go (defs,main) in
    (rereshape defs, main)
*)
