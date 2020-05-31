open Util
open Syntax

module Debug = Debug.Make(struct let check = Flag.Debug.make_check __MODULE__ end)

module List = Core.List

module IdKey = struct
  include ID
  let t_of_sexp _ = assert false
  let sexp_of_t x = Core.String.sexp_of_t (Id.to_string x)
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
      let ret =
        match Map.find g v with
        | None   -> g, ls
        | Some s ->
            Set.fold s ~init:(Map.remove g v, v :: ls) ~f:begin fun (rg,ls) v ->
              rdfs rg v ls
            end
      in
      ret

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

  let dfs_post_order g v =
    let add x ys =
      if List.mem ~equal:Id.same ys x then ys else x :: ys
    in
    let rec aux g v ls =
        match Map.find g v with
        | None -> g, add v ls
        | Some s ->
            let g, ls =
              Set.fold s ~init:(Map.remove g v, ls) ~f:begin fun (rg,ls) v ->
                aux rg v ls
              end
            in g, add v ls
    in
    List.rev @@ snd @@ aux g v []

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
    Debug.eprintf "@[<v2>DependencyGraph:@,%a@]@." Scc.pp_graph dep_graph;
    let mutual_recursives =
      Scc.scc dep_graph
      |> List.filter ~f:(fun x -> List.length x > 1)
      |> List.concat
      |> IdSet.of_list
    in
    Debug.eprintf "Mutually Recursive: %a@." Print.(list id) @@
      IdSet.to_list mutual_recursives
    ;
    let recursive, non_recursive =
      List.partition_tf defs ~f:begin fun (f,_,body) ->
        IdSet.mem mutual_recursives f ||
        IdSet.(mem (of_list (get_fv body)) f)
      end
    in
    let defs, linear =
      List.partition_tf recursive ~f:begin fun (f,_,body) ->
        let used =
          List.count (main_def::defs) ~f:begin fun (_,_,body) ->
            IdSet.(mem (of_list (get_fv body)) f)
          end
        in
        Debug.eprintf "used(%a) = %d@." Print.id f used;
        used > 1
      end
    in
    let topological_ord =
      Scc.dfs_post_order dep_graph main_var
      |> List.rev
      |> enumurate
      |> IdMap.of_alist_exn
    in
    let topological_sort xs = (* sort *)
      let value (v,_,_) =
        Core.Option.value ~default:0 @@ IdMap.find topological_ord v
      in
      List.sort xs ~compare:begin fun x y ->
        Int.compare (value x) (value y)
      end
    in
    let inlinables1 = topological_sort linear in
    let inlinables2 = topological_sort non_recursive in
    Debug.eprintf "Inlinable1: %a@." Print.(list id) @@
      List.map inlinables1 ~f:(fun (f,_,_) -> f)
    ;
    Debug.eprintf "Inlinable2: %a@." Print.(list id) @@
      List.map inlinables2 ~f:(fun (f,_,_) -> f)
    ;
    let inlinables = inlinables1 @ inlinables2 in
    Debug.eprintf "Inlinable: %a@." Print.(list id) @@
      List.map inlinables ~f:(fun (f,_,_) -> f)
    ;
    let pp_def ppf (f,xs,body) =
      Format.fprintf ppf "@[<2>%a %a =@ %a@]"
        Print.id f
        Print.(list id) xs
        Print.term body
    in
    let inlinables =
      List.fold_left inlinables ~init:inlinables ~f:begin fun fs (f,xs,body) ->
        Debug.eprintf "[Inline %a]@." Print.id f;
        (* Debug.eprintf "  @[<v>%a@]@." (Fmt.list pp_def) fs; *)
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

