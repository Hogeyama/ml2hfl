open Util

type 'a t = Leaf of 'a | Node of 'a t list
type path = int list

let root = function
  | Leaf t -> t
  | Node _ -> raise (Invalid_argument "Rose_tree.root")
let rec flatten = function
  | Leaf t -> [t]
  | Node ts -> List.flatten @@ List.map flatten ts

let rec map f path = function
  | Leaf t -> Leaf (f path t)
  | Node ts -> Node(List.mapi (fun i t -> map f (path@[i]) t) ts)
let map f t = map f [] t

let rec fold f_node f_leaf = function
  | Leaf x -> f_leaf x
  | Node ts -> f_node @@ List.map (fold f_node f_leaf) ts

let rec for_all f = function
  | Leaf x -> f x
  | Node ts -> List.for_all (for_all f) ts

let rec exists f = function
  | Leaf x -> f x
  | Node ts -> List.exists (exists f) ts

let rec proj path t =
  match path,t with
  | [],_ -> t
  | i::path',Node ts -> proj path' @@ List.nth ts i
  | _ -> assert false

let rec print pr fm t =
  match t with
  | Leaf x -> pr fm x
  | Node ts -> Format.fprintf fm "[%a]" (print_list (print pr) ";") ts

let rec update path t' t =
  match path,t with
  | [], _ -> t'
  | i::path', Node ts ->
      Node (List.mapi (fun j t -> if i = j then update path' t' t else t) ts)
  | _ -> failwith "Rose_tree.update"

let rec zip t1 t2 =
  match t1, t2 with
  | Leaf x, Leaf y -> Leaf (x,y)
  | Node ts1, Node ts2 -> Node (List.map2 (fun t1 t2 -> zip t1 t2) ts1 ts2)
  | _ -> raise (Invalid_argument "Rose_tree.zip")
