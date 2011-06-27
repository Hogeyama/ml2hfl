open ExtList

let rec split_at xs n =
  if n = 0 then
    [], xs
  else if n > 0 then
    (match xs with
      x::xs' ->
        let xs1, xs2 = split_at xs' (n - 1) in
        x::xs1, xs2)
  else
    assert false

let rec pr_list epr sep ppf xs =
  match xs with
    [] ->
      ()
  | [x] ->
      Format.fprintf ppf "%a" epr x
  | x::xs' ->
      Format.fprintf
        ppf
        "%a%a%a"
        epr x
        (fun ppf sep -> Format.fprintf ppf sep) sep
        (pr_list epr sep) xs'

(* graph *)
let save_as_dot filename vertices edges =
  let oc = open_out filename in 
  let ocf =
    Format.make_formatter
      (output oc)
      (fun () -> flush oc) in
  Format.fprintf ocf "@[<v>digraph flow {@ ";
  List.iter
    (fun (vertex, attribute) ->
      Format.fprintf ocf "  \"%s\" %s@ " vertex attribute)
    vertices;
  List.iter
    (fun (vertex1, vertex2, attribute) ->
      Format.fprintf ocf "  \"%s\" -> \"%s\" %s@ " vertex1 vertex2 attribute)
    edges;
  Format.fprintf ocf "}@]@?";
  close_out oc
