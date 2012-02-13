(**
# module X = struct type t = int ref;; let equal = (==);; let hash = Hashtbl.hash end;;
module X :
  sig type t = int ref val equal : 'a -> 'a -> bool val hash : 'a -> int end
# module Y = Hashtbl.Make(X);;
module Y :
  sig
    type key = X.t
    type 'a t = 'a Hashtbl.Make(X).t
    val create : int -> 'a t
    val clear : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length : 'a t -> int
  end
**)


let g0 =
 {nt =
  [("S", O); ("Sort", O); ("Sortsub", O); ("Sortsub2", O); ("Sortsub3", O);
   ("Sort_and_merge", O); ("Sort_and_merge_sub", O);
   ("Sort_and_merge_sub2", O); ("Div", O); ("Divsub", O); ("Copy", O);
   ("G", O); ("Merge", O); ("C", O); ("Cons1", O); ("C2", O); ("Cons2", O)];
 t = [("a", O); ("b", O); ("nil", O)];
 r =
  [("S",
    ([],
     App (NT "Sort",
      [App (NT "Copy", []);
       App (PAIR,
        [App (FD 1, []);
         App (PAIR,
          [App (FD 2, []);
           App (PAIR,
            [App (FD 2, []);
             App (PAIR,
              [App (FD 2, []);
               App (PAIR,
                [App (FD 1, []);
                 App (PAIR, [App (FD 0, []); App (FD 0, [])])])])])])])])));
   ("Sort",
    (["k"; "x"],
     App (DPAIR,
      [App (Var "x", []); App (NT "Sortsub", [App (Var "k", [])])])));
   ("Sortsub",
    (["k"; "c"; "x"],
     App (CASE 3,
      [App (Var "c", []);
       App (Var "k", [App (PAIR, [App (FD 0, []); App (FD 0, [])])]);
       App (NT "Sortsub2",
        [App (Var "k", []); App (Var "x", []); App (FD 1, [])]);
       App (NT "Sortsub2",
        [App (Var "k", []); App (Var "x", []); App (FD 2, [])])])));
   ("Sortsub2",
    (["k"; "tl"; "hd"],
     App (DPAIR,
      [App (Var "tl", []);
       App (NT "Sortsub3", [App (Var "k", []); App (Var "hd", [])])])));
   ("Sortsub3",
    (["k"; "hd"; "c"; "x"],
     App (CASE 3,
      [App (Var "c", []);
       App (Var "k",
        [App (PAIR,
          [App (Var "hd", []); App (PAIR, [App (FD 0, []); App (FD 0, [])])])]);
       App (NT "Div",
        [App (NT "Sort_and_merge", [App (Var "k", [])]);
         App (PAIR,
          [App (Var "hd", []);
           App (PAIR, [App (FD 1, []); App (Var "x", [])])]);
         App (PAIR, [App (FD 0, []); App (FD 0, [])]);
         App (PAIR, [App (FD 0, []); App (FD 0, [])])]);
       App (NT "Div",
        [App (NT "Sort_and_merge", [App (Var "k", [])]);
         App (PAIR,
          [App (Var "hd", []);
           App (PAIR, [App (FD 2, []); App (Var "x", [])])]);
         App (PAIR, [App (FD 0, []); App (FD 0, [])]);
         App (PAIR, [App (FD 0, []); App (FD 0, [])])])])));
   ("Sort_and_merge",
    (["k"; "x"],
     App (DPAIR,
      [App (Var "x", []); App (NT "Sort_and_merge_sub", [App (Var "k", [])])])));
   ("Sort_and_merge_sub",
    (["k"; "x"; "y"],
     App (NT "Sort",
      [App (NT "Sort_and_merge_sub2", [App (Var "k", []); App (Var "y", [])]);
       App (Var "x", [])])));
   ("Sort_and_merge_sub2",
    (["k"; "y"; "x"],
     App (NT "Sort",
      [App (NT "Merge", [App (Var "k", []); App (Var "x", [])]);
       App (Var "y", [])])));
   ("Div",
    (["k"; "x"; "y"; "z"],
     App (DPAIR,
      [App (Var "x", []);
       App (NT "Divsub",
        [App (Var "k", []); App (Var "y", []); App (Var "z", [])])])));
   ("Divsub",
    (["k"; "y"; "z"; "c"; "x"],
     App (CASE 3,
      [App (Var "c", []);
       App (Var "k", [App (PAIR, [App (Var "y", []); App (Var "z", [])])]);
       App (NT "Div",
        [App (Var "k", []); App (Var "x", []); App (Var "z", []);
         App (PAIR, [App (FD 1, []); App (Var "y", [])])]);
       App (NT "Div",
        [App (Var "k", []); App (Var "x", []); App (Var "z", []);
         App (PAIR, [App (FD 2, []); App (Var "y", [])])])])));
   ("Copy", (["x"], App (DPAIR, [App (Var "x", []); App (NT "G", [])])));
   ("G",
    (["x"; "y"],
     App (CASE 3,
      [App (Var "x", []); App (T "nil", []);
       App (T "a", [App (NT "Copy", [App (Var "y", [])])]);
       App (T "b", [App (NT "Copy", [App (Var "y", [])])])])));
   ("Merge",
    (["k"; "x"; "y"],
     App (DPAIR,
      [App (Var "x", []);
       App (NT "C", [App (Var "k", []); App (Var "y", [])])])));
   ("C",
    (["k"; "y"; "c"; "x"],
     App (CASE 3,
      [App (Var "c", []); App (Var "k", [App (Var "y", [])]);
       App (NT "Merge",
        [App (NT "Cons1", [App (Var "k", [])]); App (Var "x", []);
         App (Var "y", [])]);
       App (DPAIR,
        [App (Var "y", []);
         App (NT "C2", [App (Var "k", []); App (Var "x", [])])])])));
   ("Cons1",
    (["k"; "x"],
     App (Var "k", [App (PAIR, [App (FD 1, []); App (Var "x", [])])])));
   ("C2",
    (["k"; "x"; "c"; "y"],
     App (CASE 3,
      [App (Var "c", []);
       App (Var "k", [App (PAIR, [App (FD 2, []); App (Var "x", [])])]);
       App (NT "Merge",
        [App (NT "Cons1", [App (Var "k", [])]);
         App (PAIR, [App (FD 2, []); App (Var "x", [])]); App (Var "y", [])]);
       App (NT "Merge",
        [App (NT "Cons2", [App (Var "k", [])]); App (Var "x", []);
         App (Var "y", [])])])));
   ("Cons2",
    (["k"; "x"],
     App (Var "k",
      [App (PAIR,
        [App (FD 2, []); App (PAIR, [App (FD 2, []); App (Var "x", [])])])])))];
 s = "S"}

let dmap0 =   [("S", []); ("Sort", ["S"; "Sort_and_merge_sub"; "Sort_and_merge_sub2"]);
   ("Sortsub", ["Sort"]); ("Sortsub2", ["Sortsub"]);
   ("Sortsub3", ["Sortsub2"]); ("Sort_and_merge", ["Sortsub3"]);
   ("Sort_and_merge_sub", ["Sort_and_merge"]);
   ("Sort_and_merge_sub2", ["Sort_and_merge_sub"]);
   ("Div", ["Divsub"; "Sortsub3"]); ("Divsub", ["Div"]);
   ("Copy", ["G"; "S"]); ("G", ["Copy"]);
   ("Merge", ["C"; "C2"; "Sort_and_merge_sub2"]); ("C", ["Merge"]);
   ("Cons1", ["C"; "C2"]); ("C2", ["C"]); ("Cons2", ["C2"])]

let nte0 = 
[("Divsub", []); ("Copy", []); ("C", []); ("Merge", []); ("C2", []);
 ("Sort", []); ("Sort_and_merge", []); ("Sort_and_merge_sub", []); ("S", []);
 ("Cons2", []); ("Sortsub", []); ("Cons1", []); ("Div", []);
 ("Sort_and_merge_sub2", []); ("Sortsub3", []); ("G", []); ("Sortsub2", [])]
let cte0 = 
[("nil", [ITbase "q1"; ITbase "q0"]);
 ("b",
  [ITfun ([ITbase "q1"], ITbase "q1"); ITfun ([ITbase "q1"], ITbase "q0")]);
 ("a", [ITfun ([ITbase "q0"], ITbase "q0")])]

let te0 = 
[("Divsub",
  [ITfun ([ITfun ([], ITbase "q0")],
    ITfun ([], ITfun ([], ITfun ([ITbase "0"], ITfun ([], ITbase "q0")))));
   ITfun ([ITfun ([], ITbase "q0")],
    ITfun ([],
     ITfun ([],
      ITfun ([ITbase "1"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0")))));
   ITfun ([ITfun ([], ITbase "q0")],
    ITfun ([],
     ITfun ([],
      ITfun ([ITbase "1"],
       ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
        ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "1"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITpair1 (ITbase "0")],
      ITfun ([ITbase "1"],
       ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
        ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "1"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun ([ITbase "1"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "1"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun ([ITbase "0"], ITfun ([], ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITpair1 (ITbase "0")],
      ITfun ([ITbase "1"],
       ITfun
        ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "1"));
          ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
        ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun ([ITbase "2"],
       ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
        ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun ([ITbase "1"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun
      ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
      ITfun ([ITbase "0"], ITfun ([], ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITpair1 (ITbase "0")],
      ITfun ([ITbase "2"],
       ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
        ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun ([ITbase "2"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun ([ITbase "0"], ITfun ([], ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")))],
       ITbase "q0")],
    ITfun ([],
     ITfun ([],
      ITfun ([ITbase "1"],
       ITfun
        ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
          ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
          ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
          ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
        ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")))],
       ITbase "q0")],
    ITfun ([],
     ITfun ([],
      ITfun ([ITbase "2"],
       ITfun
        ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
          ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
          ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
        ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")))],
       ITbase "q0")],
    ITfun ([],
     ITfun ([ITpair1 (ITbase "2")],
      ITfun ([ITbase "2"],
       ITfun
        ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "1"));
          ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
        ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")))],
       ITbase "q0")],
    ITfun ([],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"))],
      ITfun ([ITbase "1"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2")],
     ITfun ([],
      ITfun ([ITbase "2"],
       ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
        ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"))],
     ITfun ([], ITfun ([ITbase "0"], ITfun ([], ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")))],
       ITbase "q0")],
    ITfun ([],
     ITfun ([ITpair1 (ITbase "0")],
      ITfun ([ITbase "1"],
       ITfun
        ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
          ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
          ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
          ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
        ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")))],
       ITbase "q0")],
    ITfun ([],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun ([ITbase "2"],
       ITfun
        ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "1"));
          ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
        ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([],
      ITfun ([ITbase "2"],
       ITfun
        ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
          ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
          ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
        ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2")],
     ITfun
      ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
      ITfun ([ITbase "1"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "2")],
      ITfun ([ITbase "2"],
       ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
        ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"))],
      ITfun ([ITbase "0"], ITfun ([], ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITpair1 (ITbase "0")],
      ITfun ([ITbase "1"],
       ITfun
        ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
          ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
          ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
          ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
        ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun ([ITbase "2"],
       ITfun
        ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
          ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
          ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
        ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun ([ITbase "2"],
       ITfun
        ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "1"));
          ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
        ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun
      ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "1"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
      ITfun ([ITbase "2"],
       ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
        ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun
      ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
      ITfun ([ITbase "1"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun
      ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
        ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
      ITfun ([ITbase "0"], ITfun ([], ITbase "q0")))))]);
 ("Copy",
  [ITfun ([ITpair1 (ITbase "0")], ITbase "q1");
   ITfun
    ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
      ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
      ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
      ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2")))));
      ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
    ITbase "q0");
   ITfun
    ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
      ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
      ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
      ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
    ITbase "q0");
   ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
    ITbase "q1");
   ITfun
    ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
      ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
    ITbase "q1");
   ITfun
    ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
      ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
      ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
    ITbase "q0")]);
 ("C",
  [ITfun ([ITfun ([], ITbase "q0")],
    ITfun ([], ITfun ([ITbase "0"], ITfun ([], ITbase "q0"))));
   ITfun ([ITfun ([], ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITbase "2"], ITfun ([], ITbase "q0"))));
   ITfun ([ITfun ([ITpair1 (ITbase "0")], ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITbase "0"], ITfun ([], ITbase "q0"))));
   ITfun ([ITfun ([ITpair1 (ITbase "1")], ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITbase "2"], ITfun ([], ITbase "q0"))));
   ITfun
    ([ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITbase "0"], ITfun ([], ITbase "q0"))));
   ITfun
    ([ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun ([ITbase "2"], ITfun ([], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITbase "1"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun ([ITbase "2"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2")))));
         ITpair2
          (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
     ITfun ([ITbase "2"],
      ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITbase "2"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun ([ITbase "2"],
      ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))));
   ITfun ([ITfun ([ITpair1 (ITbase "2")], ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2")],
     ITfun ([ITbase "2"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))));
   ITfun
    ([ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITbase "2"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITbase "2"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITbase "2"],
      ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))))]);
 ("Merge",
  [ITfun ([ITfun ([], ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")], ITfun ([], ITbase "q0")));
   ITfun ([ITfun ([], ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2")],
     ITfun ([ITpair1 (ITbase "0")], ITbase "q0")));
   ITfun ([ITfun ([ITpair1 (ITbase "0")], ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITpair1 (ITbase "0")], ITbase "q0")));
   ITfun ([ITfun ([ITpair1 (ITbase "1")], ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2")],
     ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
      ITbase "q0")));
   ITfun
    ([ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
      ITbase "q0")));
   ITfun
    ([ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2")],
     ITfun
      ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun
      ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2")))));
         ITpair2
          (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun
      ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
        ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun
      ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
      ITbase "q0")));
   ITfun ([ITfun ([ITpair1 (ITbase "2")], ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "2")], ITbase "q0")));
   ITfun
    ([ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "0")], ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
      ITbase "q0")))]);
 ("C2",
  [ITfun ([ITfun ([], ITbase "q0")],
    ITfun ([], ITfun ([ITbase "0"], ITfun ([], ITbase "q0"))));
   ITfun ([ITfun ([ITpair1 (ITbase "1")], ITbase "q0")],
    ITfun ([],
     ITfun ([ITbase "1"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))));
   ITfun
    ([ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"))],
       ITbase "q0")],
    ITfun ([],
     ITfun ([ITbase "1"],
      ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITbase "1"],
      ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2")))));
         ITpair2
          (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITbase "1"],
      ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITbase "1"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITbase "1"],
      ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))));
   ITfun ([ITfun ([ITpair1 (ITbase "2")], ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITbase "2"], ITfun ([], ITbase "q0"))));
   ITfun
    ([ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITbase "0"], ITfun ([], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITbase "2"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITbase "2"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))))]);
 ("Sort",
  [ITfun
    ([ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITbase "q0"));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITbase "q0"));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
     ITbase "q0"));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2")))));
         ITpair2
          (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
       ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1")))));
       ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
     ITbase "q0"));
   ITfun ([ITfun ([ITpair1 (ITbase "2")], ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITbase "q0"));
   ITfun
    ([ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITbase "q0"));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITbase "q0"))]);
 ("Sort_and_merge",
  [ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITpair1 (ITbase "1"));
       ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
       ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITbase "q0"));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITpair1 (ITbase "2"));
       ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
       ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
     ITbase "q0"));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2")))));
         ITpair2
          (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITpair1 (ITbase "2"));
       ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
       ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
       ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
       ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
     ITbase "q0"));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITpair1 (ITbase "2"));
       ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
       ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITbase "q0"))]);
 ("Sort_and_merge_sub",
  [ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun
      ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2")))));
         ITpair2
          (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun
      ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
        ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
      ITbase "q0")))]);
 ("S", [ITbase "q0"]);
 ("Cons2",
  [ITfun ([ITfun ([ITpair1 (ITbase "2")], ITbase "q0")],
    ITfun ([], ITbase "q0"));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")], ITbase "q0"));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITbase "q0"))]);
 ("Sortsub",
  [ITfun
    ([ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0")],
    ITfun ([ITbase "1"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITbase "1"],
     ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun ([ITbase "1"],
     ITfun
      ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "1"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2")))));
         ITpair2
          (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
       ITbase "q0")],
    ITfun ([ITbase "1"],
     ITfun
      ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
        ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
        ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
      ITbase "q0")));
   ITfun ([ITfun ([ITpair1 (ITbase "2")], ITbase "q0")],
    ITfun ([ITbase "2"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0")));
   ITfun
    ([ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0")],
    ITfun ([ITbase "2"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITbase "2"],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
      ITbase "q0")))]);
 ("Cons1",
  [ITfun ([ITfun ([ITpair1 (ITbase "1")], ITbase "q0")],
    ITfun ([], ITbase "q0"));
   ITfun
    ([ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1")], ITbase "q0"));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITbase "q0"));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITbase "q0"));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2")))));
         ITpair2
          (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
       ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
     ITbase "q0"));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITbase "q0"));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
     ITbase "q0"))]);
 ("Div",
  [ITfun ([ITfun ([], ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")], ITfun ([], ITfun ([], ITbase "q0"))));
   ITfun ([ITfun ([], ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([], ITfun ([], ITbase "q0"))));
   ITfun ([ITfun ([], ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun ([], ITfun ([], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "1"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "1"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "0")],
      ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "1"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun ([ITpair1 (ITbase "0")],
      ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
     ITfun ([ITpair1 (ITbase "0")],
      ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun ([ITpair1 (ITbase "0")],
      ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "0")],
      ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun ([ITpair1 (ITbase "0")],
      ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"))],
      ITfun ([], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([],
      ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
       ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1")))));
       ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
     ITfun ([], ITfun ([], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun ([ITpair1 (ITbase "2")], ITfun ([], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
     ITfun ([], ITfun ([ITpair1 (ITbase "2")], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
       ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
     ITfun ([], ITfun ([], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun
      ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
      ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "2")],
      ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
       ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1")))));
       ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
     ITfun ([], ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun ([ITpair1 (ITbase "2")], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
     ITfun ([],
      ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
       ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
     ITfun ([ITpair1 (ITbase "0")], ITfun ([], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun
      ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
      ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun
      ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "1"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
      ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
       ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1")))));
       ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
     ITfun ([ITpair1 (ITbase "0")],
      ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
     ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
       ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
     ITfun ([ITpair1 (ITbase "0")],
      ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))))]);
 ("Sort_and_merge_sub2",
  [ITfun
    ([ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun ([ITpair1 (ITbase "2")], ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2")))));
         ITpair2
          (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
     ITfun
      ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
      ITbase "q0")))]);
 ("Sortsub3",
  [ITfun ([ITfun ([], ITbase "q0")],
    ITfun ([], ITfun ([ITbase "0"], ITfun ([], ITbase "q0"))));
   ITfun
    ([ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0")],
    ITfun ([ITbase "1"], ITfun ([ITbase "0"], ITfun ([], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITbase "1"],
     ITfun ([ITbase "1"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun ([ITbase "1"],
     ITfun ([ITbase "2"],
      ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2")))));
         ITpair2
          (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
       ITbase "q0")],
    ITfun ([ITbase "1"],
     ITfun ([ITbase "2"],
      ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0"))));
   ITfun ([ITfun ([ITpair1 (ITbase "2")], ITbase "q0")],
    ITfun ([ITbase "2"], ITfun ([ITbase "0"], ITfun ([], ITbase "q0"))));
   ITfun
    ([ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0")],
    ITfun ([ITbase "2"], ITfun ([ITbase "0"], ITfun ([], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITbase "2"],
     ITfun ([ITbase "2"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))))]);
 ("G",
  [ITfun ([ITbase "0"], ITfun ([], ITbase "q1"));
   ITfun ([ITbase "1"],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
       ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
     ITbase "q0"));
   ITfun ([ITbase "1"],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
     ITbase "q0"));
   ITfun ([ITbase "2"], ITfun ([ITpair1 (ITbase "0")], ITbase "q1"));
   ITfun ([ITbase "2"],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITbase "q1"));
   ITfun ([ITbase "2"],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITbase "q0"))]);
 ("Sortsub2",
  [ITfun ([ITfun ([], ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")], ITfun ([], ITbase "q0")));
   ITfun
    ([ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")], ITfun ([ITbase "1"], ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITbase "1"], ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun ([ITbase "1"], ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2")))));
         ITpair2
          (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
       ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
     ITfun ([ITbase "1"], ITbase "q0")));
   ITfun ([ITfun ([ITpair1 (ITbase "2")], ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")], ITfun ([ITbase "2"], ITbase "q0")));
   ITfun
    ([ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")], ITfun ([ITbase "2"], ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITbase "2"], ITbase "q0")))])]

let te1 = 
[("Divsub",
  [ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "1"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITpair1 (ITbase "0")],
      ITfun ([ITbase "1"],
       ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
        ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "1"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun ([ITbase "1"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "1"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun ([ITbase "0"], ITfun ([], ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITpair1 (ITbase "0")],
      ITfun ([ITbase "1"],
       ITfun
        ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "1"));
          ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
        ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun ([ITbase "2"],
       ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
        ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun ([ITbase "1"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun
      ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
      ITfun ([ITbase "0"], ITfun ([], ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITpair1 (ITbase "0")],
      ITfun ([ITbase "2"],
       ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
        ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun ([ITbase "2"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun ([ITbase "0"], ITfun ([], ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITpair1 (ITbase "0")],
      ITfun ([ITbase "1"],
       ITfun
        ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
          ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
          ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
          ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
        ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun ([ITbase "2"],
       ITfun
        ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
          ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
          ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
        ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun ([ITbase "2"],
       ITfun
        ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "1"));
          ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
        ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun
      ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "1"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
      ITfun ([ITbase "2"],
       ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
        ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun
      ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
      ITfun ([ITbase "1"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0")))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun
      ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
        ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
      ITfun ([ITbase "0"], ITfun ([], ITbase "q0")))))]);
 ("Copy",
  [ITfun ([ITpair1 (ITbase "0")], ITbase "q1");
   ITfun
    ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
      ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
      ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
      ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2")))));
      ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
    ITbase "q0");
   ITfun
    ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
      ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
      ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
      ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
    ITbase "q0");
   ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
    ITbase "q1");
   ITfun
    ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
      ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
    ITbase "q1");
   ITfun
    ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
      ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
      ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
    ITbase "q0")]);
 ("C",
  [ITfun ([ITfun ([ITpair1 (ITbase "0")], ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITbase "0"], ITfun ([], ITbase "q0"))));
   ITfun
    ([ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITbase "0"], ITfun ([], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITbase "1"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun ([ITbase "2"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2")))));
         ITpair2
          (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
     ITfun ([ITbase "2"],
      ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITbase "2"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun ([ITbase "2"],
      ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))));
   ITfun
    ([ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITbase "2"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITbase "2"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITbase "2"],
      ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))))]);
 ("Merge",
  [ITfun ([ITfun ([ITpair1 (ITbase "0")], ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITpair1 (ITbase "0")], ITbase "q0")));
   ITfun
    ([ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun
      ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2")))));
         ITpair2
          (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun
      ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
        ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun
      ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
      ITbase "q0")));
   ITfun
    ([ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "0")], ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
      ITbase "q0")))]);
 ("C2",
  [ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITbase "1"],
      ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2")))));
         ITpair2
          (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITbase "1"],
      ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITbase "1"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITbase "1"],
      ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))));
   ITfun
    ([ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITbase "0"], ITfun ([], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITbase "2"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITbase "2"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))))]);
 ("Sort",
  [ITfun
    ([ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITbase "q0"));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITbase "q0"));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
     ITbase "q0"));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2")))));
         ITpair2
          (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
       ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1")))));
       ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
     ITbase "q0"));
   ITfun
    ([ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITbase "q0"));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITbase "q0"))]);
 ("Sort_and_merge",
  [ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITpair1 (ITbase "1"));
       ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
       ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITbase "q0"));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITpair1 (ITbase "2"));
       ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
       ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
     ITbase "q0"));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2")))));
         ITpair2
          (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITpair1 (ITbase "2"));
       ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
       ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
       ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
       ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
     ITbase "q0"));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITpair1 (ITbase "2"));
       ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
       ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITbase "q0"))]);
 ("Sort_and_merge_sub",
  [ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun
      ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2")))));
         ITpair2
          (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun
      ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
        ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
      ITbase "q0")))]);
 ("S", [ITbase "q0"]);
 ("Cons2",
  [ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")], ITbase "q0"));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITbase "q0"))]);
 ("Sortsub",
  [ITfun
    ([ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0")],
    ITfun ([ITbase "1"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITbase "1"],
     ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun ([ITbase "1"],
     ITfun
      ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "1"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2")))));
         ITpair2
          (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
       ITbase "q0")],
    ITfun ([ITbase "1"],
     ITfun
      ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
        ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
        ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
      ITbase "q0")));
   ITfun
    ([ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0")],
    ITfun ([ITbase "2"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITbase "2"],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
      ITbase "q0")))]);
 ("Cons1",
  [ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITbase "q0"));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITbase "q0"));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2")))));
         ITpair2
          (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
       ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
     ITbase "q0"));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITbase "q0"));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
     ITbase "q0"))]);
 ("Div",
  [ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "1"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "1"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "0")],
      ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "1"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun ([ITpair1 (ITbase "0")],
      ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
     ITfun ([ITpair1 (ITbase "0")],
      ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun ([ITpair1 (ITbase "0")],
      ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "0")],
      ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "0")));
         ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun ([ITpair1 (ITbase "0")],
      ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun
      ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
      ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun
      ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "1"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
      ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
       ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1")))));
       ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
     ITfun ([ITpair1 (ITbase "0")],
      ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
     ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
      ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITpair1 (ITbase "2"));
         ITpair1 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair1 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))));
         ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
       ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
     ITfun ([ITpair1 (ITbase "0")],
      ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))))]);
 ("Sort_and_merge_sub2",
  [ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2")))));
         ITpair2
          (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
     ITfun
      ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
      ITbase "q0")))]);
 ("Sortsub3",
  [ITfun
    ([ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0")],
    ITfun ([ITbase "1"], ITfun ([ITbase "0"], ITfun ([], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITbase "1"],
     ITfun ([ITbase "1"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun ([ITbase "1"],
     ITfun ([ITbase "2"],
      ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2")))));
         ITpair2
          (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
       ITbase "q0")],
    ITfun ([ITbase "1"],
     ITfun ([ITbase "2"],
      ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0"))));
   ITfun
    ([ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0")],
    ITfun ([ITbase "2"], ITfun ([ITbase "0"], ITfun ([], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITbase "2"],
     ITfun ([ITbase "2"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))))]);
 ("G",
  [ITfun ([ITbase "0"], ITfun ([], ITbase "q1"));
   ITfun ([ITbase "0"], ITfun ([], ITbase "q0")); (** manually added **)
   ITfun ([ITbase "1"],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
       ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
     ITbase "q0"));
   ITfun ([ITbase "1"],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
     ITbase "q0"));
   ITfun ([ITbase "2"], ITfun ([ITpair1 (ITbase "0")], ITbase "q1"));
   ITfun ([ITbase "2"],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITbase "q1"));
   ITfun ([ITbase "2"],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITbase "q0"))]);
 ("Sortsub2",
  [ITfun
    ([ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")], ITfun ([ITbase "1"], ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITbase "1"], ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun ([ITbase "1"], ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "2")))));
         ITpair2
          (ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "2")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
       ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
     ITfun ([ITbase "1"], ITbase "q0")));
   ITfun
    ([ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")], ITfun ([ITbase "2"], ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "2"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "2"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITbase "2"], ITbase "q0")))])];;
(**
let t1 = List.assoc "G" te1;;
let (x1,m1) = ty2automaton t1 empty_automaton;;
**)
let (mte1, m1') = te2automaton te1 empty_automaton;;
let (mcte1, m1) = te2automaton cte0 m1';;
let x = diff_in_te mte1 m1;;
let q = merge_type m1  44 22 [(([45; 22], Tpair), 1)];;
(**
let mte2 = gen_te mte1 m1 [q];;
let mte2' = List.map
    (fun (f,ty) -> if f="Copy"||f="G" then (f,ty) else (f,[])) mte2;;
let resTE te l = List.map
    (fun (f,ty) -> if List.mem f l then (f,ty) else (f,[])) te;;
let mte2' = resTE mte2 ["Copy";"G";"Cons1";"C2";"Cons2"];;
let rec unionTE te1 te2 = 
  match te1 with
     [] -> te2
   | (f,ty)::te1' ->
        let (ty2,te2') = list_assoc2 f te2 in
          (f, ty@ty2)::(unionTE te1' te2')

let mte3 = compute_te m1 mte2' mcte1 mte1 dmap0 g0;;
let mte4 = unionTE mte1 mte3;;
let mte5 = resTE mte2 ["C"; "Merge"];;
let mte6 = compute_te m1 mte5 mcte1 mte4 dmap0 g0;;
**)

(**
let m2 = gen_automaton m1 [54;121];;
let mte2 = expand_te mte1 m2;;
let mte3 = compute_te m2 mte2 mcte1 nte0 dmap0 g0;;
**)