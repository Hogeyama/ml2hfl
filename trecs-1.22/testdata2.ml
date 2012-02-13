let g0 =
{nt =
  [("S", O); ("Add", O); ("Addsub1", O); ("Addsub2", O); ("Copy", O);
   ("G", O)];
 t = [("s", O); ("zero", O)];
 r =
  [("S",
    ([],
     App (NT "Add",
      [App (PAIR,
        [App (FD 1, []);
         App (PAIR,
          [App (FD 1, []);
           App (PAIR,
            [App (FD 1, []); App (PAIR, [App (FD 0, []); App (FD 0, [])])])])]);
       App (PAIR,
        [App (FD 1, []); App (PAIR, [App (FD 0, []); App (FD 0, [])])])])));
   ("Add",
    (["x"; "y"],
     App (NT "Addsub1",
      [App (NT "Copy", []); App (Var "x", []); App (Var "y", [])])));
   ("Addsub1",
    (["k"; "x"; "y"],
     App (DPAIR,
      [App (Var "x", []);
       App (NT "Addsub2", [App (Var "k", []); App (Var "y", [])])])));
   ("Addsub2",
    (["k"; "y"; "c"; "x"],
     App (CASE 2,
      [App (Var "c", []); App (Var "k", [App (Var "y", [])]);
       App (NT "Addsub1",
        [App (Var "k", []); App (Var "x", []);
         App (PAIR, [App (FD 1, []); App (Var "y", [])])])])));
   ("Copy", (["x"], App (DPAIR, [App (Var "x", []); App (NT "G", [])])));
   ("G",
    (["x"; "y"],
     App (CASE 2,
      [App (Var "x", []); App (T "zero", []);
       App (T "s", [App (NT "Copy", [App (Var "y", [])])])])))];
 s = "S"}

let dmap0 =   
  [("S", []); ("Add", ["S"]); ("Addsub1", ["Add"; "Addsub2"]);
   ("Addsub2", ["Addsub1"]); ("Copy", ["Add"; "G"]); ("G", ["Copy"])]

let nte0 = 
  [("S", []); ("G", []); ("Copy", []); ("Addsub2", []); ("Addsub1", []);
   ("Add", [])]

let cte0 = 
  [("zero", [ITbase "q0"]);
   ("s",
    [ITfun ([ITbase "q0"], ITbase "q1"); ITfun ([ITbase "q1"], ITbase "q0")])]

let te0 = 
[("S", [ITbase "q0"]);
 ("G",
  [ITfun ([ITbase "0"], ITfun ([], ITbase "q0"));
   ITfun ([ITbase "1"], ITfun ([ITpair1 (ITbase "0")], ITbase "q1"));
   ITfun ([ITbase "1"],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITbase "q0"));
   ITfun ([ITbase "1"],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITbase "q1"));
   ITfun ([ITbase "1"],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
     ITbase "q0"))]);
 ("Copy",
  [ITfun ([ITpair1 (ITbase "0")], ITbase "q0");
   ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
    ITbase "q1");
   ITfun
    ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
      ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
    ITbase "q0");
   ITfun
    ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
      ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
      ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
    ITbase "q1");
   ITfun
    ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
      ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
      ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
      ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
    ITbase "q0")]);
 ("Addsub2",
  [ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun ([ITbase "1"],
      ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun ([ITbase "1"],
      ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
       ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
     ITfun ([ITbase "1"], ITfun ([ITpair1 (ITbase "0")], ITbase "q0"))));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
       ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
     ITfun ([ITbase "0"], ITfun ([], ITbase "q0"))))]);
 ("Addsub1",
  [ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "0")],
     ITfun
      ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
        ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
        ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITfun
      ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
        ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
     ITfun
      ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
        ITpair2 (ITpair2 (ITpair1 (ITbase "0")))],
      ITbase "q0")));
   ITfun
    ([ITfun
       ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
         ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
         ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "1"))));
         ITpair2 (ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0")))))],
       ITbase "q0")],
    ITfun
     ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
       ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
       ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
     ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
      ITbase "q0")))]);
 ("Add",
  [ITfun
    ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "1"));
      ITpair2 (ITpair2 (ITpair1 (ITbase "1")));
      ITpair2 (ITpair2 (ITpair2 (ITpair1 (ITbase "0"))))],
    ITfun ([ITpair1 (ITbase "1"); ITpair2 (ITpair1 (ITbase "0"))],
     ITbase "q0"))])]

(**
let t1 = List.assoc "G" te1;;
let (x1,m1) = ty2automaton t1 empty_automaton;;
**)
let (mte1, m1') = te2automaton te0 empty_automaton;;
let (mcte1, m1) = te2automaton cte0 m1';;
let x = diff_in_te mte1 m1;;
(**
let q = merge_type m1  31 24 [(([21; 28], Tpair), 1); (([21; 24], Tpair), 1)];;
let mte2 = gen_te mte1 m1 [q];;
let resTE te l = List.map
    (fun (f,ty) -> if List.mem f l then (f,ty) else (f,[])) te;;
let rec unionTE te1 te2 = 
  match te1 with
     [] -> te2
   | (f,ty)::te1' ->
        let (ty2,te2') = list_assoc2 f te2 in
          (f, ty@ty2)::(unionTE te1' te2')

let mte3 = compute_te m1 mte2 mcte1 mte1 dmap0 g0;;
**)

(**
let m2 = gen_automaton m1 [54;121];;
let mte2 = expand_te mte1 m2;;
let mte3 = compute_te m2 mte2 mcte1 nte0 dmap0 g0;;
**)