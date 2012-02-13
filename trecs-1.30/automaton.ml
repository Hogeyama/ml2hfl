open Utilities;;
open Grammar;;

type state = string
type transfunc = ((state * nameT) * state list) list
type automaton = {alpha: terminals;
                  st: state list;
                  delta: transfunc;
                  init: state
                 }

let next_state q a m =
  List.assoc (q,a) m.delta

let size_st m = List.length (m.st)

(*** example ***)
(*** a*b*c ***)
let m1 = {alpha=t1; st = ["q0"; "q1"]; 
          delta = [(("q0", "a"), ["q0"; "q0"]);
                   (("q0", "b"), ["q1"]);
                   (("q1", "b"), ["q1"]);
                   (("q0", "c"), []);
                   (("q1", "c"), [])];
          init = "q0"}

(*** a*b+c ***)
let m2 = {alpha=t1; st = ["q0"; "q1"]; 
          delta = [(("q0", "a"), ["q0"; "q0"]);
                   (("q0", "b"), ["q1"]);
                   (("q1", "b"), ["q1"]);
                   (("q1", "c"), [])];
          init = "q0"}

(*** a+b*c ***)
let m3 = {alpha=t1; st = ["q0"; "q1"; "q2"]; 
          delta = [(("q0", "a"), ["q1"; "q1"]);
                   (("q1", "a"), ["q1"; "q1"]);
                   (("q1", "b"), ["q2"]);
                   (("q2", "b"), ["q2"]);
                   (("q1", "c"), []);
                   (("q2", "c"), [])];
          init = "q0"}
                   
                  
let m4 = {alpha=t4; st = ["q0"; "q1"; "q2"];
          delta = [(("q0", "br"), ["q0"; "q0"]);
                   (("q1", "br"), ["q1"; "q1"]);
                   (("q2", "br"), ["q2"; "q2"]);
                   (("q0", "new"), ["q1"]);
                   (("q1", "read"), ["q1"]);
                   (("q1", "close"), ["q2"]);
                   (("q0", "d"), []);
                   (("q2", "d"), [])];
            init = "q0"}

                   
let m5 = {alpha=t5; st = ["q0"; "qr"; "qw"; "qc"; "qrw"];
          delta = [(("q0", "br"), ["q0"; "q0"]);
                   (("qr", "br"), ["qr"; "qr"]);
                   (("qw", "br"), ["qw"; "qw"]);
                   (("qrw", "br"), ["qrw"; "qrw"]);
                   (("q0", "newr"), ["qr"]);
                   (("qr", "read"), ["qr"]);
                   (("qr", "close"), ["qc"]);
                   (("q0", "neww"), ["qw"]);
                   (("qw", "write"), ["qw"]);
                   (("qw", "close"), ["qc"]);
                   (("qw", "newr"), ["qrw"]);
                   (("qr", "neww"), ["qrw"]);
                   (("qrw", "read"), ["qrw"]);
                   (("qrw", "write"), ["qrw"]);
                   (("qrw", "close"), ["qrw"]);
                   (("qc", "d"), []);
                   (("q0", "d"), []);
                   (("qrw", "d"), [])];
          init = "q0"}