type node = string
type edges = (node * node) list
val compute_scc: edges -> node list list
exception Cycle
val check_acyclicity: edges -> unit
type rgraph
val mk_rgraph: edges -> rgraph
val compute_rset: rgraph -> node -> node list
