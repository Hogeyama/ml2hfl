
val hd : 'a list -> 'a
val check_aux : 'a -> CEGAR_syntax.t list -> CEGAR_syntax.t -> bool
val check :
  'a ->
  CEGAR_syntax.t list -> (CEGAR_syntax.t * 'b) list -> CEGAR_syntax.t -> bool
val equiv :
  'a -> CEGAR_syntax.t list -> CEGAR_syntax.t -> CEGAR_syntax.t -> bool
val satisfiable : 'a -> CEGAR_syntax.t list -> CEGAR_syntax.t -> bool
val print_pb : Format.formatter -> CEGAR_syntax.t * CEGAR_syntax.t -> unit
val print_pbs :
  Format.formatter -> (CEGAR_syntax.t * CEGAR_syntax.t) list -> unit
val min_unsat_cores :
  'a ->
  CEGAR_syntax.t list ->
  (CEGAR_syntax.t * CEGAR_syntax.t) list -> CEGAR_syntax.t
val weakest :
  'a ->
  CEGAR_syntax.t list ->
  (CEGAR_syntax.t * CEGAR_syntax.t) list ->
  CEGAR_syntax.t -> CEGAR_syntax.t * CEGAR_syntax.t
val filter_pbs :
  'a ->
  CEGAR_syntax.t list ->
  (CEGAR_syntax.t * 'b) list -> (CEGAR_syntax.t * 'b) list
val filter :
  'a ->
  CEGAR_syntax.t list ->
  (CEGAR_syntax.t * CEGAR_syntax.t) list ->
  CEGAR_syntax.t -> CEGAR_syntax.t
val abst :
  'a ->
  CEGAR_syntax.t list ->
  (CEGAR_syntax.t * CEGAR_syntax.t) list -> CEGAR_syntax.t -> CEGAR_syntax.t
val assume :
  'a ->
  CEGAR_syntax.t list ->
  (CEGAR_syntax.t * CEGAR_syntax.t) list ->
  CEGAR_syntax.t -> CEGAR_syntax.t -> CEGAR_syntax.t
val congruent :
  (string * CEGAR_syntax.typ) list ->
  CEGAR_syntax.t list -> CEGAR_syntax.typ -> CEGAR_syntax.typ -> bool
val is_base_term :
  (CEGAR_syntax.var * 'a CEGAR_type.t) list -> CEGAR_syntax.t -> bool
val make_arg_let_term : CEGAR_syntax.t -> CEGAR_syntax.t
val reduce_let : CEGAR_syntax.env -> CEGAR_syntax.t -> CEGAR_syntax.t
val make_arg_let_def :
  (CEGAR_syntax.var * CEGAR_syntax.typ) list ->
  CEGAR_syntax.var * CEGAR_syntax.var list * 'a * 'b * CEGAR_syntax.t ->
  CEGAR_syntax.var * CEGAR_syntax.var list * 'a * 'b * CEGAR_syntax.t
val make_arg_let : CEGAR_syntax.prog -> CEGAR_syntax.prog
val has_branch : CEGAR_syntax.prog -> CEGAR_syntax.var list
val add_label :
  CEGAR_syntax.prog -> CEGAR_syntax.var list * CEGAR_syntax.prog
val use_arg :
  CEGAR_syntax.var ->
  CEGAR_syntax.t CEGAR_type.t -> CEGAR_syntax.t -> CEGAR_syntax.t
val make_ext_fun : CEGAR_syntax.t CEGAR_type.t -> CEGAR_syntax.t
val add_ext_funs : CEGAR_syntax.prog -> CEGAR_syntax.prog
val check_exist :
  'a -> CEGAR_syntax.t list -> CEGAR_syntax.var -> CEGAR_syntax.t -> bool
