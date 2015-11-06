open CEGAR_syntax

val merge_typ :
  typ -> typ -> t CEGAR_type.t

val add_neg_preds_renv : env -> env

val trans_var : 'a Id.t -> string
val trans_inv_var : string -> 'a Type.t Id.t
val trans_inv_term : t -> Syntax.typed_term
val trans_typ : Syntax.typ -> t CEGAR_type.t
val trans_binop : Syntax.binop -> t
val trans_const :
  Syntax.const -> Syntax.typ -> const
val formula_of : Syntax.typed_term -> t
val trans_def :
  Syntax.typ Id.t *
  (Syntax.typ Id.t list * Syntax.typed_term) ->
  (string * t CEGAR_type.t * var list *
   t * 'a list * t)
  list

val get_var_arity : 'a -> ('a * t CEGAR_type.t) list -> int

val is_CPS_value :
  (var * t CEGAR_type.t) list ->
  t -> bool

val is_CPS_def :
  (var * t CEGAR_type.t) list ->
  var * var list * t * 'a *
  t -> bool

val is_CPS : prog -> bool

val event_of_temp : prog -> prog

val uniq_env : ('a * 'b) list -> ('a * 'b) list

val rename_prog :
  prog ->
  prog * (var * var) list *
  (var * 'a Type.t Id.t) list

val id_prog :
  prog ->
  prog * (var * var) list *
  (var * 'a Type.t Id.t) list

val trans_ref_type : CEGAR_ref_type.t -> Ref_type.t

val trans_term :
  Syntax.typed_term ->
  (string * t CEGAR_type.t * var list *
   t * 'a list * t)
  list * t

val trans_prog :
  ?spec:('a Id.t * Syntax.typ) list ->
  Syntax.typed_term ->
  prog * (var * var) list *
  (var * 'b Type.t Id.t) list *
  (Syntax.typ Id.t -> CEGAR_ref_type.t -> Ref_type.t)

val is_value :
  (var * t CEGAR_type.t) list ->
  t -> bool

val read_bool : unit -> bool

val step_eval_abst_cbn :
  int list ->
  var list ->
  'a ->
  (var * var list * t *
   event list * t)
  list -> t -> int list * t

val eval_abst_cbn :
  prog ->
  var list -> prog -> int list -> unit

val trans_ce :
  var list -> prog -> int list -> int list

val simplify_if : prog -> prog

val add_env : Syntax.env -> prog -> prog
