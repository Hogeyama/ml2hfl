
val merge_typ :
  CEGAR_syntax.typ -> CEGAR_syntax.typ -> CEGAR_syntax.t CEGAR_type.t

val add_neg_preds_renv : CEGAR_syntax.env -> CEGAR_syntax.env

val trans_var : 'a Id.t -> string
val trans_inv_var : string -> 'a Type.t Id.t
val trans_inv_term : CEGAR_syntax.t -> Syntax.typed_term
val trans_typ : CEGAR_util.S.typ -> CEGAR_syntax.t CEGAR_type.t
val trans_binop : CEGAR_util.S.binop -> CEGAR_syntax.t
val trans_const :
  CEGAR_util.S.const -> CEGAR_util.S.typ -> CEGAR_syntax.const
val formula_of : CEGAR_util.S.typed_term -> CEGAR_syntax.t
val trans_def :
  CEGAR_util.S.typ Id.t *
  (CEGAR_util.S.typ Id.t list * CEGAR_util.S.typed_term) ->
  (string * CEGAR_syntax.t CEGAR_type.t * CEGAR_syntax.var list *
   CEGAR_syntax.t * 'a list * CEGAR_syntax.t)
  list

val get_var_arity : 'a -> ('a * CEGAR_syntax.t CEGAR_type.t) list -> int

val is_CPS_value :
  (CEGAR_syntax.var * CEGAR_syntax.t CEGAR_type.t) list ->
  CEGAR_syntax.t -> bool

val is_CPS_def :
  (CEGAR_syntax.var * CEGAR_syntax.t CEGAR_type.t) list ->
  CEGAR_syntax.var * CEGAR_syntax.var list * CEGAR_syntax.t * 'a *
  CEGAR_syntax.t -> bool

val is_CPS : CEGAR_syntax.prog -> bool

val event_of_temp : CEGAR_syntax.prog -> CEGAR_syntax.prog

val uniq_env : ('a * 'b) list -> ('a * 'b) list

val rename_prog :
  ?is_cps:bool ->
  CEGAR_syntax.prog ->
  CEGAR_syntax.prog * (CEGAR_syntax.var * CEGAR_syntax.var) list *
  (CEGAR_syntax.var * 'a Type.t Id.t) list

val id_prog :
  CEGAR_syntax.prog ->
  CEGAR_syntax.prog * (CEGAR_syntax.var * CEGAR_syntax.var) list *
  (CEGAR_syntax.var * 'a Type.t Id.t) list

val trans_ref_type : CEGAR_ref_type.t -> Ref_type.t

val trans_term :
  CEGAR_util.S.typed_term ->
  (string * CEGAR_syntax.t CEGAR_type.t * CEGAR_syntax.var list *
   CEGAR_syntax.t * 'a list * CEGAR_syntax.t)
  list * CEGAR_syntax.t

val trans_prog :
  ?spec:('a Id.t * CEGAR_util.S.typ) list ->
  Syntax.typed_term ->
  CEGAR_syntax.prog * (CEGAR_syntax.var * CEGAR_syntax.var) list *
  (CEGAR_syntax.var * 'b Type.t Id.t) list *
  (Syntax.typ Id.t -> CEGAR_ref_type.t -> Ref_type.t)

val is_value :
  (CEGAR_syntax.var * CEGAR_syntax.t CEGAR_type.t) list ->
  CEGAR_syntax.t -> bool

val read_bool : unit -> bool

val step_eval_abst_cbn :
  int list ->
  CEGAR_syntax.var list ->
  'a ->
  (CEGAR_syntax.var * CEGAR_syntax.var list * CEGAR_syntax.t *
   CEGAR_syntax.event list * CEGAR_syntax.t)
  list -> CEGAR_syntax.t -> int list * CEGAR_syntax.t

val eval_abst_cbn :
  CEGAR_syntax.prog ->
  CEGAR_syntax.var list -> CEGAR_syntax.prog -> int list -> unit

val trans_ce :
  int list -> CEGAR_syntax.var list -> CEGAR_syntax.prog -> int list

val simplify_if : CEGAR_syntax.prog -> CEGAR_syntax.prog
