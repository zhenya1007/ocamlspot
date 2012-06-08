open Format

val type_expr :
  ?with_pos:bool -> formatter -> Types.type_expr -> unit
val type_sch :
  ?with_pos:bool -> formatter -> Types.type_expr -> unit
val type_scheme :
  ?with_pos:bool -> formatter -> Types.type_expr -> unit
val modtype :
  ?with_pos:bool -> formatter -> Types.module_type -> unit


