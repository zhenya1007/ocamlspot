open Path

val name : t -> Name.t
val local : t -> bool
    (** return true if "local" *)
val parse : Name.t -> t
val format : Format.formatter -> t -> unit
