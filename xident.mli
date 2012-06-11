open Ident

val name : t -> Name.t
val format : Format.formatter -> t -> unit
val unsafe_create_with_stamp : ?flags:int -> string -> int -> t
  (** create an ident with given flags and stamp *)
val parse : Name.t -> t
