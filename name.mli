(* Name is an identifier name with its stamp number. For example, 
   an ident of name "string" with a stamp 3 has the name "string__3".

   With these names, OCamlSpotter distinguishes textual
   representations of idents with the same name but with different
   stamps.
*)
type t = string (* CR jfuruse: should be abstracted? *)
val create : string -> int -> t
val parse : t -> string * int

