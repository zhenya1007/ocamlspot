open Ext

module type S = sig
  val app_version            : string
  
  val version                : string
  val print_version          : unit -> unit
  
  val dump_file              : bool
  val dump_rannots           : [ `None | `Full | `Summary ]
  val dump_tree              : bool
  val dump_top               : bool
  val dump_flat              : bool
  val dump_any               : bool
  val eager_dump             : bool
  
  val no_definition_analysis : bool
  
  val strict_time_stamp      : bool
  
  val print_interface        : bool
  val type_expand            : bool

  module SearchSpec : sig
    type t =
      | Pos  of Spot.Position.t
      | Kind of Spot.Kind.t * Path.t
    val parse : string -> string * t
    val to_string : t -> string
  end
  
  val mode : [ `Dump      of string
             | `Info      of string
             | `Query     of string * SearchSpec.t
             | `Use       of (string * SearchSpec.t) * string list
             | `Recheck   of string list
             | `Typecheck of string list ]
end
