(* This module is extended in ocamlspot, therefore it cannot be .mli *)

(* Annotations 

   Annotations are stored in .spot with their locations
*)

val magic_number : string
val ocaml_version : string
val version : string

module Location_bound : sig
  val upperbound : Location.t -> Location.t -> Location.t
end

module Kind : sig
  type t = 
    | Value | Type | Exception 
    | Module | Module_type 
    | Class | Class_type

  val to_string : t -> string
  val from_string : string -> t
  val name : t -> string
end

module Abstraction : sig

  (* module definition abstraction *)
  type module_expr = (* private *)
    | AMod_ident      of Path.t (* module M = N *)
    | AMod_packed     of string (* full path *)
        (* -pack overrides load paths: ocamlc -pack dir1/dir2/dir3/x.cmo *)
    | AMod_structure  of structure (* module M = struct ... end *)
    | AMod_functor    of Ident.t * Types.module_type * module_expr (* module M(I:S) = *)
    | AMod_apply      of module_expr * module_expr (* module M = N(O) *)
    | AMod_constraint of module_expr * Types.module_type
    | AMod_unpack     of module_expr
    | AMod_abstract (* used for Tmodtype_abstract *)

  (* structure abstraction : name - defloc asoc list *)
  and structure = structure_item list

  and structure_item = 
    | AStr_value     of Ident.t
    | AStr_type      of Ident.t
    | AStr_exception of Ident.t
    | AStr_module    of Ident.t * module_expr
    | AStr_modtype   of Ident.t * module_expr
    | AStr_class     of Ident.t
    | AStr_cltype    of Ident.t
    | AStr_include   of module_expr * (Kind.t * Ident.t) list

  val ident_of_structure_item : structure_item -> (Kind.t * Ident.t) option

  val structure : Typedtree.structure -> module_expr
  val signature : Typedtree.signature -> module_expr

  open Format
  val format_module_expr : formatter -> module_expr -> unit
  val format_structure : formatter -> structure -> unit
  val format_structure_item : formatter -> structure_item -> unit
end

module Annot : sig
  type t =
    | Type of Types.type_expr * Env.t * [`Expr | `Pattern | `Val]
    | Str of Abstraction.structure_item 
    | Use of Kind.t * Path.t
    | Module of Abstraction.module_expr
    | Functor_parameter of Ident.t
    | Non_expansive of bool
    | Mod_type of Types.module_type

  val record : Location.t -> t -> unit
    
  (* [record_constr_type_use loc ty] records a constructor use of type [ty]
     at the location [loc]. [ty] must be a constructor type, otherwise,
     an error message is printed out. 
  *)
  val record_constr_type_use : Location.t -> Types.type_expr -> unit
  val record_module_expr_def : Location.t -> Ident.t -> Typedtree.module_expr -> unit
  val record_module_expr_use : Location.t -> Typedtree.module_expr -> unit
  val record_include :
    Location.t -> Typedtree.module_expr -> (* Types.signature -> *) unit
  val record_include_sig :
    Location.t -> Typedtree.module_type -> Types.signature -> unit
  val record_module_type_def : Location.t -> Ident.t -> Typedtree.module_type -> unit
  val recorded : unit -> (Location.t * t) list

  val format : Format.formatter -> t -> unit
  val summary : Format.formatter -> t -> unit
  (** same as [format] but bigger structures are omitted *)    

  val dummy : t
end

module Top : sig
  val record_structure : Typedtree.structure -> unit
  val record_signature : Typedtree.signature -> unit

  val recorded : unit -> Abstraction.module_expr option
end

(* Spot file *)
(*
module File : sig
  type elem =
    | Argv of string array
    | Source_path of string option
    | Cwd of string
    | Load_paths of string list
    | Top of Abstraction.structure option
    | Annots of (Location.t * Annot.t) list

  (* marshalled type *)
  type t = elem list

  val dump : source: string option -> string -> unit
  val dump_package : prefix: string -> source: string -> string list -> unit

  val set_argv : string array -> unit
    (** override the original Sys.argv. Required for ocamlspot --recheck *)
end
*)

module Position : sig

  type t = { line_column : (int * int) option; 
             bytes : int option; }

  val none : t
  val compare : t -> t -> int
  val next : t -> t
  val of_lexing_position : Lexing.position -> t

  exception Parse_failure of string
  val parse : string -> t (* may raise Parse_failure *)

  val to_string : t -> string
  val is_complete : t -> bool
  val complete : string -> t -> t
end

module Region : sig

  type t = { start : Position.t; end_ : Position.t; }
  
  val compare : t -> t -> [> `Included | `Includes | `Left | `Overwrap | `Right | `Same ]

  val to_string : t -> string
  val of_parsing : Location.t -> t
  val split : t -> by:t -> (t * t) option
  val point_by_byte : int -> t  
    (** works only if bytes are available *)
  val point : Position.t -> t
  val length_in_bytes : t -> int
  val is_complete : t -> bool
  val complete : string -> t -> t
  val substring : string -> t -> t * string
end

module Regioned : sig
  type 'a t = { region : Region.t; value : 'a; }
  val compare :
    'a t ->
    'b t -> [> `Included | `Includes | `Left | `Overwrap | `Right | `Same ]
  val split : 'a t -> by:'b t -> ('a t * 'a t) option
  val format : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module Tree : sig
  type elem = Annot.t Regioned.t
  type t
  val empty : t
  val is_empty : t -> bool
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val subset : t -> t -> bool
  val cardinal : t -> int
  val add : t -> elem -> t
  val find_path_contains : Region.t -> t -> (elem * t) list

  val iter : (parent:elem option -> elem -> unit) -> t -> unit
    (** Region splitted Annot may be itered more than once. *)

  val dump : t -> unit
end

