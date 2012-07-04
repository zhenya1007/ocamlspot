(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2012 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)


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
    | AMod_functor_parameter

  (* structure abstraction : name - defloc asoc list *)
  and structure = structure_item list

  and structure_item = 
    | AStr_value      of Ident.t
    | AStr_type       of Ident.t
    | AStr_exception  of Ident.t
    | AStr_module     of Ident.t * module_expr
    | AStr_modtype    of Ident.t * module_expr
    | AStr_class      of Ident.t
    | AStr_class_type of Ident.t
    | AStr_include    of module_expr * (Ident.t * (Kind.t * Ident.t)) list
    | AStr_included   of Ident.t * module_expr * Kind.t * Ident.t

  val ident_of_structure_item : structure_item -> (Kind.t * Ident.t) option

  val structure : Typedtree.structure -> module_expr
  val signature : Typedtree.signature -> module_expr

  val clear_cache : unit -> unit

  open Format
  val format_module_expr : formatter -> module_expr -> unit
  val format_structure : formatter -> structure -> unit
  val format_structure_item : formatter -> structure_item -> unit
end

module Annot : sig
  type t =
    | Use of Kind.t * Path.t
    | Type of Types.type_expr * Env.t * [`Expr | `Pattern | `Val]
    | Mod_type of Types.module_type
    | Str of Abstraction.structure_item 
    | Module of Abstraction.module_expr
    | Functor_parameter of Ident.t
    | Non_expansive of bool

  val record_structure : Typedtree.structure -> (Location.t, int * t list) Hashtbl.t
  val record_signature : Typedtree.signature -> (Location.t, int * t list) Hashtbl.t

  val format : Format.formatter -> t -> unit
  val summary : Format.formatter -> t -> unit
  (** same as [format] but bigger structures are omitted *)    

  val dummy : t
end

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
  type elem = Annot.t list Regioned.t
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

