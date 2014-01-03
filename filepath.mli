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

(* File path normalization *)

type os = 
  | Unix (** We love *)
  | Win32 (** We hate *)
  | Cygwin (** a failed effort of reconcillation *)

val os : os

type t

val is_prefix : t -> t -> string list option
(** contains_abs /a/b/c /a/b/c/d/e = Some ["d"; "e"] 
*)

val of_string : os -> string -> t
val to_string : t -> string

val is_absolute : t -> bool
val is_relative : t -> bool
(* val root : t *)
val is_root : t -> bool
val dirbase : t -> t * string option
val (^/) : t -> string -> t
val concats : t -> string list -> t
val parent : t -> t
val wrap : os -> (t -> t) -> string -> string

val test : unit -> unit
