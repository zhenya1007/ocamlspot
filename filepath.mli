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

type t

val is_prefix : t -> t -> t option
(** contains_abs /a/b/c /a/b/c/d/e = Some ["d"; "e"] 
*)

val of_string : string -> t
val to_string : t -> string

val is_absolute : t -> bool
val is_relative : t -> bool
(* val root : t *)
val is_root : t -> bool
val dirbase : t -> t * string option
val (^/) : t -> t -> t
val parent : t -> t
val wrap : (t -> t) -> string -> string

val to_list : t -> string list
