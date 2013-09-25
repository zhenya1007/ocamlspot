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

val compare : t -> t -> int
val equal : t -> t -> bool

val contains_abs : t -> t -> string list option
(** contains_abs /a/b/c /a/b/c/d/e = Some ["d"; "e"] 
    Only works for absolute paths
*)

val of_string : string -> t
val to_string : t -> string
val is_absolute : t -> bool
val is_relative : t -> bool
val root : t
val is_root : t -> bool
val dirbase : t -> t * string option
val (^/) : t -> string -> t
val parent : t -> t
