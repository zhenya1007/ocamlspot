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

(** ocamlbuild compilation directory tweak *)

val comp_dir : Filepath.t -> Filepath.t
(** ocaml/camlp4/Camlp4 => ocaml/_build/camlp4/Camlp *)

val src_file : Filepath.t -> Filepath.t
(** ocaml/_build/camlp4/Camlp4Bin.ml => ocaml/camlp4/Camlp4Bin.ml *)
