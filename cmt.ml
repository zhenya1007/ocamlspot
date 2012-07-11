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
open Utils

open Cmt_format

let source_path file = 
  Option.map file.cmt_sourcefile ~f:(fun f -> file.cmt_builddir ^/ f)

(* xxx.{ml,cmo,cmx,spot} => xxx.cmt
   xxx.{mli,cmi,spit}    => xxx.cmti *)
let of_path path =
  let dirname, filename =
    try
      let slash = String.rindex path '/' in
      Some (String.sub path 0 slash),
      String.sub path (slash + 1) (String.length path - slash - 1)
    with
    | Not_found -> None, path
  in
  let filename =
    match Filename.split_extension filename with
    | body, (".cmi" | ".mli" | ".cmti" | ".spit") -> body ^ ".cmti"
    | body, _ -> body ^ ".cmt"
  in
  match dirname with
  | None -> filename
  | Some d -> d ^/ filename

(* CR jfuruse: this is a dirty workaround. It should be nice if we could know cmt is created by opt or byte *)          
let is_opt cmt = 
  (* We cannot guess this simply by the compiler name "ocamlc" or "ocamlopt", 
     since someone can create a modified compiler like gcaml *)
  List.exists (fun x -> match Filename.split_extension x with 
    | (_, ".cmx") -> true 
    | _ -> false) (Array.to_list cmt.cmt_args)
