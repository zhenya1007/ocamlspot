(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2014 Jun Furuse. All rights reserved.              *)
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
  let module FP = Filepath in
  (* CR jfuruse: we should create a function for this *)
  let path = if Filename.is_relative path then Unix.getcwd () ^/ path else path in
  let fp = FP.of_string FP.os path in
  match FP.dirbase fp with
  | _, None -> failwithf "Error: %s is not a normal file path" path
  | dir, Some base ->
      let rec find = function
        | [] -> assert false
        | [path] -> path
        | path::fps -> 
            if Sys.file_exists path then  path
            else find fps
      in
      let loc = Compdir.cmfile_location (FP.to_string fp) in
      find (match Filename.split_extension base with
          | _, (".cmi" | ".cmti" | ".spit") -> [ loc ^ ".cmti" ]
          | _, (".cmo" | ".cmx" | ".cmt" | ".spot") -> [ loc ^ ".cmt" ]
          | _, ".mli" -> [ loc ^ ".cmti" ]
          | _ -> [ loc ^ ".cmt" ])
  
    
(* CR jfuruse: this is a dirty workaround. It should be nice if we could know cmt is created by opt or byte *)          
let is_opt cmt = 
  (* We cannot guess this simply by the compiler name "ocamlc" or "ocamlopt", 
     since someone can create a modified compiler like gcaml *)
  List.exists (fun x -> match Filename.split_extension x with 
    | (_, ".cmx") -> true 
    | _ -> false) (Array.to_list cmt.cmt_args)

let reset_env_cache () = Envaux.reset_cache ()

let recover_env env = 
  Envaux.reset_cache (); (* reset required for machines with small memory... *)
  Envaux.env_from_summary (Env.summary env) Subst.identity
