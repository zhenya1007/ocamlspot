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
  let module FP = Filepath in
  (* CR jfuruse: we should create a function for this *)
  let path = if Filename.is_relative path then Unix.getcwd () ^/ path else path in
  let fp = FP.of_string FP.os path in
  match FP.dirbase fp with
  | _, None -> failwithf "Error: %s is not a normal file path" path
  | dir, Some base ->
      let rec find = function
        | [] -> assert false
        | [fp] -> FP.to_string fp
        | fp::fps -> 
            let path = FP.to_string fp in
            if Sys.file_exists path then  path
            else find fps
      in
      find (match Filename.split_extension base with
      | body, (".cmi" | ".cmti" | ".spit") -> [ FP.(^/) dir (body ^ ".cmti") ]
      | body, (".cmo" | ".cmx" | ".cmt" | ".spot") -> [ FP.(^/) dir (body ^ ".cmt") ]
      | body, ".mli" -> 
          [ FP.(^/) (Compdir.comp_dir dir ) (body ^ ".cmti");
            FP.(^/) dir (body ^ ".cmti"); ]
      | body, _ (* .ml, mll, mly, or eliom *) -> 
          [ FP.(^/) (Compdir.comp_dir dir ) (body ^ ".cmt");
            FP.(^/) dir (body ^ ".cmt") ])

(* CR jfuruse: this is a dirty workaround. It should be nice if we could know cmt is created by opt or byte *)          
let is_opt cmt = 
  (* We cannot guess this simply by the compiler name "ocamlc" or "ocamlopt", 
     since someone can create a modified compiler like gcaml *)
  List.exists (fun x -> match Filename.split_extension x with 
    | (_, ".cmx") -> true 
    | _ -> false) (Array.to_list cmt.cmt_args)

(*
(* CR jfuruse: This module should be removed once OCaml compilier-libs has the env restoration function *)
module Envaux = struct (* copied from debugger/envaux.ml *)
  open Misc
  open Types
  open Env
  
  type error =
      Module_not_found of Path.t
  
  exception Error of error
  
  let env_cache =
    (Hashtbl.create 59 : ((Env.summary * Subst.t), Env.t) Hashtbl.t)
  
  let reset_cache () =
    Hashtbl.clear env_cache;
    Env.reset_cache()
  
  let extract_sig env mty =
    match Mtype.scrape env mty with
      Mty_signature sg -> sg
    | _ -> fatal_error "Envaux.extract_sig"
  
  let rec env_from_summary sum subst =
    try
      Hashtbl.find env_cache (sum, subst)
    with Not_found ->
      let env =
        match sum with
          Env_empty ->
            Env.empty
        | Env_value(s, id, desc) ->
            Env.add_value id (Subst.value_description subst desc) (env_from_summary s subst)
        | Env_type(s, id, desc) ->
            Env.add_type id (Subst.type_declaration subst desc) (env_from_summary s subst)
        | Env_exception(s, id, desc) ->
            Env.add_exception id (Subst.exception_declaration subst desc) (env_from_summary s subst)
        | Env_module(s, id, desc) ->
            Env.add_module id (Subst.modtype subst desc) (env_from_summary s subst)
        | Env_modtype(s, id, desc) ->
            Env.add_modtype id (Subst.modtype_declaration subst desc) (env_from_summary s subst)
        | Env_class(s, id, desc) ->
            Env.add_class id (Subst.class_declaration subst desc) (env_from_summary s subst)
        | Env_cltype (s, id, desc) ->
            Env.add_cltype id (Subst.cltype_declaration subst desc) (env_from_summary s subst)
        | Env_open(s, path) ->
            let env = env_from_summary s subst in
            let path' = Subst.module_path subst path in
            let mty =
              try
                Env.find_module path' env
              with Not_found ->
                raise (Error (Module_not_found path'))
            in
            Env.open_signature path' (extract_sig env mty) env
      in
        Hashtbl.add env_cache (sum, subst) env;
        env
end 
*)

let reset_env_cache () = Envaux.reset_cache ()

let recover_env env = 
  Envaux.reset_cache (); (* reset required for machines with small memory... *)
  Envaux.env_from_summary (Env.summary env) Subst.identity
