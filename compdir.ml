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

(** ocamlbuild and dune compilation directory tweak *)

open Utils

module FP = Filepath

let rec find_dot_ocamlspot fp = 
  let open FP in
  match
    let dot_ocamlspot = fp ^/ ".ocamlspot" in
    if Sys.file_exists (FP.to_string dot_ocamlspot) then
        match (Dotfile.load (FP.to_string dot_ocamlspot)).Dotfile.build_dir with
        | Some dir -> Some (fp, dir)
        | None -> None
    else if Unix.is_dir FP.(to_string (fp ^/ "_build")) then Some (fp, "_build")
    else None
  with
  | (Some _ as res) -> res
  | None ->
      if FP.is_root fp then None
      else match FP.dirbase fp with
      | dir, Some _ -> find_dot_ocamlspot dir
      | _ -> None

let comp_dir fp0 =
  if not  (FP.is_absolute fp0) then fp0
  else
    match find_dot_ocamlspot fp0 with
    | None -> fp0
    | Some (dir, mv) ->
        match FP.is_prefix dir fp0 with
        | None -> fp0
        | Some postfixes ->
            match FP.(is_prefix (dir ^/ mv) fp0) with
            | Some _ -> fp0 (* already in the comp dir *)
            | None -> FP.concats dir (mv :: postfixes)

let comp_dir x =
  let y = comp_dir x in
  if x <> y then
    Format.eprintf "comp_dir: %s => %s@." (FP.to_string x) (FP.to_string y);
  y

let comp_dir = Hashtbl.memoize (Hashtbl.create 107) comp_dir

let src_file fp0 =
  if not (FP.is_absolute fp0) then fp0
  else
    match find_dot_ocamlspot fp0 with
    | None -> fp0
    | Some (dir, mv) ->
        match FP.(is_prefix (dir ^/ mv) fp0) with
        | None -> fp0
        | Some postfixes -> FP.concats dir postfixes

let src_file x =
  let y = src_file x in
  if x <> y then
    Format.eprintf "src_file: %s => %s@." (FP.to_string x) (FP.to_string y);
  y

(* No need of memoize
let src_file = Hashtbl.memoize (Hashtbl.create 107) src_file
*)

module F = Filename

type conf = 
  { build_dir : string option
  ; module_prefix : string option
  ; dir_level : int
  }

let load_conf dir =
  match Dotfile.find dir with
  | None -> None
  | Some (n, _, f) -> 
      let tbl = Dotfile.load' f in
      let build_dir = try Hashtbl.find tbl "build_dir" with _ -> None in
      let module_prefix = try Hashtbl.find tbl "module_prefix" with _ -> None in
      Some { build_dir; module_prefix; dir_level = n }

let cmfile_location confopt f = match F.extension f with
  | ".cmi" | ".cmti" | ".cmo" | ".cmt" | ".cmx" | ".spit" | ".spot" -> 
      F.remove_extension f
  | _ -> 
      match confopt with
      | None -> F.remove_extension f
      | Some { build_dir; module_prefix; dir_level } ->
          let rec split_dir f = function
            | 0 -> f, ""
            | i -> 
                let d, rest = split_dir (F.dirname f) (i-1) in
                d, F.concat rest (F.basename f)
          in
          let dir = F.dirname f in
          let dir = match build_dir with
            | None -> dir
            | Some bdir ->
                let d1, d2 = split_dir dir dir_level in
                F.concat (F.concat d1 bdir) d2
          in
          let name = F.remove_extension @@ F.basename f in
          let name = match module_prefix with
            | None -> name
            | Some s -> s ^ "__" ^ String.capitalize_ascii name
          in
          F.concat dir name

let () =
  assert (cmfile_location (Some {build_dir= Some "_build/default"; module_prefix= Some "tiny_json"; dir_level= 0}) "/z/a/b/c/a.ml" = "/z/a/b/c/_build/default/tiny_json__A");
  assert (cmfile_location (Some {build_dir= Some "_build/default"; module_prefix= Some "tiny_json"; dir_level= 1}) "/z/a/b/c/a.ml" = "/z/a/b/_build/default/c/tiny_json__A");
  assert (cmfile_location (Some {build_dir= Some "_build/default/"; module_prefix= Some "tiny_json"; dir_level= 1}) "/z/a/b/c/a.ml" = "/z/a/b/_build/default/c/tiny_json__A")

let cmfile_location f = cmfile_location (load_conf @@ F.dirname f) f
