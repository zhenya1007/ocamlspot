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
