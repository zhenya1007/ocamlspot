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

open Utils

module FP = Filepath

let comp_dir fp0 =
  assert (FP.is_absolute fp0);
  let rec f bases fp = 
    let dir = FP.to_string fp in

    let fix () =
      let dot_ocamlspot = 
        let dot_ocamlspot_path = dir ^/ ".ocamlspot" in
        if Sys.file_exists dot_ocamlspot_path then
          match (Dotfile.load dot_ocamlspot_path).Dotfile.build_dir with
          | (Some _ as res) -> res
          | None -> None
        else None
      in
      match dot_ocamlspot with
      | (Some _ as res) -> res
      | None ->
          let ocamlbuild_path = dir ^/ "_build" in
          if Unix.is_dir ocamlbuild_path then Some "_build"
          else None
    in
    match fix () with
    | Some p -> Some (FP.(^/) fp (Filename.concats (p :: bases)))
    | None -> 
        if FP.is_root fp then Some fp0
        else match FP.dirbase fp with
        | dir, Some base -> f (base :: bases) dir
        | _ -> assert false
  in
  Option.default (f [] fp0) (fun () -> fp0)

let comp_dir x =
  let y = comp_dir x in
  Format.eprintf "comp_dir: %s => %s@." (FP.to_string x) (FP.to_string y);
  y

let comp_dir = Hashtbl.memoize (Hashtbl.create 107) comp_dir

let src_dir fp0 =
  assert (FP.is_absolute fp0);

  Debug.format "Compdir.src_dir: %s@." (FP.to_string fp0);

  let rec f dirs fp = 
    match FP.dirbase fp with
    | dir, Some "_build" -> FP.(^/) dir (Filename.concats dirs)
    | _, None -> fp0
    | dir, Some x -> f (x::dirs) dir
  in
  
  let res = f [] fp0 in
  Debug.format "Compdir.src_dir => %s@." (FP.to_string res); 
  res

let src_dir = Hashtbl.memoize (Hashtbl.create 107) src_dir
