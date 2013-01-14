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

  let rec f rev_bases fp = 
    let dir = FP.to_string fp in
    let ocamlbuild_path = dir ^/ "_build" in
    let diropt = 
      if Unix.is_dir ocamlbuild_path then Some ocamlbuild_path
      else None
    in
    match diropt with
    | Some _ -> Some (FP.(^/) fp (Filename.concats ("_build" :: List.rev rev_bases)))
    | None ->
        if FP.is_root fp then Some fp0
        else match FP.dirbase fp with
        | dir, Some base -> f (base :: rev_bases) dir
        | _ -> assert false
  in
  Option.default (f [] fp0) (fun () -> fp0)

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
