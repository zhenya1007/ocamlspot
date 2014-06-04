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

(* module for .ocamlspot file 

   build_dir=dirname

      Work around for build systems which create object files to exotic places.
      If .ocamlspot is placed in a directory $DIR,
      then .cmt* files of source files under $DIR ex. $DIR/subdir/subdir/source.ml
      is searched in $DIR/dirname/subdir/subdir/.
*)

open Utils

let name = ".ocamlspot"

let rec find absdir =
prerr_endline absdir;
  let path = absdir ^/ name in
  if Sys.file_exists path then Some (absdir, path)
  else if absdir = "/" then None
  else find (Filename.dirname absdir)
;;

type t = {
  build_dir : string option;
}

(* very strict .ini file style format *)
let split_by_equal s =
  try
    let pos = String.index s '=' in
    let key = String.sub s 0 pos in
    let value = String.sub s (pos+1) (String.length s - pos - 1) in
    key, Some value
  with
  | Not_found -> s, None
;;

let load s =
  let build_dir = ref None in
  let set name ref v =
    match !ref with
    | Some _ -> failwithf "key %s is defined twice" name
    | None -> ref := Some v
  in
  let ic = open_in s in
  let rec load () =
    let line = input_line ic in
    let key, value = split_by_equal line in
    match key, value with
    | "build_dir", Some s -> 
        set "build_dir" build_dir s;
        load ()
    | "build_dir", None -> failwithf "key %S must have a value" key
    | key, _ -> failwithf "unknown key %S" key
  in
  try load () with End_of_file ->
    close_in ic;
    { build_dir = !build_dir }
;;

let find_and_load absdir =
  match find absdir with
  | None -> None
  | Some (dir, path) -> Some (dir, load path)
;;
