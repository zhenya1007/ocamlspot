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

open Utils

module F = Filename

let get_component : string -> string = 
  Hashtbl.memoize (Hashtbl.create 1023) (fun x -> x)

let dotdot = get_component ".."

let rec split st path = 
  let dir  = F.dirname  path in
  let base = F.basename path in
  let st = 
    let base = 
      if base = ""
      || base = F.current_dir_name 
      || base = F.dir_sep || base = "/" then ""
      else base
    in
    get_component base :: st
  in
  if dir = F.current_dir_name then false, st
  else if dir = F.dir_sep || dir = "/" then true, st
  else split st dir

let split = split []
      
let () = 
  match Sys.os_type with
  | "Unix" -> 
      assert (split ""       = (false, [""]));
      assert (split "."      = (false, [""]));
      assert (split "/"      = (true, [""]));
      assert (split "/."     = (true, [""]));
      assert (split "./"     = (false, [""]));
      assert (split ".."     = (false, [".."]));
      assert (split "/.."    = (true, [".."]));
      assert (split "/.."    = (true, [".."]));
      assert (split "//"     = (true, [""]));
      assert (split "///"    = (true, [""]));
      assert (split "a/"     = (false, ["a"]));
      assert (split "a/."    = (false, ["a"; ""]));
      assert (split "a/b"    = (false, ["a"; "b"]));
      assert (split "a/b/"   = (false, ["a"; "b"]));
      assert (split "a/b/."  = (false, ["a"; "b"; ""]));
      assert (split "a//b/." = (false, ["a"; "b"; ""]))
  | _ -> ()

let rec rev_normalize rev abs = function
  | [] -> rev
  | x::xs ->
      if x = F.dir_sep || x = F.current_dir_name then assert false;
      if x = F.parent_dir_name then
        match rev with
        | [] -> rev_normalize (if abs then [] else [F.parent_dir_name]) abs xs
        | r::_ when r = F.parent_dir_name -> rev_normalize (x::rev) abs xs
        | _::rs -> rev_normalize rs abs xs
      else if x = "" then rev_normalize rev abs xs
      else rev_normalize (x::rev) abs xs
      
let hashcons_list = 
  let cache = Hashtbl.create 1023 in
  let rec f xs = Hashtbl.memoize cache (function
    | [] -> []
    | x::xs -> x :: f xs) xs
  in
  f

let rev_normalize rev abs xs = hashcons_list (rev_normalize rev abs xs)

let () = 
  assert (rev_normalize [] false [] = []);
  assert (rev_normalize [] false [""] = []);
  assert (rev_normalize [] false [""; ""] = []);
  assert (rev_normalize [] false ["a"; ""] = ["a"]);
  assert (rev_normalize [] false [""; "a"] = ["a"]);
  assert (rev_normalize [] false ["a"; ""] == rev_normalize [] false [""; "a"]);
  assert (rev_normalize [] false ["a"; ".."] = []);
  assert (rev_normalize [] false ["a"; ".."; "b"] = ["b"]);
  assert (rev_normalize [] false [".."; "a"] = ["a"; ".."]);
  assert (rev_normalize [] false [".."; "a"; ".."] = [".."]);
  assert (rev_normalize [] false [".."; "a"; "b"; ".."; ".."; "c"] = ["c"; ".."]);
  assert (rev_normalize [] false [".."; "a"; ".."; ".."; "c"] = ["c"; ".."; ".."]);
  assert (rev_normalize [] true  [".."; "a"] = ["a"]);
  assert (rev_normalize [] true  [".."; "a"; ".."] = []);
  assert (rev_normalize [] true  [".."; "a"; "b"; ".."; ".."; "c"] = ["c"]);
  assert (rev_normalize [] true  [".."; "a"; ".."; ".."; "c"] = ["c"]);

type t = bool * string list

let equal (b1,xs1) (b2,xs2) = b1 = b2 && xs1 == xs2

let compare t1 t2 = if equal t1 t2 then 0 else compare t1 t2

let of_string path = 
  let abs, xs = split path in
  abs, rev_normalize [] abs xs

let to_string (abs, rev) =
  let xs = List.rev rev in
  let xs = if abs then "" :: xs else xs in
  if xs = [] then "." else String.concat (F.dir_sep) xs

let is_absolute (abs, _) = abs
let is_relative (abs, _) = not abs

let root = (true, [])
let is_root = function 
  | (true, []) -> true
  | _ -> false

let dirbase (abs, xs) = match xs with
  | [] | ".." :: _ -> (abs, xs), None
  | x::xs -> (abs, xs), Some x

let (^/) (abs, xs) s = 
  let abs', ys = split s in
  assert (not abs');
  abs, rev_normalize xs abs ys

let parent = function
  | (false, (".."::_ | [] as xs)) -> (false, dotdot :: xs)
  | (true, []) -> (true, [])
  | (abs, (_::xs)) -> (abs, xs)

