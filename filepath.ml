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

open Filename

let get_component : string -> string = Hashtbl.memoize (Hashtbl.create 1023) (fun x -> x)

let dotdot = get_component (parent_dir_name)

let root_normalize = 
  match Sys.os_type with
  | "Win32" ->
      (* "c:/" => "C:\\" *)
      (fun s -> 
        let s = String.uppercase s in
        String.iteri (fun p -> function
          | '/' -> String.unsafe_set s p '\\'
          | _ -> ()) s;
        s)
  | "Cygwin" ->
      (* "C:\\" => "c:/" *)
      (fun s ->
        let s = String.lowercase s in
        String.iteri (fun p -> function
          | '\\' -> String.unsafe_set s p '/'
          | _ -> ()) s;
        s)
  | _ -> (fun s -> s)

let rec split st s =
  let d = dirname s in
  if d = s then get_component (root_normalize s) :: st
  else 
    let b = basename s in
    split (get_component b :: st) d

let split = split []

let rec concats = function
  | [] -> current_dir_name
  | x::xs when not (is_relative x) -> x ^ concats xs
  | xs -> String.concat dir_sep xs

let test s = 
  let ss = split s in
  (* Format.eprintf "%s => %s@." s (concats ss); *)
  ss

let () =
  assert (test "" = ["."]);
  assert (test "." = ["."]);
  assert (test "a/b/c" = ["."; "a"; "b"; "c"]);
  assert (test "/a/b/c" = ["/"; "a"; "b"; "c"]);
  assert (test "//a//b//c" = ["/"; "a"; "b"; "c"]);
  assert (test "/a/./b/./c/" = ["/"; "a"; "."; "b"; "."; "c"]);
  
  if Sys.os_type = "Win32" then begin
    assert (test "\\a\\b\\c" = ["\\"; "a"; "b"; "c"]);
    assert (test "c:\\a\\b\\c" = ["C:\\"; "a"; "b"; "c"]);
    assert (test "c:/a/b/c" = ["C:\\"; "a"; "b"; "c"]);
  end

let hashcons_list = 
  let cache = Hashtbl.create 1023 in
  let rec f xs = Hashtbl.memoize cache (function
    | [] -> []
    | x::xs -> x :: f xs) xs
  in
  f

let rec normalize' rev = function
  | [] -> hashcons_list (List.rev rev)
  | x::xs when x = current_dir_name -> normalize' rev xs
  | x::xs when x = parent_dir_name -> 
      begin match rev with
      | r::_ when not (is_relative r) -> normalize' rev xs
      | r::_ when r = parent_dir_name || r = current_dir_name -> normalize' (x::rev) xs
      | _::rev -> normalize' rev xs
      | [] -> normalize' [x] xs
      end
  | x::xs -> normalize' (x::rev) xs
          
let normalize = normalize' []

let () =
  let test s = 
    let ss = split s in
    let nss = normalize ss in
    Format.eprintf "%s => %s => %s@." s (concats ss) (concats nss);
    nss
  in

  assert (test "a/b/c" = ["a"; "b"; "c"]);
  assert (test "/a/b/c" = ["/"; "a"; "b"; "c"]);
  assert (test "//a//b//c" = ["/"; "a"; "b"; "c"]);
  assert (test "/a/./b/./c/" = ["/"; "a"; "b"; "c"]);
  assert (test "/a/../b/../c/" = ["/"; "c"]);
  assert (test "../../a/../b" = [".."; ".."; "b"]);
    
      assert (test ""       = []);
      assert (test "."      = []);
      assert (test "/"      = ["/"]);
      assert (test "/."     = ["/"]);
      assert (test "./"     = [""]);
      assert (test ".."     = [".."]);
      assert (test "/.."    = ["/"]);
      assert (test "//"     = ["/"]);
      assert (test "///"    = ["/"]);
      assert (test "a/"     = ["a"]);
      assert (test "a/."    = ["a"]);
      assert (test "a/b"    = ["a"; "b"]);
      assert (test "a/b/"   = ["a"; "b"]);
      assert (test "a/b/."  = ["a"; "b"]);
      assert (test "a//b/." = ["a"; "b"]);

  if Sys.os_type = "Win32" then begin
    assert (test "\\a\\b\\c" = ["\\"; "a"; "b"; "c"]);
    assert (test "c:\\a\\b\\c" = ["c:\\"; "a"; "b"; "c"]);
    assert (test "c:/a/b/c" = ["c:/"; "a"; "b"; "c"]);
    assert (test "\\a\\..\\\\b\\\\..\\c\\" = ["\\"; "c"]);
    assert (test "c:a/../b" = ["c:."; "b"]);
  end

type t = string list

let rec is_prefix xs ys =
  match xs, ys with
  | [], ys -> Some ys
  | x::xs, y::ys when x = y -> is_prefix xs ys
  | _ -> None

let () = assert (is_prefix ["a"; "b"; "c"] ["a"; "b"; "c"; "d"; "e"] = Some ["d"; "e"])

let of_string path = normalize (split path)

let to_string = concats

let is_absolute = function
  | [] -> false
  | x::_ -> not (is_relative x)

let is_relative xs = not (is_absolute xs)

let is_root = function
  | ([_] as xs) -> is_absolute xs
  | _ -> false

let dirbase = function
  | [] -> [], None
  | xs ->
      let rec dirbase rev = function
        | [] -> assert false
        | [x] -> List.rev rev, Some x
        | x::xs -> dirbase (x::rev) xs
      in
      dirbase [] xs

let (^/) xs ys = 
  assert (is_relative ys);
  normalize' (List.rev xs) ys

let parent = function
  | [] -> [parent_dir_name]
  | [x] when is_absolute [x] -> [x]
  | [x] when dirname x = x -> [x; dotdot]
  | xs ->
      let rec parent rev = function
        | [] -> assert false
        | [x] when x = parent_dir_name -> List.rev (parent_dir_name :: x :: rev)
        | [_] -> List.rev rev
        | x::xs -> parent (x::rev) xs
      in
      parent [] xs

let wrap f s = to_string (f (of_string s))

let to_list xs = xs
