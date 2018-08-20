(* a module to implement module file search *)

open Utils

let find_with_opened_modules ~load_paths ~opened_modules n =
  let rec f = function
    | [] -> None
    | o::os ->
        let n = String.uncapitalize_ascii o ^ "__" ^ String.capitalize_ascii n in
(*
        Format.eprintf "%s in load_paths %s@."
          n
          (String.concat " " load_paths);
*)
        match Misc.find_in_path load_paths n with
        | x -> Some x
        | exception _ -> f os
  in
  match Misc.find_in_path load_paths n with
  | x -> Some x
  | exception _ -> f opened_modules
  
let cmt_of p =
  prerr_endline ("cmt_of " ^ p);
  let open Filename in
  let dir  = dirname p in
  let base = basename p in
  let base_body, base_ext = split_extension base in
  let cm_ext = match base_ext with
    | ".mli" -> Some ".cmti"
    | ".ml" | ".mll" | ".mly" -> Some ".cmt"
    | _      -> Some ".cmt"
  in
  let default () = 
    match cm_ext with
    | Some cm_ext ->
        let path = dir ^/ base_body ^ cm_ext in
        if Sys.file_exists path then Some path else None
    | None -> None
  in
  match base_ext with
  | ".cmt" | ".cmti" -> Some p
  | ".cmo" | ".cmx"  -> Some (dir ^/ base_body ^ ".cmt")
  | ".cmi" -> Some (dir ^/ base_body ^ ".cmti")
  | ".mli" ->
      begin match Dot_merlin.load_path_of_dir dir with
        | Some (load_paths, opened_modules) -> 
            begin match find_with_opened_modules ~load_paths ~opened_modules (base_body ^ ".cmti") with
            | None -> default ()
            | Some x -> Some x
            end
        | None -> default ()
      end
  | ".ml" | ".mll" | ".mly" | _ ->
      begin match Dot_merlin.load_path_of_dir dir with
        | Some (load_paths, opened_modules) -> 
            begin match find_with_opened_modules ~load_paths ~opened_modules (base_body ^ ".cmt") with
            | None -> default ()
            | Some x -> Some x
            end
        | None -> default ()
        end

let cmt_of p = match cmt_of p with
  | None -> assert false
  | Some x -> x
