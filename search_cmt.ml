(* a module to implement module file search *)

open Utils

let cmt_of p =
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
        | Some load_paths -> 
            begin match Misc.find_in_path load_paths (base_body ^ ".cmti") with
              | x -> Some x
              | exception Not_found -> default ()
            end
        | None -> default ()
      end
  | ".ml" | ".mll" | ".mly" | _ ->
      begin match Dot_merlin.load_path_of_dir dir with
        | Some load_paths -> 
            begin match Misc.find_in_path load_paths (base_body ^ ".cmt") with
              | x -> Some x
              | exception Not_found -> default ()
            end
        | None -> default ()
        end

let cmt_of p = match cmt_of p with
  | None -> assert false
  | Some x -> x
