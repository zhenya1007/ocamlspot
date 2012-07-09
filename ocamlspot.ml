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

(* module names may corride in different source/spot files *)

open Format
open Utils

open Ext

open Spot
open Spoteval

module C = Spotconfig

module File = struct
  include Spotfile
  include Spotfile.Make(C)
end

module SAbs = Spot.Abstraction

module Dump = struct
  (* mainly debugging purpose *)

  let file = Spot.File.dump
  let unit = Spot.Unit.dump

  let rannots_full unit = 
    eprintf "@[<2>rannots =@ [ @[<v>%a@]@] ]@."
      (Format.list ";@ " (Regioned.format (Format.list ";@ " Annot.format)))
      !!(unit.Unit.rannots)
  ;;
  
  let rannots_summary unit = 
    eprintf "@[<2>rannots =@ [ @[<v>%a@]@] ]@."
      (Format.list ";@ " (Regioned.format (Format.list ";@ " Annot.summary)))
      !!(unit.Unit.rannots)
  ;;
  
  let tree unit = Tree.dump !!(unit.Unit.tree)
  ;;

  let top file = 
    eprintf "@[<2>top =@ @[%a@]@]@." 
      Abstraction.format_structure file.Unit.top;
    let str = 
      Eval.structure (File.empty_env file) file.Unit.top
    in
    if C.eager_dump then begin
      let module Enforcer = Value.Enforcer(struct end) in
      Enforcer.structure str;
    end;
    eprintf "==>@.@[%a@]@." Value.Format.structure str
  ;;

  let flat file = 
    eprintf "@[<2>flat =@ @[%a@]@]@." 
      Abstraction.format_structure !!(file.Unit.flat);
    let str = 
      let env = File.invalid_env file in
      let str = Eval.structure env !!(file.Unit.flat) in
      Binding.set env.Env.binding str; (* dirty hack (dup code) *)
      str
    in
    if C.eager_dump then begin
      let module Enforcer = Value.Enforcer(struct end) in
      Enforcer.structure str;
    end;
    eprintf "==>@.@[%a@]@." Value.Format.structure str;
  ;;
end

module Main = struct

  let bye return =
    printf "BYE!@.";
    exit return

  let load path =

    let file = File.load ~load_paths: ["."] path in
    
    if C.dump_file then Dump.unit file; (* CR jfuruse: to be fixed *)
    if C.dump_rannots = `Full then Dump.rannots_full file;
    if C.dump_rannots = `Summary then Dump.rannots_summary file;
    if C.dump_tree then Dump.tree file;
    if C.dump_top then Dump.top file;
    if C.dump_flat then Dump.flat file;

    file
  ;;

  let info path =
    let file = load (Cmt.of_path path) in
    printf "Compile: %s@."
      (String.concat " " 
         (List.map Command.escaped_for_shell 
            (Array.to_list file.Unit.args)));
    printf "@[<v2>Included_dirs:@ %a@]@."
      (Format.list "" pp_print_string)
      file.Unit.loadpath

  let write_spot path =
    let file = load (Cmt.of_path path) in
    let spot = match Filename.split_extension path with
      | body, (".cmi" | ".mli" | ".cmti") -> body ^ ".spit"
      | body, _ -> body ^ ".spot"
    in
    Spot.File.save spot (Spot.Unit.to_file file)

  let query_by_kind_path file kind path = 
    try Some (File.find_path_in_flat file (kind, path)) with Not_found -> None
  ;;

  let print_query_result kind = function
    | None -> printf "Spot: no spot@."
    | Some (pident, res) -> match res with
	| File.File_itself ->
            printf "Spot: <%s:all>@." pident.PIdent.path
	| File.Found_at region ->
            printf "Spot: <%s:%s>@."
              pident.PIdent.path
              (Region.to_string region)
	| File.Predefined ->
            printf "Spot: %a: predefined %s@."
              PIdent.format pident
              (Kind.name kind);
  ;;
    
  let query_by_pos file pos = 
    let probe = Region.point pos in
    let treepath = 
      (* subtree is not used *)
      List.map fst (Tree.find_path_contains probe !!(file.Unit.tree))
    in
    match treepath with
    | [] -> failwith (Printf.sprintf "nothing at %s" (Position.to_string pos))
    | { Regioned.region = r; _ } :: _ ->
	
	(* find annots bound to the region *)
        let annots = 
	  List.concat_map (fun rannot ->
	    if Region.compare r rannot.Regioned.region = `Same then 
	      rannot.Regioned.value
	    else [])
	    treepath
        in

	(* annots and region improvement by path *)
	let annots, r = 
	  match 
	    (* only the first Use *)
	    List.find_map_opt (function
	      | Annot.Use (_, path) -> 
		  (* Find subpath *)
		  begin match Pathreparse.get file.Unit.path r pos path with    
		  | None -> None
		  | Some (path', r) -> 
		      if path = path' then None (* as original *)
		      else Some ([Annot.Use (Kind.Module, path')], r)
		  end
	      | _ -> None) annots
	  with
	  | None -> annots, r
	  | Some (annots, r) -> annots, r
	in
	  
        List.iter (printf "@[<v>%a@]@." Annot.format) annots;

	(* Tree is an older format. XTree is a newer which is the same as one for Spot *)
        printf "Tree: %s@." (Region.to_string r);
        printf "XTree: <%s:%s>@." file.Unit.path (Region.to_string r);

	(* Find the innermost module *)
        let find_module_path treepath = List.concat_map (fun { Regioned.value = annots } ->
          List.filter_map (function 
            | Annot.Str (Abstraction.AStr_module (id, _)) -> Some id
            | _ -> None) annots) treepath
        in
        printf "In_module: %s@."
          (String.concat "." (List.map Ident0.name (List.rev (find_module_path treepath))));

        (* print "Val: val name : type" if it is a Str: val *)
        let print_sig_entry annots =
          let rec find_type = function
            | Annot.Type (typ, _, _) :: _ -> Some typ
            | _::xs -> find_type xs
            | [] -> None
          in
          let rec find_str_value = function
            | Annot.Str (Abstraction.AStr_value id) :: _ -> Some id
            | _::xs -> find_str_value xs
            | [] -> None
          in
          match find_type annots, find_str_value annots with
          | Some typ, Some id ->
              printf "Val: val %s : @[%a@]@."
                (Ident0.name id)
                (Printtyp.type_scheme ~with_pos:false) typ
          | _ -> ()
        in
        print_sig_entry annots;

        (* print_type_decl: if one Type is found *)
        if C.type_expand then begin
          match List.filter (function Annot.Type _ -> true | _ -> false) annots with
          (* CR jfuruse: Sometimes more than one Annot.Type are found at the same place... *)
          | Annot.Type (typ, env, `Expr) :: _ -> 
              printf "Expand: @[%a@]@." Typeexpand.format_as_expr (Typeexpand.expand file.Unit.loadpath env typ)
          | Annot.Type (typ, env, `Pattern) :: _ -> 
              printf "Expand: @[%a@]@." Typeexpand.format_as_pattern (Typeexpand.expand file.Unit.loadpath env typ)
          | Annot.Type (_typ, _env, `Val) :: _ -> ()
          | _ -> ()
        end;

	annots
  ;;

  let query path spec = 
    (* CR jfuruse: dup *)
    Debug.format "ocamlspot %s%s@." path (C.SearchSpec.to_string spec);
    Debug.format "cwd: %s@." (Sys.getcwd ());
    let path = Cmt.of_path path in
    let file = load path in

    let query_kind_path k path = print_query_result k (query_by_kind_path file k path) in

    begin match spec with
    | C.SearchSpec.Kind (k,path) -> query_kind_path k path
    | C.SearchSpec.Pos pos -> 
	let annots = query_by_pos file pos in
        if not C.no_definition_analysis then begin
          List.iter (function
            | Annot.Use (k, path) -> query_kind_path k path
            | _ -> ()) annots
        end
    end;

    bye 0

  let query file spec =
    try query file spec with
    | Failure s ->
        eprintf "Error: %s@." s;
        bye 1
    | File.Old_cmt (_spot, source) ->
        eprintf "Error: source %s is newer than the spot@." source;
        bye 1
    | e ->
        eprintf "uncaught exception: %s@." (Printexc.to_string e);
        bye 1

  let use path spec targets =
    let targets = if targets = [] then ["."] else targets in
    (* CR jfuruse: dup *)
    Debug.format "ocamlspot %s%s@." path (C.SearchSpec.to_string spec);
    Debug.format "cwd: %s@." (Sys.getcwd ());
    let path = Cmt.of_path path in
    let file = load path in

    let find_by_kind_path k path found =
      Unix.find targets ~f:(fun pathname ->
	match Filename.split_extension pathname.Unix.base with
	| _body, (".cmti" | ".cmt") ->
	  let file = load pathname.Unix.path in
	  Debug.format "Searching %s@." pathname.Unix.path;
	  let base_ident = function
	    | Path.Pident id -> Ident0.name id
	    | Path.Pdot (_, name, _) -> name
	    | Path.Papply _ -> assert false
	  in
	  let base = base_ident path in
	  List.iter (fun { Regioned.region= region; value= annots } -> 
                List.iter (function
                  | Annot.Use (k', path') when k = k' && base = base_ident path' ->
	              begin match query_by_kind_path file k' path' with
	              | Some found' when found = found' ->
		          printf "<%s:%s>: %s@." 
		            file.Unit.path
		            (Region.to_string region)
		            (Path.name path)
	              | None | Some _ -> ()
	              end
                  | _ -> ()) annots) !!(file.Unit.rannots)
	| _ -> ());
    in

    let by_kind_path file k path =
      Debug.format "Searching %s:%s:%s ...@." 
	file.Unit.path 
	(Kind.to_string k) 
	(Path.name path); 
      let res = query_by_kind_path file k path in
      print_query_result k res;
      match res with
      | None -> printf "No query result found.@.";
      | Some found -> find_by_kind_path k path found
    in

    let by_pos file pos = 
      eprintf "Searching %s:%s ...@." 
	file.Unit.path 
	(Position.to_string pos);
      match List.find_map_opt (function 
	| Annot.Str str_item -> 
	    begin match Abstraction.ident_of_structure_item str_item with
	    | Some v -> Some (`Def v)
	    | None -> None
	    end
	| Annot.Use (kind, path) -> Some (`Use (kind, path))
	| _ -> None) (query_by_pos file pos)
      with
      | Some (`Def (k, id))   -> by_kind_path file k (Path.Pident id)
      | Some (`Use (k, path)) -> by_kind_path file k path
      | None -> ()
    in

    begin match spec with
    | C.SearchSpec.Kind (k,path) -> by_kind_path file k path
    | C.SearchSpec.Pos pos       -> by_pos file pos
    end;
    bye 0
  ;;

(*
  let typecheck args =
    let command = Sys.argv.(0) :: args in
    prerr_endline (String.concat " " command);
    Xmain.main (Array.of_list command)
  ;;

  let recheck files =
    let recheck mlpath =
      Debug.format "cwd: %s@." (Sys.getcwd ());
      let path = Cmt.of_path mlpath in
      let file = Unit.load ~load_paths: ["."] path in
    
      printf "Compile: %s@."
        (String.concat " " 
          (List.map Command.escaped_for_shell 
            (Array.to_list file.Unit.argv)));
      let command = 
	Sys.argv.(0) :: List.tl (Array.to_list file.Unit.argv) 
      in
      Xmain.main (Array.of_list command)
    in
    List.iter recheck files
  ;;
*)

  let main () = 
    match C.mode with
    | `Dump path                   -> ignore (load path)
    | `Info path                   -> info path
    | `Spot path                   -> write_spot path
    | `Query (path, spec)          -> query path spec
(*
    | `Typecheck args              -> typecheck args
    | `Recheck args                -> recheck args
*)
    | `Use ((path, spec), targets) -> use path spec targets
    | `Typecheck _ | `Recheck _ -> assert false
end

let _ = Main.main ()

