(***********************************************************************)
(*                                                                     *)
(*                            ocamlspotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008, 2009 Jun Furuse. All rights reserved.             *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

(* module names may corride in different source/spot files *)

open Format
open Utils

(* Keep the original modules *)
module Ident0 = Ident

open Spot
open Spoteval
open Cmt_format

type file = {
  cmt            : Cmt_format.cmt_infos;
  path           : string; (** source path. If packed, the .cmo itself *)
  flat           : Abstraction.structure;
  top            : Abstraction.structure;
  id_def_regions : (Ident.t, Region.t) Hashtbl.t lazy_t;
  rannots        : Annot.t list Regioned.t list lazy_t;
  tree           : Tree.t lazy_t
}

let source_path_of_cmt file = match file.cmt_sourcefile with 
  | Some f -> Some (Filename.concat file.cmt_builddir f)
  | None -> None

let dump_file file =
  eprintf "@[<v2>{ module= %S;@ path= %S;@ source= %S;@ builddir= %S;@ loadpath= [ @[%a@] ];@ argv= [| @[%a@] |];@ ... }@]@."
    file.cmt.cmt_modname
    file.path
    (match file.cmt.cmt_sourcefile with Some s -> s | None -> "???")
    file.cmt.cmt_builddir
    (Format.list ";@ " (fun ppf s -> fprintf ppf "%S" s)) file.cmt.cmt_loadpath
    (Format.list ";@ " (fun ppf s -> fprintf ppf "%S" s)) (Array.to_list file.cmt.cmt_args)

(* xxx.{ml,cmo,cmx,spot} => xxx.spot 
   xxx.{mli,cmi,spit} => xxx.spit *)
let cmt_of_file file =
  let dirname, filename =
    try
      let slash = String.rindex file '/' in
      Some (String.sub file 0 slash),
      String.sub file (slash + 1) (String.length file - slash - 1)
    with
    | Not_found -> None, file
  in
  let filename =
    match Filename.split_extension filename with
    | body, (".cmi" | ".mli" | ".cmti") -> body ^ ".cmti"
    | body, _ -> body ^ ".cmt"
  in
  match dirname with
  | None -> filename
  | Some d -> Filename.concat d filename

let abstraction_of_cmt cmt = match cmt.cmt_annots with
  | Implementation str -> 
      let loc_annots = Spot.Annot.record_structure str in
      begin match Abstraction.structure str with
      | Abstraction.AMod_structure str -> str, loc_annots
      | _ -> assert false
      end
  | Interface sg -> 
      let loc_annots = Spot.Annot.record_signature sg in
      begin match Abstraction.signature sg with
      | Abstraction.AMod_structure str -> str, loc_annots
      | _ -> assert false
      end
  | Packed (_sg, files) ->
      (List.map (fun file ->
        let fullpath = if Filename.is_relative file then Filename.concat cmt.cmt_builddir file else file in
        let modname = match Filename.split_extension (Filename.basename file) with 
          | modname, (".cmo" | ".cmx") -> String.capitalize modname
          | _ -> assert false
        in
        Abstraction.AStr_module (Ident.create modname (* stamp is bogus *),
                                 Abstraction.AMod_packed fullpath)) files),
      (Hashtbl.create 1 (* empty *))
  | Partial_implementation _parts | Partial_interface _parts -> assert false

let abstraction_of_cmt cmt = 
  try abstraction_of_cmt cmt with e -> 
    Format.eprintf "Aiee %s@." (Printexc.to_string e);
    raise e

module Make(Spotconfig : Spotconfig_intf.S) = struct
  (* open Abstraction *)

  module Load : sig
    exception Old_cmt of string (* cmt *) * string (* source *)
    val load : load_paths:string list -> string -> file
    val load_module : ?spit:bool -> load_paths:string list -> string -> file
  end = struct

    let check_time_stamp ~cmt source =
      let stat_cmt = Unix.stat cmt in
      let stat_source = Unix.stat source in
        (* Needs = : for packed modules, .cmt and the source .cmo are written 
           almost at the same moment. *)
      stat_cmt.Unix.st_mtime >= stat_source.Unix.st_mtime

    let find_alternative_source ~cmt source =
        (* if [source] is not found, we try finding files with the same basename
           in
           - the directory of [cmt]
           - the directory of [cmt] points to (if [cmt] is symlink)
         *)
      let source_base = Filename.basename source in
      let source_dirs =
          Filename.dirname cmt ::
          begin 
            let stat_cmt = Unix.lstat cmt in
            if stat_cmt.Unix.st_kind = Unix.S_LNK then
              [ Filename.dirname (Unix.readlink cmt) ]
            else []
          end
        in
        List.find Sys.file_exists 
          (List.map (fun d -> 
            Filename.concat d source_base) source_dirs)

    let load_cmt_file file = snd (Cmt_format.read file)

    let load_directly path : file =
      Debug.format "cmt loading from %s@." path;
      match load_cmt_file path with
      | Some cmt -> 
          (* CR jfuruse: all things are not always required. so inefficient *)
          Debug.format "cmt loaded from %s@." path;
          Debug.format "cmt loaded now extracting things from %s ...@." path;
          let str, loc_annots = abstraction_of_cmt cmt in
          Debug.format "cmt loaded: abstraction extracted from %s@." path;

          (* CR jfuruse: this is a dirty workaround. It should be nice if we could know cmt is created by opt or byte *)          
          let cm_extension = 
            if List.exists (fun x -> match Filename.split_extension x with (_, ".cmx") -> true | _ -> false) (Array.to_list cmt.cmt_args)
            then ".cmx" else ".cmo"
          in

          let path = Option.default (Filename.chop_extension path ^ cm_extension) (source_path_of_cmt cmt) in
          let rannots = lazy (Hashtbl.fold (fun loc (_,annots) st -> 
            { Regioned.region = Region.of_parsing loc;  value = annots } :: st) loc_annots [])
          in
          Debug.format "cmt loaded: rannots created from %s@." path;
          let id_def_regions = lazy (
            let tbl = Hashtbl.create 1023 in
            Hashtbl.iter (fun loc (_,annots) ->
              List.iter (function
                | Annot.Str sitem ->
                    Option.iter (Abstraction.ident_of_structure_item sitem) ~f:(fun (_kind, id) ->
                      Hashtbl.add tbl id (Region.of_parsing loc))
                | _ -> ()) annots) loc_annots;
            tbl)
          in
          Debug.format "cmt loaded: id_def_regions created from %s@." path;
          let tree = lazy begin
            Hashtbl.fold (fun loc (_, annots) st ->
              Tree.add st { Regioned.region = Region.of_parsing loc; value = annots })
              loc_annots Tree.empty 
          end in
          (* CR jfuruse: it is almost the same as id_def_regions_list *)
          let flat = Hashtbl.fold (fun _loc (_, annots) st -> 
            List.filter_map (function
              | Annot.Str sitem -> Some sitem
              | _ -> None) annots @ st) loc_annots []
          in
          Debug.format "cmt loaded: flat created from %s@." path;
          Debug.format "cmt analysis done from %s@." path;
          { cmt; path;
            top = str;
            flat;
            id_def_regions;
            rannots;
            tree;
          }
      | None -> failwith (sprintf "load_directly failed: %s" path)

    exception Old_cmt of string (* cmt *) * string (* source *)

    (* CR jfuruse: exception *)
    (* CRv2 jfuruse: add and check cache time stamp *)
    let load_directly_with_cache : string -> file = 
      let cache = Hashtbl.create 17 in
      fun path ->
        try 
          Hashtbl.find cache path
        with
        | Not_found ->
              try
                let file = load_directly path in
                if not (check_time_stamp ~cmt:path file.path) then 
                  if Spotconfig.strict_time_stamp then 
                    raise (Old_cmt (path, file.path))
                  else
                    eprintf "Warning: source %s is newer than the cmt@." file.path;
                Hashtbl.replace cache path file;
                file
              with
              | Not_found ->
                  failwith (Printf.sprintf "failed to find cmt file %s" path)

    let find_in_path load_paths body ext =
        let body_ext = body ^ ext in
      let find_in_path load_paths name = 
        try Misc.find_in_path load_paths name with Not_found ->
          Misc.find_in_path_uncap load_paths name
      in
      try find_in_path load_paths body_ext with Not_found ->
      (* We do not give up yet.
         .cmt file is not found, 
         but we still find a .cmi which is sym-linked to the original directory with .cmt
      *)
      let cminame = body ^ ".cmi" in
        try
        let cmipath = find_in_path load_paths cminame in
        let stat = Unix.lstat cmipath in
        if stat.Unix.st_kind = Unix.S_LNK then begin
          let cmipath = Filename.dirname cmipath ^/ Unix.readlink cmipath in
          let cmtpath = Filename.chop_extension cmipath ^ ext in
          if Sys.file_exists cmtpath then begin
            Debug.format "Found an alternative %s: %s@." ext cmtpath;
              cmtpath 
            end else failwith (Printf.sprintf "cmt file not found: %s, neither in %s" body_ext cmtpath)
          end else raise Not_found
        with
        | (Failure _ as e) -> raise e
        | _ -> failwith (Printf.sprintf "cmt file not found: %s" body_ext)
      

    let load ~load_paths cmtname : file =
      Debug.format "@[<2>cmt searching %s in@ paths [@[%a@]]@]@." 
          cmtname
          (Format.list "; " (fun ppf x -> fprintf ppf "%S" x)) 
          load_paths;
        let body, ext = Filename.split_extension cmtname in
      let path = find_in_path load_paths body ext in
      load_directly_with_cache path

    let load ~load_paths cmtname : file =
      let alternate_cmtname = 
        if Filename.is_relative cmtname then None
        else
          Option.bind (Dotfile.find_and_load (Filename.dirname cmtname)) 
            (fun (found_dir, dotfile) ->
              Option.map dotfile.Dotfile.build_dir ~f:(fun build_dir ->
                let length_found_dir = String.length found_dir in
                let found_dir' = 
                  String.sub cmtname 0 length_found_dir
                in
                let rel_cmtname =
                  String.sub cmtname 
                    (length_found_dir + 1)
                    (String.length cmtname - length_found_dir - 1)
                in
                assert (found_dir = found_dir');
                let dir = 
                  if Filename.is_relative build_dir then 
                    Filename.concat found_dir build_dir
                  else build_dir
                in
                Filename.concat dir rel_cmtname))
      in
      try load ~load_paths cmtname with
      | e -> 
          match alternate_cmtname with
          | Some cmtname -> load ~load_paths cmtname
          | None -> raise e

    (* CR jfuruse: searching algorithm must be reconsidered *)        
    let load_module ?(spit=false) ~load_paths name =
      let cmtname = name ^ if spit then ".cmti" else ".cmt" in
      try
        load ~load_paths cmtname
      with
      | Failure s ->
          let spitname = name ^ if spit then ".cmt" else ".cmti" in
          Format.printf "%s load failed. Try to load %s@."
            cmtname spitname;
          try
            load ~load_paths spitname
          with
          | Failure s' ->
                (* CR jfuruse: ugly! *)
              raise (Failure (s ^ "\n" ^ s'))
  end

  include Load

  let empty_env file =
    { Env.path = file.path;
      cwd = file.cmt.cmt_builddir;
      load_paths = file.cmt.cmt_loadpath;
      binding = Binding.empty }

  let invalid_env file =
    { Env.path = file.path;
      cwd = file.cmt.cmt_builddir;
      load_paths = file.cmt.cmt_loadpath;
      binding = Binding.invalid }
      
  type result =
      | File_itself
      | Found_at of Region.t
      | Predefined

  let find_path_in_flat file path : PIdent.t * result =
    let env = 
      let env = invalid_env file in
      let str = Eval.structure env file.flat in
      Binding.set env.Env.binding str; (* dirty hack *)
      env
    in
    let find_loc pid =
      match  pid.PIdent.path with
      | "" -> Predefined
      | path ->
          (* CR jfuruse: loading twice... *)
          Debug.format "Finding %a@." PIdent.format pid;
          let file = Load.load ~load_paths:[] (cmt_of_file path) in
          match pid.PIdent.ident with
          | None -> File_itself (* the whole file *)
          | Some id -> 
              Found_at begin try
                Hashtbl.find !!(file.id_def_regions) id
              with
              | Not_found ->
                  eprintf "Error: find location of id %a failed@."
                    PIdent.format pid;
                  raise Not_found
              end
    in
    
    let eval_and_find path =
      (* we need evaluate the path *)
      let v = !!(Eval.find_path env path) in
      Debug.format "Value=%a@." Value.Format.t v;
      match v with
      | Value.Ident id -> id, find_loc id
      | Value.Parameter id -> id, find_loc id
      | Value.Structure (id, _, _)  -> id, find_loc id
      | Value.Closure (id, _, _, _, _) -> id, find_loc id
      | Value.Error (Failure _ as e) -> raise e
      | Value.Error (Load.Old_cmt _ as exn) -> raise exn
      | Value.Error exn -> raise exn
    in
    eval_and_find path

  let str_of_global_ident ~load_paths id =
    assert (Ident.global id);
    let file = Load.load_module ~spit:Spotconfig.print_interface ~load_paths (Ident0.name id) in
    file.path,
    Eval.structure (empty_env file) file.top

  let _ = Eval.str_of_global_ident := str_of_global_ident

  let eval_packed env file =
    let f = Load.load ~load_paths:[""] (cmt_of_file (env.Env.cwd ^/ file)) in
    Value.Structure ({ PIdent.path = f.path; ident = None },
                    Eval.structure (empty_env f) f.top,
                    None (* packed has no .mli *))

  let _ = Eval.packed := eval_packed

(*
  let dump_elem = function
    | Source_path (Some s) -> eprintf "Source_path: %s@." s
    | Source_path None -> eprintf "Source_path: None@." 
    | Cwd s -> eprintf "Cwd: %s@." s 
    | Load_paths ds -> 
        eprintf "Load_paths: @[%a@]@."
          (Format.list "; " (fun ppf s -> fprintf ppf "%S" s)) ds
    | Argv argv ->
        eprintf "Argv: @[%a@]@."
          (Format.list "; " (fun ppf s -> fprintf ppf "%S" s)) 
            (Array.to_list argv)
    | Top None -> eprintf "Top None@."
    | Top (Some str) -> 
        eprintf "@[<2>Top@ %a@]@."
          format_structure str
    | Annots _ -> eprintf "Annots [...]@."

  let dump_elems elems = List.iter dump_elem elems
*)
end
