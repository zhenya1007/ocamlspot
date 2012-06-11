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
    
module Make(Spotconfig : Spotconfig_intf.S) = struct
  include Spot.File

  type file = {
    path : string; (* "" means no source *)
    cwd : string;
    load_paths : string list;
    version : string * string;
    argv : string array;
    top : Abstraction.structure;
    flat : Abstraction.structure;
    rannots : Annot.t Regioned.t list;
    tree : Tree.t lazy_t;
    id_def_regions : (Ident.t, Region.t) Hashtbl.t;
  }

  let dump_file file =
    eprintf "@[<2>{ path= %S;@ cwd= %S;@ load_paths= [ @[%a@] ];@ version= %S,%S;@ argv= [| @[%a@] |]; ... }@]@."
      (match file.path with 
      | "" -> "NONE"
      | s -> s)
      file.cwd
      (Format.list "; " (fun ppf s -> fprintf ppf "%S" s)) file.load_paths
      (fst file.version) (snd file.version)
      (Format.list "; " (fun ppf s -> fprintf ppf "%S" s)) (Array.to_list file.argv)

  (* xxx.{ml,cmo,cmx,spot} => xxx.spot 
     xxx.{mli,cmi,spit} => xxx.spit
   *)        
  let spot_of_file file =
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
      | body, (".cmi" | ".mli" | ".spit") -> body ^ ".spit"
      | body, _ -> body ^ ".spot"
    in
    match dirname with
    | None -> filename
    | Some d -> Filename.concat d filename

  open Abstraction

  module Load : sig
    exception Old_spot of string (* spot *) * string (* source *)
    val load : load_paths:string list -> string -> file
    val load_module : ?spit:bool -> load_paths:string list -> string -> file
  end = struct

    let check_time_stamp ~spot source =
      let stat_spot = Unix.stat spot in
      let stat_source = Unix.stat source in
        (* Needs = : for packed modules, .spot and the source .cmo are written 
           almost at the same moment. *)
      stat_spot.Unix.st_mtime >= stat_source.Unix.st_mtime

    let find_alternative_source ~spot source =
        (* if [source] is not found, we try finding files with the same basename
           in
           - the directory of [spot]
           - the directory of [spot] points to (if [spot] is symlink)
         *)
        let source_base = Filename.basename source in
      let source_dirs =
          Filename.dirname spot ::
          begin 
            let stat_spot = Unix.lstat spot in
            if stat_spot.Unix.st_kind = Unix.S_LNK then
              [ Filename.dirname (Unix.readlink spot) ]
            else []
          end
        in
        List.find Sys.file_exists 
          (List.map (fun d -> 
            Filename.concat d source_base) source_dirs)

    let load_spot_file file =
      let ic = open_in_bin file in
      let magic_number_in_file = 
        let length = String.length magic_number in
        let buffer = String.create length in
        really_input ic buffer 0 length;
        buffer
      in
      if magic_number_in_file <> magic_number then 
        failwith (Printf.sprintf "Not a spot file: %s" file);
      let file_version : string * string = input_value ic in
      if (Spot.ocaml_version, Spot.version) <> file_version then begin
        failwith 
	  (Printf.sprintf "Incompatible spot file version %s for ocaml %s (must be %s for ocaml %s)" 
              (fst file_version) (snd file_version)
	      Spot.version Spot.ocaml_version);
      end;
      let v : t = input_value ic in
      close_in ic;
      file_version, v
    ;;

    let load_directly path : file =
      Debug.format "spot loading from %s@." path;
      let version, file = load_spot_file path in
      let rannots = 
        match 
          List.find_map_opt 
            (function Annots v -> Some v | _ -> None) file 
        with 
        | Some annots -> annots
        | None -> failwith "no annotations found"
      in
      let rannots = 
        List.map (fun (r, annot) ->
          { Regioned.region = Region.of_parsing r;
            value = annot }) rannots
      in
      let source_path =
        match 
          List.find_map_opt 
            (function Source_path v -> Some v | _ -> None) 
            file 
        with
        | Some source_path -> source_path
        | None -> failwith "no source path found"
      in

      (* fix source_path *)
      let source_path =
        match source_path with
        | None -> ""
        | Some source_path ->
            if Sys.file_exists source_path then source_path
            else
              try 
                let path = find_alternative_source ~spot:path source_path in
                Debug.format "Found an alternative source: %s@." path;
                path
              with
              | Not_found -> source_path
      in

      let cwd = 
        match List.find_map_opt (function Cwd v -> Some v | _ -> None) file with
        | Some cwd -> cwd
        | None -> failwith "no cwd found"
      in
      let load_paths =
        match List.find_map_opt (function Load_paths v -> Some v | _ -> None) file with
        | Some load_paths -> load_paths
        | None -> failwith "no load paths found"
      in
      let top = 
        match List.find_map_opt (function Top v -> Some v | _ -> None) file with
        | Some (Some top) -> top
        | Some None -> [] (* Error before writing any top element *)
        | None -> failwith "no top structure found"
      in
      let argv =
        match List.find_map_opt (function Argv v -> Some v | _ -> None) file with
        | Some argv -> argv
        | None -> failwith "no argv found"
      in
      let tree =
        lazy begin
          List.fold_left Tree.add Tree.empty rannots
        end
      in
      let id_def_regions = 
        let tbl = Hashtbl.create 107 in
        List.iter (fun { Regioned.region = loc; value = annot } ->
          match annot with
          | Annot.Str ( Abstraction.Str_value id
                      | Abstraction.Str_type id
                      | Abstraction.Str_exception id
                      | Abstraction.Str_modtype (id, _)
                      | Abstraction.Str_class id
                      | Abstraction.Str_cltype id   
                      | Abstraction.Str_module (id, _) )  ->
              Hashtbl.add tbl id loc
          | Annot.Str ( Abstraction.Str_include _ ) -> ()
          | Annot.Functor_parameter id ->
              Hashtbl.add tbl id loc
          | Annot.Type _ | Annot.Use _ | Annot.Module _ 
          | Annot.Non_expansive _ | Annot.Mod_type _ -> ()) rannots;
        tbl
      in
      let flat = 
        (* flat is created NOT from top but from rannots, since top
           may not exist when the compilation fails *)
        List.fold_left (fun st { Regioned.value = annot; _ } -> 
          match annot with
          | Annot.Str sitem -> sitem :: st
          | Annot.Functor_parameter id ->
              (* CR: fake a sitem. quite ad-hoc *)
              Str_module (id, Mod_ident (Path.Pdot (Path.Pident id, 
                                                    "parameter", 
                                                    -2))) 
              :: st
          | Annot.Type _ 
          | Annot.Use _
          | Annot.Module _ 
          | Annot.Non_expansive _ 
          | Annot.Mod_type _ -> st ) [] rannots
      in
      { version = version;
        path = source_path;
        cwd = cwd;
        load_paths = List.map (fun load_path -> cwd ^/ load_path) load_paths;
        argv = argv;
        top = top;
        flat = flat;
        rannots = rannots;
        tree = tree;
        id_def_regions = id_def_regions;
      }

    exception Old_spot of string (* spot *) * string (* source *)

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
                begin match file.path with 
                | "" -> ()
                | source -> 
                    if not (check_time_stamp ~spot:path source) then 
                      if Spotconfig.strict_time_stamp then 
                        raise (Old_spot (path, source))
                      else
                        eprintf "Warning: source %s is newer than the spot@." source
                end;
                Hashtbl.replace cache path file;
                file
              with
              | Not_found ->
                  failwith (Printf.sprintf "failed to find spot file %s" path)

    let find_in_path load_paths body ext =
        let body_ext = body ^ ext in
      let find_in_path load_paths name = 
        try Misc.find_in_path load_paths name with Not_found ->
          Misc.find_in_path_uncap load_paths name
      in
      try find_in_path load_paths body_ext with Not_found ->
      (* We do not give up yet.
         .spot file is not found, 
         but we still find a .cmi which is sym-linked to the original directory with .spot
      *)
      let cminame = body ^ ".cmi" in
        try
        let cmipath = find_in_path load_paths cminame in
        let stat = Unix.lstat cmipath in
        if stat.Unix.st_kind = Unix.S_LNK then begin
          let cmipath = Filename.dirname cmipath ^/ Unix.readlink cmipath in
          let spotpath = Filename.chop_extension cmipath ^ ext in
          if Sys.file_exists spotpath then begin
            Debug.format "Found an alternative %s: %s@." ext spotpath;
              spotpath 
            end else failwith (Printf.sprintf "spot file not found: %s, neither in %s" body_ext spotpath)
          end else raise Not_found
        with
        | (Failure _ as e) -> raise e
        | _ -> failwith (Printf.sprintf "spot file not found: %s" body_ext)
      

    let load ~load_paths spotname : file =
      Debug.format "@[<2>spot searching %s in@ paths [@[%a@]]@]@." 
          spotname
          (Format.list "; " (fun ppf x -> fprintf ppf "%S" x)) 
          load_paths;
        let body, ext = Filename.split_extension spotname in
      let path = find_in_path load_paths body ext in
      load_directly_with_cache path

    let load ~load_paths spotname : file =
      let alternate_spotname = 
        if Filename.is_relative spotname then None
        else
          Option.bind (Dotfile.find_and_load (Filename.dirname spotname)) 
            (fun (found_dir, dotfile) ->
              Option.map dotfile.Dotfile.build_dir ~f:(fun build_dir ->
                let length_found_dir = String.length found_dir in
                let found_dir' = 
                  String.sub spotname 0 length_found_dir
                in
                let rel_spotname =
                  String.sub spotname 
                    (length_found_dir + 1)
                    (String.length spotname - length_found_dir - 1)
                in
                assert (found_dir = found_dir');
                let dir = 
                  if Filename.is_relative build_dir then 
                    Filename.concat found_dir build_dir
                  else build_dir
                in
                Filename.concat dir rel_spotname))
      in
      try load ~load_paths spotname with
      | e -> 
          match alternate_spotname with
          | Some spotname -> load ~load_paths spotname
          | None -> raise e

    (* CR jfuruse: searching algorithm must be reconsidered *)        
    let load_module ?(spit=false) ~load_paths name =
      let spotname = name ^ if spit then ".spit" else ".spot" in
      try
        load ~load_paths spotname
      with
      | Failure s ->
          let spitname = name ^ if spit then ".spot" else ".spit" in
          Format.printf "%s load failed. Try to load %s@."
            spotname spitname;
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
      cwd = file.cwd;
      load_paths = file.load_paths;
      binding = Binding.empty }

  let invalid_env file =
    { Env.path = file.path;
      cwd = file.cwd;
      load_paths = file.load_paths;
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
      | _ ->
          (* CR jfuruse: loading twice... *)
          Debug.format "Finding %a@." PIdent.format pid;
          let file = 
            Load.load ~load_paths:[] (spot_of_file pid.PIdent.path) 
          in
          match pid.PIdent.ident with
          | None -> File_itself (* the whole file *)
          | Some id -> 
              Found_at begin try
                Hashtbl.find file.id_def_regions id
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
      | Value.Error (Load.Old_spot _ as exn) -> raise exn
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
    let f = Load.load ~load_paths:[""] (spot_of_file (env.Env.cwd ^/ file)) in
    Value.Structure ({ PIdent.path = f.path; ident = None },
                    Eval.structure (empty_env f) f.top,
                    None (* packed has no .mli *))

  let _ = Eval.packed := eval_packed

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
end