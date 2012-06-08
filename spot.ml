(* Annotations 

   Annotations are stored in .spot with their locations
*)

open Utils
open Ext
open Format

let magic_number = "OCamlSpot"
let ocaml_version = "3.12.1"
let version = "1.4.0"

module Location_bound = struct
  open Location
  let upperbound loc by = { loc with loc_end = by.loc_start }
end

module Kind = struct
  type t = 
    | Value | Type | Exception 
    | Module | Module_type 
    | Class | Class_type

  let to_string = function
    | Value       -> "v"
    | Type        -> "t"
    | Exception   -> "e" 
    | Module      -> "m"
    | Module_type -> "mt"
    | Class       -> "c"
    | Class_type  -> "ct"

  (* for messages *)
  let name = function
    | Value       -> "value"
    | Type        -> "type"
    | Exception   -> "exception" 
    | Module      -> "module"
    | Module_type -> "module_type"
    | Class       -> "class"
    | Class_type  -> "class_type"

  (* used for query interface *)        
  let from_string = function
    | "v"  | "value"       -> Value
    | "t"  | "type"        -> Type
    | "e"  | "exception"   -> Exception
    | "m"  | "module"      -> Module
    | "mt" | "module_type" -> Module_type
    | "c"  | "class"       -> Class
    | "ct" | "class_type"  -> Class_type
    | _ -> raise Not_found
end

(* CR jfuruse: ultimately we do not need this *)
module Abstraction = struct
  (* module definition abstraction *)

  (* CR jfuruse: types may be incompatible between compiler versions *)
  type module_expr = 
    | Mod_ident      of Path.t (* module M = N *)
    | Mod_packed     of string (* full path *)
        (* -pack overrides load paths: ocamlc -pack dir1/dir2/dir3/x.cmo *)
    | Mod_structure  of structure (* module M = struct ... end *)
    | Mod_functor    of Ident.t * Types.module_type * module_expr (* module M(I:S) = *)
    | Mod_apply      of module_expr * module_expr (* module M = N(O) *)
    | Mod_constraint of module_expr * Types.module_type
    | Mod_unpack     of module_expr
    | Mod_abstract (* used for Tmodtype_abstract *)

  (* structure abstraction : name - defloc asoc list *)
  and structure = structure_item list

  (* modtype must be identified from module, since they can have the
     same name *) 

  and structure_item = 
    | Str_value     of Ident.t
    | Str_type      of Ident.t
    | Str_exception of Ident.t
    | Str_module    of Ident.t * module_expr
    | Str_modtype   of Ident.t * module_expr
    | Str_class     of Ident.t
    | Str_cltype    of Ident.t
    | Str_include   of module_expr * (Kind.t * Ident.t) list

  let rec format_module_expr ppf = function
    | Mod_ident p -> fprintf ppf "%s" (Path.name p)
    | Mod_packed s -> fprintf ppf "packed(%s)" s
    | Mod_structure str -> format_structure ppf str
    | Mod_functor (id, mty, mexp) ->
        fprintf ppf "@[<4>\\(%s : %a) ->@ %a@]" 
	  (Ident.name id)
          (Printtyp.modtype ~with_pos:true) mty
          format_module_expr mexp
    | Mod_apply (mexp1, mexp2) ->
        fprintf ppf "%a(%a)"
          format_module_expr mexp1
          format_module_expr mexp2 
    | Mod_constraint (mexp, mty) ->
        fprintf ppf "@[%a@ :@ @[%a@]@]"
          format_module_expr mexp
          (Printtyp.modtype ~with_pos:true) mty
    | Mod_abstract -> fprintf ppf "<abst>"
    | Mod_unpack mty -> 
        fprintf ppf "@[unpack@ : @[%a@]@]"
          format_module_expr mty

  and format_structure ppf items = 
    fprintf ppf "{ @[<v>%a@] }"
      (list "; " format_structure_item) items
      
  and format_structure_item ppf = function
    | Str_value id -> fprintf ppf "val %s" (Ident.name id)
    | Str_type id -> fprintf ppf "type %s" (Ident.name id) (* CR jfuruse: todo *)
    | Str_exception id -> fprintf ppf "exception %s" (Ident.name id)
    | Str_module (id, mexp) -> 
        fprintf ppf "@[<v4>module %s = %a@]" 
          (Ident.name id) 
          format_module_expr mexp
    | Str_modtype (id, mexp) ->
        fprintf ppf "@[<v4>module type %s =@ %a@]" 
          (Ident.name id)
          format_module_expr mexp
    | Str_class id -> fprintf ppf "class %s" (Ident.name id)
    | Str_cltype id -> fprintf ppf "class type %s" (Ident.name id)
    | Str_include (mexp, kidents) ->
        fprintf ppf "@[include %a@ : [ @[%a@] ]@]"
          format_module_expr mexp
          (list "; " (fun ppf (k,id) -> 
            fprintf ppf "%s %s" (String.capitalize (Kind.name k)) (Ident.name id))) 
          kidents

  let ident_of_structure_item : structure_item -> (Kind.t * Ident.t) option = function
    | Str_value id        -> Some (Kind.Value, id)
    | Str_type id         -> Some (Kind.Type, id)
    | Str_exception id    -> Some (Kind.Exception, id) 
    | Str_module (id, _)  -> Some (Kind.Module, id)
    | Str_modtype (id, _) -> Some (Kind.Module_type, id)
    | Str_class id        -> Some (Kind.Class, id)
    | Str_cltype id       -> Some (Kind.Class_type, id)
    | Str_include _       -> None

  module Module_expr = struct
    (* cache key is Typedtree.module_expr *)
    module M = struct
      type t = Typedtree.module_expr
      let equal m1 m2 = m1 == m2
      let hash_source m = m.Typedtree.mod_loc
      let hash m = Hashtbl.hash (hash_source m)
    end
    include M
    module Table = Hashtbl.Make(M)
  end

  module Structure_item = struct
    (* cache key is Abstraction.structure_item, not Typedtree.structure_item *)
    module M = struct
      type t = structure_item
      let equal s1 s2 =
	match s1, s2 with
	| Str_value id1, Str_value id2 
	| Str_type id1, Str_type id2
	| Str_exception id1, Str_exception id2
	| Str_class id1, Str_class id2
	| Str_cltype id1, Str_cltype id2 -> id1 = id2
	| Str_module (id1, mexp1) , Str_module (id2, mexp2) ->
	    id1 = id2 && Module_expr.equal mexp1 mexp2
	| Str_modtype (id1, mty1), Str_modtype (id2, mty2) ->
            id1 = id2 && Module_expr.equal mty1 mty2
	| Str_include (mexp1, kids1), Str_include (mexp2, kids2) ->
	    Module_expr.equal mexp1 mexp2 && kids1 = kids2
	| (Str_value _ | Str_type _ | Str_exception _ | Str_modtype _ 
	  | Str_class _ | Str_cltype _ | Str_module _ | Str_include _),
	  (Str_value _ | Str_type _ | Str_exception _ | Str_modtype _ 
	  | Str_class _ | Str_cltype _ | Str_module _ | Str_include _) -> false

      let hash = Hashtbl.hash
    end
    include M
    module Table = Hashtbl.Make(M)
  end

  open Types
  open Typedtree

  let cache_module_expr = Module_expr.Table.create 31
  let cache_structure_item = Structure_item.Table.create 31

  let included_sig_identifier_table = Hashtbl.create 31

  module T = struct
    let kident_of_sigitem = function
      | Sig_value (id, _)     -> [Kind.Value, id]
      | Sig_exception (id, _) -> [Kind.Exception, id]
      | Sig_module (id, _, _) -> [Kind.Module, id]
      | Sig_type (id, _, _) -> [Kind.Type, id]
      | Sig_modtype (id, _)   -> [Kind.Module_type, id]
      | Sig_class (id, _, _) -> [Kind.Class, id]
      | Sig_class_type (id, _, _) -> [Kind.Class_type, id]

    let rec signature sg = Mod_structure (List.map signature_item sg)
      
    and signature_item = function
      | Sig_value (id, _) -> Str_value id
      | Sig_type (id, _, _) -> Str_type id
      | Sig_exception (id, _) -> Str_exception id
      | Sig_module (id, mty, _) -> Str_module (id, module_type mty)
      | Sig_modtype (id, mdtd) -> Str_modtype (id, modtype_declaration mdtd)
      | Sig_class (id, _, _) -> Str_class id
      | Sig_class_type (id, _, _) -> Str_cltype id

    and module_type = function
      | Mty_ident p -> Mod_ident p
      | Mty_signature sg -> signature sg
      | Mty_functor (id, mty1, mty2) -> Mod_functor(id, mty1, module_type mty2)

    and modtype_declaration = function
      | Modtype_abstract -> Mod_structure []
      | Modtype_manifest mty -> module_type mty
  end

  module TT = struct
    let kident_of_sigitem = function
      | Tsig_value (id, _, _)     -> [Kind.Value, id]
      | Tsig_exception (id, _, _) -> [Kind.Exception, id]
      | Tsig_module (id, _, _) -> [Kind.Module, id]
      | Tsig_type typs -> 
          List.map (fun (id, _, _) -> Kind.Type, id) typs
      | Tsig_modtype (id, _, _)   -> [Kind.Module_type, id]
      | Tsig_class clses -> 
          List.map (fun cls -> 
            Kind.Class, cls.ci_id_class) clses
      | Tsig_class_type clses ->
          List.map (fun cls -> 
            Kind.Class_type, cls.ci_id_class) clses
      | Tsig_recmodule _ -> assert false
      | Tsig_open _ -> assert false
      | Tsig_include _ -> assert false
  end

  let rec module_expr mexp =
    try
      match Module_expr.Table.find cache_module_expr mexp with
      | None ->
          (* When a module definition finds itself in itself.
             Impossible to happen, so far. *)
          assert false
      | Some v -> v
    with
    | Not_found ->
	Module_expr.Table.replace cache_module_expr mexp None;
	let res = module_expr_desc mexp.mod_desc in
	Module_expr.Table.replace cache_module_expr mexp (Some res);
        res

  and module_expr_desc = function
    | Tmod_ident (p, _) -> Mod_ident p
    | Tmod_structure str ->
	(* This may recompute abstractions of structure_items.
	   It sounds inefficient but not so much actually, since
	   module_expr is nicely cached. *)
	structure str
    | Tmod_functor (id, _, mty, mexp) ->
        let mty = Mtype.scrape mexp.mod_env mty.mty_type in
	Mod_functor(id, mty, module_expr mexp)
    | Tmod_apply (mexp1, mexp2, _mcoercion) -> (* CR jfuruse ? *)
	Mod_apply (module_expr mexp1, module_expr mexp2)
    | Tmod_constraint (mexp, mty_, _constraint, _mcoercion) ->
	Mod_constraint (module_expr mexp, mty_)
    | Tmod_unpack (_expr, mty_) -> 
        Mod_unpack (T.module_type mty_) (* CR jfuruse: need to unpack, really? *)
          
  and structure str = 
    Mod_structure (List.concat_map structure_item str.str_items)

  and structure_item sitem = 
    (* it may recompute the same thing, but it is cheap *)
    let sitems = structure_item_desc sitem.str_desc in
    (* eq consing *)
    let equalize sitem =
      try
	Structure_item.Table.find cache_structure_item sitem
      with
      | Not_found -> 
	  Structure_item.Table.replace cache_structure_item sitem sitem;
	  sitem
    in
    List.map equalize sitems
    
  and structure_item_desc = function
    | Tstr_eval _ -> []
    | Tstr_value (_, pat_exps) ->
	List.map (fun id -> Str_value id) (let_bound_idents pat_exps)
    | Tstr_primitive (id, _, _vdesc) -> 
	[Str_value id]
    | Tstr_type id_descs -> List.map (fun (id, _, _) -> Str_type id) id_descs
    | Tstr_exception (id ,_ , _) ->
	[Str_exception id]
    | Tstr_exn_rebind (id, _, _path, _) -> (* CR jfuruse: path? *)
	[Str_exception id]
    | Tstr_module (id, _, mexp) ->
	[Str_module (id, module_expr mexp)]
    | Tstr_recmodule (idmexps) ->
	List.map (fun (id, _, _, mexp) ->
	  Str_module (id, module_expr mexp)) idmexps
    | Tstr_modtype (id, _, mty) -> [Str_modtype (id, module_type mty)]
    | Tstr_open _ -> []
    | Tstr_class classdescs ->
	List.map (fun (cls, _names, _) -> Str_class cls.ci_id_class) classdescs
    | Tstr_class_type iddecls ->
	List.map (fun (id, _, _) -> Str_cltype id) iddecls
    | Tstr_include (mexp, _ids) ->
        let sg = match (mexp.mod_type : Types.module_type) with Mty_signature sg -> sg | _ -> assert false in (* CR jfuruse: I hope so... *)
	let kids = List.concat_map T.kident_of_sigitem sg in
        [Str_include (module_expr mexp, kids)]

  (* CR jfuruse: caching like module_expr_sub *)
  and module_type mty = module_type_desc mty.mty_desc

  and module_type_desc = function
    | Tmty_ident (p, _) -> Mod_ident p
    | Tmty_signature sg -> signature sg
    | Tmty_functor (id, _, mty1, mty2) ->
        (* CR jfuruse: need to scrape ? but how ? *)
        Mod_functor(id, mty1.mty_type, module_type mty2)
(*
    | Tmty_with of module_type * (Path.t * Longident.t loc * with_constraint) list
    | Tmty_typeof of module_expr
*)
    | _ -> assert false

  and signature sg = Mod_structure (List.concat_map signature_item sg.sig_items)

  and signature_item sitem = 
      let aux id f =
	  (* Sigitem might be defined by include, but it is not recorded
	     in signature. We here try to recover it. *)
	  (* CR jfuruse: included modules may listed more than once *)
	  let sitem, recorded = Hashtbl.find included_sig_identifier_table id in
          if !recorded then f ()
          else begin
            recorded := true;
            sitem
          end
      in
      match sitem.sig_desc with
      | Tsig_value (id, _, _) -> [aux id (fun () -> Str_value id)]
      | Tsig_exception (id, _, _) -> [aux id (fun () -> Str_exception id)]
      | Tsig_module (id, _ , mty) ->
          [aux id (fun () -> Str_module (id, module_type mty))]
      | Tsig_modtype (id, _, mty_decl) ->
          [aux id (fun () -> 
            (* todo *) Str_modtype (id, modtype_declaration mty_decl) (* sitem.sig_final_env can be used? *)) ]

      | Tsig_type typs -> List.map (fun (id, _, _) -> aux id (fun () -> Str_type id)) typs
      | Tsig_class clses -> List.map (fun cls -> aux cls.ci_id_class (fun () -> Str_class cls.ci_id_class)) clses
      | Tsig_class_type clses -> List.map (fun cls -> aux cls.ci_id_class (fun () -> Str_cltype cls.ci_id_class)) clses

      | Tsig_recmodule _ -> assert false
      | Tsig_open _ -> assert false
      | Tsig_include _ -> assert false
	
  and modtype_declaration = function
    | Tmodtype_abstract -> Mod_abstract
    | Tmodtype_manifest mty -> module_type mty

end

let protect name f v =
  try f v with e ->
    Format.eprintf "Error: %s: %s@." name (Printexc.to_string e)
    
module Annot = struct
  type t =
    | Type of Types.type_expr * Env.t * [`Expr | `Pattern | `Val]
    | Str of Abstraction.structure_item 
    | Use of Kind.t * Path.t
    | Module of Abstraction.module_expr
    | Functor_parameter of Ident.t
    | Non_expansive of bool
    | Mod_type of Types.module_type

  let equal t1 t2 = match t1, t2 with
    | Type (t1, _, _), Type (t2, _, _) -> t1 == t2
    | Mod_type mty1, Mod_type mty2 -> mty1 == mty2
    | Str sitem1, Str sitem2 -> Abstraction.Structure_item.equal sitem1 sitem2
    | Module mexp1, Module mexp2 -> mexp1 == mexp2
    | Use (k1,p1), Use (k2,p2) -> k1 = k2 && p1 = p2
    | Non_expansive b1, Non_expansive b2 -> b1 = b2
    | Functor_parameter id1, Functor_parameter id2 -> id1 = id2
    | (Type _ | Str _ | Module _ | Functor_parameter _ | Use _ | Non_expansive _ 
          | Mod_type _),
      (Type _ | Str _ | Module _ | Functor_parameter _ | Use _ | Non_expansive _
          | Mod_type _) -> false 

  (* CR jfuruse: A Location.t contains a filename, though it is always
     unique. Waste of 4xn bytes. *)
  let recorded = (Hashtbl.create 1023 : (Location.t, (int * t list)) Hashtbl.t)

  let clear () = Hashtbl.clear recorded

  type location_property = Wellformed | Flipped | Over_files | Illformed

  let check_location loc = 
    if loc.Location.loc_start == Lexing.dummy_pos || loc.Location.loc_end == Lexing.dummy_pos then Illformed
    else if loc.Location.loc_start = Lexing.dummy_pos || loc.Location.loc_end = Lexing.dummy_pos then Illformed
    else 
      (* If the file name is different between the start and the end, we cannot tell the wellformedness. *)
      if loc.Location.loc_start.Lexing.pos_fname <> loc.Location.loc_end.Lexing.pos_fname then Over_files
      else
        (* P4 creates some flipped locations where loc_start > loc_end *)
        match compare loc.Location.loc_start.Lexing.pos_cnum loc.Location.loc_end.Lexing.pos_cnum 
        with
        | -1 | 0 -> Wellformed
        | _ -> Flipped

  let record loc t = 
    if !Clflags.annotations then begin
      let really_record () = 
        let num_records, records = 
          try Hashtbl.find recorded loc with Not_found -> 0, []
        in
        (* CR jfuruse: I am not really sure the below is correct now, 
           but I remember the huge compilation slow down... *)
        (* This caching works horribly when too many things are defined 
           at the same location. For example, a type definition of more than 
           3000 variants, with sexp camlp4 extension, the compile time explodes
           from 10secs to 4mins! Therefore this works 
           only if [num_records <= 10] 
        *)
        if num_records <= 10 && List.exists (equal t) records then ()
        else Hashtbl.replace recorded loc (num_records + 1, t :: records)
      in
      match check_location loc with
      | Wellformed -> really_record ()
      | Flipped -> 
          if not loc.Location.loc_ghost then Format.eprintf "%aWarning: Flipped location.@." Location.print loc; 
          really_record ()
      | Illformed -> 
          if not loc.Location.loc_ghost then Format.eprintf "%aWarning: Ill-formed location.@." Location.print loc
      | Over_files -> ()
    end

  let record_constr_type_use loc ty =
    let path_of_constr_type t =
      let t = Ctype.repr t in 
      match (Ctype.repr t).Types.desc with
      | Types.Tconstr (p, _, _) -> Some p
      | _ ->
          Format.eprintf "Error: Spot.Annot.record_constr_type_use: not a constructor type: %a@." (Printtyp.type_expr ~with_pos:false) ty;
          None
    in
    match path_of_constr_type ty with
    | Some path -> record loc (Use (Kind.Type, path))
    | None -> ()

  let record_module_expr_def loc id modl =
    protect "Spot.Annot.record_module_expr_def" (fun () ->
      record loc (Str (Abstraction.Str_module 
	                  (id, 
	                  (Abstraction.module_expr modl)))))
      ()
    
  let record_module_expr_use loc modl =
    protect "Spot.Annot.record_module_expr_use" (fun () ->
      record loc (Module (Abstraction.module_expr modl));
      record loc (Mod_type modl.Typedtree.mod_type))
      ()

  let record_include loc modl (* _sg *) =
    protect "Spot.Annot.record_include" (fun () ->
      let abs = Abstraction.module_expr modl in
      match abs with
      | Abstraction.Mod_structure str ->
          List.iter (fun sitem -> record loc (Str sitem)) str
      | _ -> assert false)
      ()

  let record_include_sig loc mty sg =
    protect "Spot.Annot.record_include_sig" (fun () ->
      let kids = (* CR jfuruse: copy of structure_item_sub *) 
	List.concat_map Abstraction.T.kident_of_sigitem sg
      in
      let sitem = Abstraction.Str_include (Abstraction.module_type mty, kids)
      in 
      (* ocaml signature simply forgets the fact that kids are
	 included. We memorize them here. *)
      List.iter (fun (_,id) ->
	Hashtbl.add
          Abstraction.included_sig_identifier_table
	  id (sitem, ref false (* never recorded in the parent sig yet *))) kids;
      record loc (Str sitem))
      ()

  let record_module_expr_def loc id modl =
    protect "Spot.Annot.record_module_expr_def" (fun () ->
      record loc (Str (Abstraction.Str_module 
	                  (id, 
	                  (Abstraction.module_expr modl))));
      record loc (Mod_type modl.Typedtree.mod_type))
      ()
    
  let record_module_type_def loc id mty =
    protect "Spot.Annot.record_module_type_def" (fun () ->
      record loc (Str (Abstraction.Str_modtype
                          (id,
                          Abstraction.module_type mty))))
      ()
      
  let recorded () = Hashtbl.fold (fun k (_,vs) st -> 
    List.map (fun v -> k,v) vs @ st) recorded []

  let string_of_at = function
    | `Expr -> "Expr"
    | `Pattern -> "Pattern"
    | `Val -> "Val"

  let format ppf = function
    | Type (typ, _env, at) -> 
	Printtyp.reset ();
	Printtyp.mark_loops typ;
        (* CR jfuruse: not fancy having @. *)
	fprintf ppf "Type: %a@ " (Printtyp.type_scheme ~with_pos:false) typ;
	fprintf ppf "XType: %a@ " (Printtyp.type_scheme ~with_pos:true) typ;
        fprintf ppf "At: %s" (string_of_at at)
    | Mod_type mty -> 
	fprintf ppf "Type: %a@ " (Printtyp.modtype ~with_pos:false) mty;
	fprintf ppf "XType: %a" (Printtyp.modtype ~with_pos:true) mty
    | Str str ->
	fprintf ppf "Str: %a"
	  Abstraction.format_structure_item str
    | Use (use, path) ->
	fprintf ppf "Use: %s, %s" 
	  (String.capitalize (Kind.name use)) (Path.name path)
    | Module mexp ->
	fprintf ppf "Module: %a"
          Abstraction.format_module_expr mexp
    | Functor_parameter id ->
	fprintf ppf "Functor_parameter: %s" (Ident.name id)
    | Non_expansive b ->
        fprintf ppf "Non_expansive: %b" b

  let summary ppf = function
    | Type (_typ, _env, at) -> 
        (* CR jfuruse: not fancy having @. *)
	fprintf ppf "Type: ...@ ";
	fprintf ppf "XType: ...@ ";
        fprintf ppf "At: %s" (string_of_at at)
    | Mod_type _mty -> 
	fprintf ppf "Type: ...@ ";
	fprintf ppf "XType: ..."
    | Str _str ->
	fprintf ppf "Str: ..."
    | Use (use, path) ->
	fprintf ppf "Use: %s, %s" 
	  (String.capitalize (Kind.name use)) (Path.name path)
    | Module _mexp ->
	fprintf ppf "Module: ..."
    | Functor_parameter id ->
	fprintf ppf "Functor_parameter: %s" (Ident.name id)
    | Non_expansive b ->
        fprintf ppf "Non_expansive: %b" b

  let dummy = Use (Kind.Value, Path.Pident (Ident.create_persistent "dummy"))
end

module Top = struct
  let recorded = ref None
  let clear () = recorded := None

  let record_structure str = 
    if !Clflags.annotations then begin
      assert (!recorded = None); 
      recorded := Some (Abstraction.structure str)
    end

  let record_structure = protect "Spot.Top.record_structure" record_structure 
    
  let record_signature sg = 
    if !Clflags.annotations then begin
      assert (!recorded = None); 
      recorded := Some (Abstraction.signature sg)
    end

  let record_signature = protect "Spot.Top.record_signature" record_signature
    
  let recorded () = !recorded
end

(*
(* Spot file *)
module File = struct
  (* not record but list for future exetensibility *)
  type elem =
    | Argv of string array
    | Source_path of string option (* packed module has None *)
    | Cwd of string
    | Load_paths of string list
    | Top of Abstraction.module_expr option
    | Annots of (Location.t * Annot.t) list

  (* marshalled type *)
  type t = elem list

  let argv_override = ref None 

  let write_to_oc ~source implementation annots oc =
    protect "Spot.File.write_to_oc" (fun () ->
      let source = 
        match source with
        | None -> None
        | Some p -> Some (Filename.concat (Sys.getcwd ()) p)
      in
      output_string oc magic_number;
      output_value oc (ocaml_version, version);
      Marshal.to_channel oc 
        [ Argv (match !argv_override with Some argv -> argv | None -> Sys.argv);
	  Source_path source;
          Cwd (Sys.getcwd ());
	  Load_paths !Config.load_path;
          Top implementation;
	  Annots annots ]
        [] (* keep sharing *))
      ()

  let write ~source implementation annots spot_file =
    protect "Spot.File.write" (fun () ->
      let oc = open_out_bin spot_file in
      write_to_oc ~source implementation annots oc;
      close_out oc) ()

  (* we must clear all the recorded after any dump of a compilation unit, 
     since the compiler may handle more than one .ml *)
  let clear () =
    Top.clear ();
    Annot.clear ();
    Abstraction.Module_expr.Table.clear Abstraction.cache_module_expr;
    Abstraction.Structure_item.Table.clear Abstraction.cache_structure_item
      
  let dump ~source spot_file =
    if !Clflags.annotations then 
      write ~source (Top.recorded ()) (Annot.recorded ()) spot_file;
    clear ()
  ;;

  (* -pack can pack modules out of include path: 
     ocamlc -pack -o p.cmo dir/m.cmo
  *)
  let dump_package ~prefix ~source files =
    if !Clflags.annotations then begin
      write ~source:(Some source)
        (Some (List.map (fun f -> 
          let module_name = 
            String.capitalize (Filename.chop_extension (Filename.basename f))
          in
          Abstraction.Str_module (Ident.create module_name, (* CR jfuruse: stamp is bogus *)
                                 Abstraction.Mod_packed f)) files))
        [] 
        (prefix ^ ".spot")
    end;
    clear ()

  let set_argv argv = argv_override := Some argv
end
*)

module Position = struct
  open Lexing

  type t = { line_column : (int * int) option; 
             bytes : int option }

  let of_lexing_position pos =
    { line_column = Some (pos.pos_lnum, pos.pos_cnum - pos.pos_bol);
      bytes = Some pos.pos_cnum }

  let compare p1 p2 = match p1, p2 with
    | { bytes = Some b1; _ }, { bytes = Some b2; _ } -> compare b1 b2
    | { line_column = Some (l1,c1); _ }, { line_column = Some (l2,c2); _ } ->
	begin match compare l1 l2 with
	| 0 -> compare c1 c2
	| n -> n
	end
    | _ -> assert false

  let to_string p = match p.line_column, p.bytes with
    | Some (l,c), Some b -> Printf.sprintf "l%dc%db%d" l c b
    | Some (l,c), None -> Printf.sprintf "l%dc%d" l c
    | None, Some b -> Printf.sprintf "b%d" b
    | None, None -> assert false

  let none = { line_column = None; bytes = None }

  exception Parse_failure of string

  let parse s =
    (* token : [a-z][0-9]+ *)
    let len = String.length s in 
    let rec get_number ~num pos =
      if pos >= len then num, pos
      else 
	match s.[pos] with
	| '0'..'9' -> 
	    get_number   
	      ~num: (num * 10 + int_of_char s.[pos] - int_of_char '0')
	      (pos + 1)
	| _ -> num, pos
    in
    let rec get_tokens pos =
      if pos >= len then []
      else
	match s.[pos] with
	| 'a'..'z' -> 
	    let k = s.[pos] in
	    let pos = pos + 1 in
	    let num, pos' = get_number ~num:0 pos in
	    if pos = pos' then 
	      raise (Parse_failure (Printf.sprintf "pos token has no number: '%c'" 
				       k));
	    (k, num) :: get_tokens pos'
        | '0'..'9' ->
            (* Good Ol' Syntax *)
            begin try ['b', int_of_string s] with _ ->
              raise (Parse_failure
                        (Printf.sprintf "failed to parse %S as a byte position" s))
            end
	| _ -> 
	    raise (Parse_failure (Printf.sprintf "illegal pos token head '%c'" 
				     s.[pos]))
    in
    let tokens = get_tokens 0 in
    match tokens with
    | ['l', line; 'c', column] -> { line_column = Some (line, column); 
				    bytes = None }
    | ['b', bytes] -> { line_column = None; bytes = Some bytes }
    | _ -> raise (Parse_failure "illegal pos token combination")

  let next = function
    | { bytes = Some b; _ } -> { bytes = Some (b + 1); line_column = None }
    | { line_column = Some (l,c); bytes = None; } ->
        { line_column = Some (l, c+1); bytes = None }
    | _ -> assert false

  let is_complete = function
    | { line_column = Some _; bytes = Some _ } -> true
    | _ -> false
      
  (* it drops one byte at the end, but who cares? *)        
  let complete mlpath t = match t with
    | { line_column = Some _; bytes = Some _ } -> 
        t (* already complete *)
    | { line_column = Some (line, column); bytes = None } ->
        let ic = open_in_bin mlpath in
        let rec iter cur_line pos =
          ignore (input_line ic);
          let cur_line = cur_line + 1 in
          if cur_line = line then begin
            close_in ic;
            { line_column = Some (line, column); bytes = Some (pos + column) }
          end else iter cur_line (pos_in ic)
        in
        iter 0 0

    | { line_column = None; bytes = Some bytes } -> 
        let ic = open_in_bin mlpath in
        let rec iter lines remain =
          let pos = pos_in ic in
          let new_remain = bytes - pos in
          if new_remain < 0 then begin (* run over *)
            close_in ic;
            { line_column = Some (lines, remain); bytes = Some bytes }
          end else begin
            ignore (input_line ic);
            iter (lines+1) new_remain
          end
        in
        iter 0 bytes
          
    | { line_column = None; bytes = None } -> assert false

end

module Region = struct
  type t = { 
    start : Position.t;
    end_ : Position.t
  }

  let to_string t =
    Printf.sprintf "%s:%s"
      (Position.to_string t.start)
      (Position.to_string t.end_)

  let of_parsing l =
    let start = Position.of_lexing_position l.Location.loc_start in
    let end_ = Position.of_lexing_position l.Location.loc_end in
    match Position.compare start end_ with
    | -1 | 0 -> { start = start; end_ = end_ }
    | _ -> { start = end_; end_ = start }

  let compare l1 l2 = 
    if Position.compare l1.start l2.start = 0 
       && Position.compare l2.end_ l1.end_ = 0 then `Same
    else if Position.compare l1.start l2.start <= 0 
         && Position.compare l2.end_ l1.end_ <= 0 then `Includes
    else if Position.compare l2.start l1.start <= 0 
         && Position.compare l1.end_ l2.end_ <= 0 then `Included
    else if Position.compare l1.end_ l2.start <= 0 then `Left
    else if Position.compare l2.end_ l1.start <= 0 then `Right
    else `Overwrap

(*
  let position_prev pos = { pos with pos_cnum = pos.pos_cnum - 1 }
  let position_next pos = { pos with pos_cnum = pos.pos_cnum + 1 }
*)

  let split l1 ~by:l2 =
    if compare l1 l2 = `Overwrap then
      if Position.compare l1.start l2.start < 0 then
	Some ({ l1 with end_ = (* position_prev *) l2.start },
	      { l1 with start = l2.start })
      else if Position.compare l2.start l1.start < 0 then
        Some ({ l1 with end_ = l2.end_ },
	      { l1 with start = (* position_next *) l2.end_ })
      else assert false
    else None

  open Position

  let point_by_byte pos =
    { start = { line_column = None;
 		bytes = Some pos };
      end_ = { line_column = None;
               bytes = Some (pos + 1)} }

  let point pos = { start = pos; end_ = Position.next pos }

  let none = { start = Position.none;
	       end_ = Position.none }

  let length_in_bytes t =
    let bytes = function
      | { Position.bytes = Some bytes; _ } -> bytes
      | _ -> raise Not_found
    in
    bytes t.end_ - bytes t.start

  let is_complete t = 
    Position.is_complete t.start && Position.is_complete t.end_

  let complete mlpath t =
    { start = Position.complete mlpath t.start;
      end_ = Position.complete mlpath t.end_ }

  let substring mlpath t =
    let t = complete mlpath t in
    let ic = open_in_bin mlpath in
    match t.start.Position.bytes, t.end_.Position.bytes with
    | Some start, Some end_ ->
	seek_in ic start;
	let s = String.create (end_ - start) in
	really_input ic s 0 (end_ - start);
	t, s
    | _ -> assert false
    
end

module Regioned = struct
  type 'a t = { region: Region.t; value: 'a }  

  let compare { region = r1; _ } { region = r2; _ } = Region.compare r1 r2

  let split { region = r1; value = v } ~by:{ region = r2; _ } = 
    Option.map (Region.split r1 ~by: r2) ~f:(fun (r11, r12) -> 
      { region = r11; value = v },
      { region = r12; value = v }) 

  let format f ppf { region = r; value = v } =
    fprintf ppf "@[<2>%s: %a@]" 
      (Region.to_string r) 
      f v
end

(* annotation with region *)
module RAnnot = struct
  type t      = Annot.t Regioned.t
  let split   = Regioned.split
  let compare = Regioned.compare
  let format  = Regioned.format Annot.format
end

module Tree = struct
  include Treeset.Make(RAnnot)

  open Regioned

  (* If the region maybe splitted, the original region will be gone *)
  let add t rannot = add_elem rannot t

  let iter = iter_elem

  let find_path_contains r t = 
    let probe = { region = r; value = Annot.dummy } in
    find_path_contains probe t

  let dump t = 
    iter_elem (fun ~parent rrspot ->
	let format_parent ppf = function
	  | None -> fprintf ppf "ROOT"
	  | Some rrspot -> RAnnot.format ppf rrspot
	in
	eprintf "@[<2>%a =>@ %a@]@."
	  format_parent parent
	  RAnnot.format rrspot) t
end

