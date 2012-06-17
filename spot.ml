(* Annotations 

   Annotations are stored in .spot with their locations
*)

open Utils
open Ext
open Format

let magic_number = "OCamlSpot"
let ocaml_version = "4.00.0"
let version = "2.0.0"

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
    | AMod_ident      of Path.t (* module M = N *)
    | AMod_packed     of string (* full path *)
        (* -pack overrides load paths: ocamlc -pack dir1/dir2/dir3/x.cmo *)
    | AMod_structure  of structure (* module M = struct ... end *)
    | AMod_functor    of Ident.t * Types.module_type * module_expr (* module M(I:S) = *)
    | AMod_apply      of module_expr * module_expr (* module M = N(O) *)
    | AMod_constraint of module_expr * Types.module_type
    | AMod_unpack     of module_expr
    | AMod_abstract (* used for Tmodtype_abstract *)

  (* structure abstraction : name - defloc asoc list *)
  and structure = structure_item list

  (* modtype must be identified from module, since they can have the
     same name *) 

  and structure_item = 
    | AStr_value     of Ident.t
    | AStr_type      of Ident.t
    | AStr_exception of Ident.t
    | AStr_module    of Ident.t * module_expr
    | AStr_modtype   of Ident.t * module_expr
    | AStr_class     of Ident.t
    | AStr_cltype    of Ident.t
    | AStr_include   of module_expr * (Ident.t * (Kind.t * Ident.t)) list
    | AStr_included  of Ident.t * module_expr * Kind.t * Ident.t

  let rec format_module_expr ppf = function
    | AMod_ident p -> fprintf ppf "%s" (Path.name p)
    | AMod_packed s -> fprintf ppf "packed(%s)" s
    | AMod_structure str -> format_structure ppf str
    | AMod_functor (id, mty, mexp) ->
        fprintf ppf "@[<4>\\(%s : %a) ->@ %a@]" 
	  (Ident.name id)
          (Printtyp.modtype ~with_pos:true) mty
          format_module_expr mexp
    | AMod_apply (mexp1, mexp2) ->
        fprintf ppf "%a(%a)"
          format_module_expr mexp1
          format_module_expr mexp2 
    | AMod_constraint (mexp, mty) ->
        fprintf ppf "@[%a@ :@ @[%a@]@]"
          format_module_expr mexp
          (Printtyp.modtype ~with_pos:true) mty
    | AMod_abstract -> fprintf ppf "<abst>"
    | AMod_unpack mty -> 
        fprintf ppf "@[unpack@ : @[%a@]@]"
          format_module_expr mty

  and format_structure ppf items = 
    fprintf ppf "{ @[<v>%a@] }"
      (list ";@," format_structure_item) items
      
  and format_structure_item ppf = function
    | AStr_value id -> fprintf ppf "val %s" (Ident.name id)
    | AStr_type id -> fprintf ppf "type %s" (Ident.name id) (* CR jfuruse: todo *)
    | AStr_exception id -> fprintf ppf "exception %s" (Ident.name id)
    | AStr_module (id, mexp) -> 
        fprintf ppf "@[<v4>module %s =@ %a@]" 
          (Ident.name id) 
          format_module_expr mexp
    | AStr_modtype (id, mexp) ->
        fprintf ppf "@[<v4>module type %s =@ %a@]" 
          (Ident.name id)
          format_module_expr mexp
    | AStr_class id -> fprintf ppf "class %s" (Ident.name id)
    | AStr_cltype id -> fprintf ppf "class type %s" (Ident.name id)
    | AStr_include (mexp, aliases) ->
        fprintf ppf "@[<v4>include %a@ { @[<v>%a@] }@]"
          format_module_expr mexp
          (list ";@ " (fun ppf (id', (k,id)) -> 
            fprintf ppf "%s %a = %a" 
              (Kind.name k)
              Ident.format id' 
              Ident.format id))
          aliases
    | AStr_included (id, mexp, kind, id') ->
        fprintf ppf "@[<v4>included %s %a = %a@ { @[<v>%a@] }@]"
          (Kind.name kind)
          Ident.format id
          Ident.format id'
          format_module_expr mexp

  let ident_of_structure_item : structure_item -> (Kind.t * Ident.t) option = function
    | AStr_value id        -> Some (Kind.Value, id)
    | AStr_type id         -> Some (Kind.Type, id)
    | AStr_exception id    -> Some (Kind.Exception, id) 
    | AStr_module (id, _)  -> Some (Kind.Module, id)
    | AStr_modtype (id, _) -> Some (Kind.Module_type, id)
    | AStr_class id        -> Some (Kind.Class, id)
    | AStr_cltype id       -> Some (Kind.Class_type, id)
    | AStr_include _       -> None
    | AStr_included (id, _, kind, _) -> Some (kind, id)

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
	| AStr_value id1, AStr_value id2 
	| AStr_type id1, AStr_type id2
	| AStr_exception id1, AStr_exception id2
	| AStr_class id1, AStr_class id2
	| AStr_cltype id1, AStr_cltype id2 -> id1 = id2
	| AStr_module (id1, mexp1) , AStr_module (id2, mexp2) ->
	    id1 = id2 && Module_expr.equal mexp1 mexp2
	| AStr_modtype (id1, mty1), AStr_modtype (id2, mty2) ->
            id1 = id2 && Module_expr.equal mty1 mty2
	| AStr_include (mexp1, aliases1), AStr_include (mexp2, aliases2) ->
            aliases1 = aliases2
            && Module_expr.equal mexp1 mexp2
	| AStr_included (id1, mexp1, kind1, id1'), AStr_included (id2, mexp2, kind2, id2') ->
            id1 = id2 && kind1 = kind2 && id1' = id2'
            && Module_expr.equal mexp1 mexp2
	| (AStr_value _ | AStr_type _ | AStr_exception _ | AStr_modtype _ 
	  | AStr_class _ | AStr_cltype _ | AStr_module _ | AStr_include _ | AStr_included _),
	  (AStr_value _ | AStr_type _ | AStr_exception _ | AStr_modtype _ 
	  | AStr_class _ | AStr_cltype _ | AStr_module _ | AStr_include _ | AStr_included _) -> false

      let hash = Hashtbl.hash
    end
    include M
    module Table = Hashtbl.Make(M)
  end

  open Types
  open Typedtree
  open Asttypes

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

    let rec signature sg = AMod_structure (List.map signature_item sg)
      
    and signature_item = function
      | Sig_value (id, _) -> AStr_value id
      | Sig_type (id, _, _) -> AStr_type id
      | Sig_exception (id, _) -> AStr_exception id
      | Sig_module (id, mty, _) -> AStr_module (id, module_type mty)
      | Sig_modtype (id, mdtd) -> AStr_modtype (id, modtype_declaration mdtd)
      | Sig_class (id, _, _) -> AStr_class id
      | Sig_class_type (id, _, _) -> AStr_cltype id

    and module_type = function
      | Mty_ident p -> AMod_ident p
      | Mty_signature sg -> signature sg
      | Mty_functor (id, mty1, mty2) -> AMod_functor(id, mty1, module_type mty2)

    and modtype_declaration = function
      | Modtype_abstract -> AMod_structure []
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

  let aliases_of_include mexp ids =
    let sg = match mexp.mod_type with Mty_signature sg -> sg | _ -> assert false in
    let kids = List.concat_map T.kident_of_sigitem sg in
    (* [ids] only contain things with values, i.e. values, modules and classes *)
    List.map (fun (k,id) -> match k with
    | Kind.Value | Kind.Module | Kind.Class -> 
        begin match List.find_all (fun id' -> Ident0.name id' = Ident0.name id) ids with
        | [id'] -> id', (k, id)
        | _ -> assert false
        end
    | _ -> Ident.unsafe_create_with_stamp (Ident0.name id) (-1), (k, id)) kids

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
    | Tmod_ident (p, _) -> AMod_ident p
    | Tmod_structure str ->
	(* This may recompute abstractions of structure_items.
	   It sounds inefficient but not so much actually, since
	   module_expr is nicely cached. *)
	structure str
    | Tmod_functor (id, _, mty, mexp) ->
        let mty = 
          try Mtype.scrape mexp.mod_env mty.mty_type with _ -> assert false
        in
	AMod_functor(id, mty, module_expr mexp)
    | Tmod_apply (mexp1, mexp2, _mcoercion) -> (* CR jfuruse ? *)
	AMod_apply (module_expr mexp1, module_expr mexp2)
    | Tmod_constraint (mexp, mty_, _constraint, _mcoercion) ->
	AMod_constraint (module_expr mexp, mty_)
    | Tmod_unpack (_expr, mty_) -> 
        AMod_unpack (T.module_type mty_) (* CR jfuruse: need to unpack, really? *)
          
  and structure str = AMod_structure (List.concat_map structure_item str.str_items)

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
	List.map (fun id -> AStr_value id) (let_bound_idents pat_exps)
    | Tstr_primitive (id, _, _vdesc) -> 
	[AStr_value id]
    | Tstr_type id_descs -> List.concat_map (fun (id, _, td) -> AStr_type id :: type_declaration td) id_descs
    | Tstr_exception (id ,_ , _) ->
	[AStr_exception id]
    | Tstr_exn_rebind (id, _, _path, _) -> (* CR jfuruse: path? *)
	[AStr_exception id]
    | Tstr_module (id, _, mexp) ->
	[AStr_module (id, module_expr mexp)]
    | Tstr_recmodule (idmexps) ->
	List.map (fun (id, _, _, mexp) ->
	  AStr_module (id, module_expr mexp)) idmexps
    | Tstr_modtype (id, _, mty) -> [AStr_modtype (id, module_type mty)]
    | Tstr_open _ -> []
    | Tstr_class classdescs ->
	List.map (fun (cls, _names, _) -> AStr_class cls.ci_id_class) classdescs
    | Tstr_class_type iddecls ->
	List.map (fun (id, _, _) -> AStr_cltype id) iddecls
    | Tstr_include (mexp, ids) ->
        let aliases = try aliases_of_include mexp ids with _ -> assert false in
        [AStr_include (module_expr mexp, aliases)]

  (* CR jfuruse: caching like module_expr_sub *)
  and module_type mty = module_type_desc mty.mty_desc

  and module_type_desc = function
    | Tmty_ident (p, _) -> AMod_ident p
    | Tmty_signature sg -> signature sg
    | Tmty_functor (id, _, mty1, mty2) ->
        (* CR jfuruse: need to scrape ? but how ? *)
        AMod_functor(id, mty1.mty_type, module_type mty2)
(*
    | Tmty_with of module_type * (Path.t * Longident.t loc * with_constraint) list
    | Tmty_typeof of module_expr
*)
    | _ -> assert false

  and signature sg = AMod_structure (List.concat_map signature_item sg.sig_items)

  and signature_item sitem = 
    let aux id f = f ()
(*
        (* Sigitem might be defined by include, but it is not recorded
           in signature. We here try to recover it. *)
        (* CR jfuruse: included modules may listed more than once *)
        let sitem, recorded = Hashtbl.find included_sig_identifier_table id in
        if !recorded then f ()
        else begin
          recorded := true;
          sitem
        end
*)
    in
    match sitem.sig_desc with
    | Tsig_value (id, _, _) -> [aux id (fun () -> AStr_value id)]
    | Tsig_exception (id, _, _) -> [aux id (fun () -> AStr_exception id)]
    | Tsig_module (id, _ , mty) ->
        [aux id (fun () -> AStr_module (id, module_type mty))]
    | Tsig_modtype (id, _, mty_decl) ->
        [aux id (fun () -> 
          (* todo *) AStr_modtype (id, modtype_declaration mty_decl) (* sitem.sig_final_env can be used? *)) ]

    | Tsig_type typs -> List.concat_map (fun (id, _, td) -> aux id (fun () -> AStr_type id :: type_declaration td)) typs
    | Tsig_class clses -> List.map (fun cls -> aux cls.ci_id_class (fun () -> AStr_class cls.ci_id_class)) clses
    | Tsig_class_type clses -> List.map (fun cls -> aux cls.ci_id_class (fun () -> AStr_cltype cls.ci_id_class)) clses

    | Tsig_recmodule _ -> assert false
    | Tsig_open _ -> assert false
    | Tsig_include _ -> assert false
	
  and modtype_declaration = function
    | Tmodtype_abstract -> AMod_abstract
    | Tmodtype_manifest mty -> module_type mty

(* This is wrong. This only flatten module related things and 
   non modules like patterns are never flattened. 


  let rec flatten str = List.concat_map flatten_item str

  and flatten_item item = match item with
    | AStr_value     _
    | AStr_type      _
    | AStr_exception _
    | AStr_class     _
    | AStr_cltype    _ -> [item]
    | AStr_module  (_, mexp)
    | AStr_modtype (_, mexp) -> item :: flatten_module_expr mexp
    | AStr_include (mexp, aliases) ->
        let flats = flatten_module_expr mexp in
(* mexp can be just M, so we dont try expanding it
        List.map (fun (id, ((k,id') as kid)) ->
          try
            Some (List.find (fun sitem -> ident_of_structure_item sitem = Some kid) flats)
          with
          | Not_found ->
              Format.eprintf "@[<2>%s %a not found in@ @[%a@]@]@." 
                (Kind.name k) Ident.format id'
                format_structure flats;
              None
        )
          aliases
*)
        item :: flats

  and flatten_module_expr = function
    | AMod_ident _ -> []
    | AMod_packed _ -> []
    | AMod_structure str -> flatten str
    | AMod_functor (_, _, mexp) -> flatten_module_expr mexp
    | AMod_apply (_, m) -> flatten_module_expr m
    | AMod_constraint (m, _) -> flatten_module_expr m
    | AMod_unpack m -> flatten_module_expr m
    | AMod_abstract -> []
*)


  and type_declaration td = match td.typ_kind with
    | Ttype_abstract -> []
    | Ttype_variant lst -> List.map (fun (id, {loc=_loc}, _, _) -> AStr_type id) lst
    | Ttype_record lst -> List.map (fun (id, {loc=_loc}, _, _, _) -> AStr_type id) lst
end

let protect name f v = try f v with e ->
  Format.eprintf "Error: %s: %s@." name (Printexc.to_string e)
    
let protect' name f v = try f v with e ->
  Format.eprintf "Error: %s: %s@." name (Printexc.to_string e); raise e
    
module Annot = struct
  type t =
    | Use               of Kind.t * Path.t
    | Type              of Types.type_expr * Env.t * [`Expr | `Pattern | `Val]
    | Mod_type          of Types.module_type
    | Str               of Abstraction.structure_item  (* CRjfuruse: Should be Sitem *)
    | Module            of Abstraction.module_expr
    | Functor_parameter of Ident.t
    | Non_expansive     of bool

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
      record loc (Str (Abstraction.AStr_module 
	                  (id, 
	                  (Abstraction.module_expr modl)))))
      ()
    
  let record_module_expr_use loc modl =
    protect "Spot.Annot.record_module_expr_use" (fun () ->
      record loc (Module (Abstraction.module_expr modl));
      record loc (Mod_type modl.Typedtree.mod_type))
      ()

(*
  let record_include_sig loc mty sg =
    protect "Spot.Annot.record_include_sig" (fun () ->
      let kids = (* CR jfuruse: copy of structure_item_sub *) 
	List.concat_map Abstraction.T.kident_of_sigitem sg
      in
      let sitem = Abstraction.AStr_include (Abstraction.module_type mty, kids)
      in 
      (* ocaml signature simply forgets the fact that kids are
	 included. We memorize them here. *)
      List.iter (fun (_,id) ->
	Hashtbl.add
          Abstraction.included_sig_identifier_table
	  id (sitem, ref false (* never recorded in the parent sig yet *))) kids;
      record loc (Str sitem))
      ()
*)

  module Record = struct
    open Asttypes
    open Typedtree
    open Abstraction

    let record_record loc typ = 
      let open Types in
      let open Ctype in
      match (repr typ).desc with
      | Tconstr (path, _, _) -> record loc (Use (Kind.Type, path)) 
      | _ -> (* strange.. *) ()

    class fold = object (self)
      inherit Ttfold.fold as super

      method! pattern p = 
        record p.pat_loc (Type (p.pat_type, p.pat_env, `Pattern));
        begin match p.pat_desc with
        | Tpat_record _ -> record_record p.pat_loc p.pat_type
        | _ -> ()
        end;
        super#pattern p

    (* CR jfuruse: pat_extra *)
          
      method! pattern_desc pd = 
        begin match pd with 
        | Tpat_var (id, {loc})
        | Tpat_alias (_, id, {loc}) 
          -> record loc (Str (AStr_value id))
        | Tpat_construct (path, {loc}, cdesc, _, _) -> 
            let kind = match cdesc.Types.cstr_tag with
              | Types.Cstr_exception _ -> Kind.Exception            
              | _ -> Kind.Type
            in
            record loc (Use (kind, path))
        | Tpat_record (lst , _) ->
            List.iter (fun (path, {loc}, _, _) -> 
              record loc (Use (Kind.Type, path))) lst
        | Tpat_any | Tpat_constant _ | Tpat_tuple _
        | Tpat_variant _ | Tpat_array _ | Tpat_or _ | Tpat_lazy _ -> ()
        end;
        super#pattern_desc pd
      
      method! expression e = 
        record e.exp_loc (Type (e.exp_type, e.exp_env, `Expr));
        begin match e.exp_desc with
        | Texp_record _ -> record_record e.exp_loc e.exp_type
        | _ -> ()
        end;
        super#expression e

(*
and exp_extra =
  | Texp_constraint of core_type option * core_type option
  | Texp_open of Path.t * Longident.t loc * Env.t
*)

      method !expression_desc ed =
        begin match ed with
        | Texp_ident (path, {loc}, _) -> 
            record loc (Use (Kind.Value, path))
        | Texp_construct (path, {loc}, cdesc, _, _) ->
            let kind = match cdesc.Types.cstr_tag with
              | Types.Cstr_exception _ -> Kind.Exception            
              | _ -> Kind.Type
            in
            record loc (Use (kind, path))
        | Texp_record (lst, _) ->
            List.iter (fun (path, {loc}, _, _) ->
              record loc (Use (Kind.Type, path))) lst
        | Texp_field (_, path, {loc}, _) 
        | Texp_setfield (_, path, {loc}, _, _) -> 
            record loc (Use (Kind.Type, path))
        | Texp_for (id, {loc}, _, _, _, _) -> 
            (* CR jfuruse: add type int to id *)
            record loc (Str (AStr_value id))
        | Texp_new (path, {loc}, _) -> 
            record loc (Use (Kind.Class, path))
        | Texp_instvar (_path, path, {loc}) (* CR jfuruse: not sure! *)
        | Texp_setinstvar (_path, path, {loc}, _) ->
            record loc (Use (Kind.Value, path))
        | Texp_override (_path, lst) ->  (* CR jfuruse: what todo with _path? *)
            List.iter (fun (path, {loc}, _) ->
              record loc (Use (Kind.Type, path))) lst
        | Texp_letmodule (id, {loc}, mexp, _) -> 
            record loc (Str (AStr_module (id, module_expr mexp)))
        | Texp_newtype (_string, _expr) (* CR jfuruse: ? *) -> ()
        | Texp_constant _ | Texp_let _ | Texp_function _
        | Texp_apply _ | Texp_match _ | Texp_try _
        | Texp_tuple _ | Texp_variant _ | Texp_array _
        | Texp_ifthenelse _ | Texp_sequence _ | Texp_while _
        | Texp_when _ | Texp_send _ | Texp_assert _ | Texp_assertfalse
        | Texp_lazy _ | Texp_poly _ | Texp_object _ | Texp_pack _ -> ()
        end;
        super#expression_desc ed
(*          
and meth =
    Tmeth_name of string
  | Tmeth_val of Ident.t
*)

(* CR jfuruse: Class_type
      method! class_expr ce =
        record ce.cl_loc (Class_type (ce.cl_type, ce.cl_env));
        super#class_expr ce
*)

      method! class_expr_desc ced =
        begin match ced with
        | Tcl_ident (path, {loc}, _) -> record loc (Use (Kind.Value, path)) 
        | Tcl_structure _ -> ()
        | Tcl_fun (_, _, _lst (* CR jfuruse: ? *), _, _) -> ()
        | Tcl_apply _ -> ()
        | Tcl_let (_, _, _lst (* CR jfuruse: ? *), _) -> ()
        | Tcl_constraint _ -> ()
        end;
        super#class_expr_desc ced
(*

and class_structure =
  { cstr_pat : pattern;
    cstr_fields: class_field list;
    cstr_type : Types.class_signature;
    cstr_meths: Ident.t Meths.t }

and class_field =
   {
    cf_desc : class_field_desc;
    cf_loc : Location.t;
  }

and class_field_kind =
  Tcfk_virtual of core_type
| Tcfk_concrete of expression
*)

      method! class_field_desc cfd = 
        begin match cfd with
        | Tcf_inher (_, _, _, _, _) -> ()
        | Tcf_val (_name, {loc}, _, id, _, _) -> record loc (Str (AStr_value id))
        | Tcf_meth (_name, {loc=_loc}, _, _, _) -> ()
        | Tcf_constr _ -> ()
        | Tcf_init _ -> ()
        end;
        super#class_field_desc cfd

      method! module_expr me = (* CR jfuruse: me.mod_env *)
        record me.mod_loc (Mod_type me.mod_type);
        super#module_expr me

(*
and module_type_constraint =
  Tmodtype_implicit
| Tmodtype_explicit of module_type
*)

      method! module_expr_desc med = 
        begin match med with
        | Tmod_ident (path, {loc}) -> 
            record loc (Use (Kind.Module, path))
        | Tmod_functor (id, {loc}, _, _) ->
            (* CR jfuruse: must rethink *)
            (* record loc (Str (AStr_module (id, ???))) *)
            record loc (Functor_parameter id);
        | Tmod_structure _
        | Tmod_apply _
        | Tmod_constraint _
        | Tmod_unpack _ -> ()
        end;
        super#module_expr_desc med

(* CR jfuruse: I want to put the sig
and structure = {
  str_items : structure_item list;
  str_type : Types.signature;
  str_final_env : Env.t;
}
*)

      method! structure_item sitem = 
        begin match sitem.str_desc with (* CR jfuruse; todo add env *)
        | Tstr_include (mexp, idents) ->
            let loc = sitem.str_loc in
            let id_kid_list = aliases_of_include mexp idents in
            let m = module_expr mexp in
            List.iter (fun (id, (k, id')) -> 
              record loc (Str (AStr_included (id, m, k, id')))) id_kid_list
        | _ -> ()
        end;
        super#structure_item sitem

      method! structure_item_desc sid =
        begin match sid with
        | Tstr_primitive (id, {loc}, _) -> 
            record loc (Str (AStr_value id))
        | Tstr_type lst ->
            List.iter (fun (id, {loc}, _) ->
              record loc (Str (AStr_type id))) lst
        | Tstr_exception (id, {loc}, _) -> 
            record loc (Str (AStr_exception id))
        | Tstr_exn_rebind (id, {loc}, path, {loc=loc'}) ->
            record loc (Str (AStr_exception id));
            record loc' (Use (Kind.Exception, path))
        | Tstr_module (id, {loc}, mexp) -> 
            record loc (Str (AStr_module (id, module_expr mexp)))
        | Tstr_recmodule lst ->
            List.iter (fun (id, {loc}, _mty, mexp) ->
              record loc (Str (AStr_module (id, module_expr mexp)))) lst
        | Tstr_modtype (id, {loc}, mty) -> 
            record loc (Str (AStr_modtype (id, module_type mty)))
        | Tstr_open (path, {loc}) -> 
            record loc (Use (Kind.Module, path))
        | Tstr_class_type lst ->
            List.iter (fun (id, {loc}, _) -> 
              record loc (Str (AStr_cltype id))) lst
        | Tstr_include (_mexp, _idents) -> () (* done in #structure_item *)
        | Tstr_eval _ 
        | Tstr_value _ 
        | Tstr_class _  
          -> ()
        end;
        super#structure_item_desc sid

(*
and module_coercion =
    Tcoerce_none
  | Tcoerce_structure of (int * module_coercion) list
  | Tcoerce_functor of module_coercion * module_coercion
  | Tcoerce_primitive of Primitive.description
*)

(* add env?
and module_type =
  { mty_desc: module_type_desc;
    mty_type : Types.module_type;
    mty_env : Env.t; (* BINANNOT ADDED *)
    mty_loc: Location.t }
*)

      method! module_type_desc mtd =
        begin match mtd with
        | Tmty_ident (path, {loc}) -> 
            record loc (Use (Kind.Module_type, path))
        | Tmty_functor (id, {loc}, mty, _mty) -> 
            record loc (Str (AStr_module (id, module_type mty)))
        | Tmty_with (_mty, lst) -> 
            List.iter (fun (path, {loc}, with_constraint) -> 
              record loc (Use ( (match with_constraint with
                                 | Twith_type _ -> Kind.Type
                                 | Twith_module _ -> Kind.Module
                                 | Twith_typesubst _ -> Kind.Type
                                 | Twith_modsubst _ -> Kind.Module),
                                path ))) lst
        | Tmty_typeof _
        | Tmty_signature _ -> ()
        end;
        super#module_type_desc mtd

(* add env
and signature = {
  sig_items : signature_item list;
  sig_type : Types.signature;
  sig_final_env : Env.t;
}

add env
and signature_item =
  { sig_desc: signature_item_desc;
    sig_env : Env.t; (* BINANNOT ADDED *)
    sig_loc: Location.t }
*)

      method! signature_item_desc sid =
        begin match sid with
        | Tsig_value (id, {loc}, _) -> record loc (Str (AStr_value id))
        | Tsig_type lst -> 
            List.iter (fun (id, {loc}, _) -> 
              record loc (Str (AStr_type id))) lst
        | Tsig_exception (id, {loc}, _) -> record loc (Str (AStr_exception id))
        | Tsig_module (id, {loc}, mty) -> record loc (Str (AStr_module (id, module_type mty)))
        | Tsig_recmodule lst -> 
            List.iter (fun (id, {loc}, mty) -> 
              record loc (Str (AStr_module (id, module_type mty)))) lst
        | Tsig_modtype (id, {loc}, mtd) -> 
            record loc (Str (AStr_modtype (id, modtype_declaration mtd)))
        | Tsig_open (path, {loc}) -> record loc (Use (Kind.Module, path))
        | Tsig_include _ -> ()
        | Tsig_class _ -> ()
        | Tsig_class_type _ -> ()
        end;
        super#signature_item_desc sid
        

      method! with_constraint wc = 
        begin match wc with 
        | Twith_module (path, {loc}) -> record loc (Use (Kind.Module, path)) 
        | Twith_modsubst (path, {loc}) -> record loc (Use (Kind.Module, path))  (*?*)
        | Twith_type _ -> ()
        | Twith_typesubst _ -> ()
        end;
        super#with_constraint wc

(* add env?
and core_type =
(* mutable because of [Typeclass.declare_method] *)
  { mutable ctyp_desc : core_type_desc;
    mutable ctyp_type : type_expr;
    ctyp_env : Env.t; (* BINANNOT ADDED *)
    ctyp_loc : Location.t }
*)

      method! core_type_desc ctd =
        begin match ctd with
        | Ttyp_var _var -> () (* CR jfuruse: todo *)
        | Ttyp_constr (path, {loc}, _) -> record loc (Use (Kind.Type, path))
        | Ttyp_class (path, {loc}, _, _) -> record loc (Use (Kind.Class, path))
            (* CR jfuruse: or class type? *)
        | Ttyp_alias (_core_type, _var) -> () (* CR jfuruse: todo *)
        | Ttyp_poly (_vars, _core_type) -> () (* CR jfuruse; todo *)
        | Ttyp_any 
        | Ttyp_arrow _
        | Ttyp_tuple _
        | Ttyp_object _
        | Ttyp_variant _
        | Ttyp_package _
            -> ()
        end;
        super#core_type_desc ctd

(*
and package_type = {
  pack_name : Path.t;
  pack_fields : (Longident.t loc * core_type) list;
  pack_type : Types.module_type;
  pack_txt : Longident.t loc;
}

and core_field_type =
  { field_desc: core_field_desc;
    field_loc: Location.t }

and core_field_desc =
    Tcfield of string * core_type
  | Tcfield_var

and row_field =
    Ttag of label * bool * core_type list
  | Tinherit of core_type

and value_description =
  { val_desc : core_type;
    val_val : Types.value_description;
    val_prim : string list;
    val_loc : Location.t;
    }

and type_declaration =
  { typ_params: string loc option list;
    typ_type : Types.type_declaration;
    typ_cstrs: (core_type * core_type * Location.t) list;
    typ_kind: type_kind;
    typ_private: private_flag;
    typ_manifest: core_type option;
    typ_variance: (bool * bool) list;
    typ_loc: Location.t }
*)

      method! type_kind tk = 
        begin match tk with 
        | Ttype_abstract -> ()
        | Ttype_variant lst -> 
            List.iter (fun (id, {loc}, _, _loc(*?*)) ->
              record loc (Str (AStr_type id))) lst
        | Ttype_record lst ->
            List.iter (fun (id, {loc}, _, _, _loc(*?*)) ->
              record loc (Str (AStr_type id))) lst
        end;
        super#type_kind tk

(*

and exception_declaration =
  { exn_params : core_type list;
    exn_exn : Types.exception_declaration;
    exn_loc : Location.t }

and class_type =
  { cltyp_desc: class_type_desc;
    cltyp_type : Types.class_type;
    cltyp_env : Env.t; (* BINANNOT ADDED *)
    cltyp_loc: Location.t }
*)

      method! class_type_desc ctd = 
        begin match ctd with
        | Tcty_constr (path, {loc}, _) -> record loc (Use (Kind.Class_type, path))
        | Tcty_signature _
        | Tcty_fun _ -> ()
        end;
        super#class_type_desc ctd

(*

and class_signature = {
    csig_self : core_type;
    csig_fields : class_type_field list;
    csig_type : Types.class_signature;
    csig_loc : Location.t;
  }

and class_type_field = {
    ctf_desc : class_type_field_desc;
    ctf_loc : Location.t;
  }

and class_type_field_desc =
    Tctf_inher of class_type
  | Tctf_val of (string * mutable_flag * virtual_flag * core_type)
  | Tctf_virt  of (string * private_flag * core_type)
  | Tctf_meth  of (string * private_flag * core_type)
  | Tctf_cstr  of (core_type * core_type)

and class_declaration =
  class_expr class_infos

and class_description =
  class_type class_infos

and class_type_declaration =
  class_type class_infos
*)

      method! class_infos f ci =
        let loc = ci.ci_id_name.loc in
        (* CR jfuruse: are they correct? *)
        record loc (Str (AStr_class ci.ci_id_class));
        record loc (Str (AStr_cltype ci.ci_id_class_type));
        record loc (Str (AStr_type ci.ci_id_object));
        record loc (Str (AStr_type ci.ci_id_typesharp));
        super#class_infos f ci

    end
  end

  let get_recorded () = Hashtbl.fold (fun k (_,vs) st -> 
    List.map (fun v -> k,v) vs @ st) recorded []

  let record_structure str = 
    protect' "Spot.Annot.record_structure" (fun () ->
      Hashtbl.clear recorded;
      let o = new Record.fold in
      ignore (o#structure str);
      get_recorded ())
      ()

  let record_signature sg = 
    protect' "Spot.Annot.record_signature" (fun () ->
      Hashtbl.clear recorded;
      let o = new Record.fold in
      ignore (o#signature sg);
      get_recorded ())
      ()

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

