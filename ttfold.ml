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

open Types
open Typedtree
  
class virtual ovisit_pattern =
  object (self)
    method virtual ref : 'a1. ('a1 -> unit) -> 'a1 ref -> unit
    method virtual option : 'a1. ('a1 -> unit) -> 'a1 option -> unit
    method virtual list : 'a1. ('a1 -> unit) -> 'a1 list -> unit
    method pattern : pattern -> unit =
      fun __value ->
        (self#pattern_desc __value.pat_desc;
         self#list (fun (__x1, __x2) -> (self#pat_extra __x1; ()))
           __value.pat_extra;
         ())
    method pat_extra : pat_extra -> unit =
      fun __value ->
        match __value with
        | Tpat_constraint __x1 -> (self#core_type __x1; ())
        | Tpat_type (__x1, __x2) -> ()
        | Tpat_unpack -> ()
    method pattern_desc : pattern_desc -> unit =
      fun __value ->
        match __value with
        | Tpat_any -> ()
        | Tpat_var (__x1, __x2) -> ()
        | Tpat_alias (__x1, __x2, __x3) -> (self#pattern __x1; ())
        | Tpat_constant __x1 -> ()
        | Tpat_tuple __x1 -> (self#list self#pattern __x1; ())
        | Tpat_construct (__x1, __x2, __x3, __x4) ->
            (self#list self#pattern __x3; ())
        | Tpat_variant (__x1, __x2, __x3) ->
            (self#option self#pattern __x2; self#ref (fun _ -> ()) __x3; ())
        | Tpat_record (__x1, __x2) ->
            (self#list (fun (__x1, __x2, __x3) -> (self#pattern __x3; ()))
               __x1;
             ())
        | Tpat_array __x1 -> (self#list self#pattern __x1; ())
        | Tpat_or (__x1, __x2, __x3) ->
            (self#pattern __x1;
             self#pattern __x2;
             self#option (fun _ -> ()) __x3;
             ())
        | Tpat_lazy __x1 -> (self#pattern __x1; ())
    method expression : expression -> unit =
      fun __value ->
        (self#expression_desc __value.exp_desc;
         self#list (fun (__x1, __x2) -> (self#exp_extra __x1; ()))
           __value.exp_extra;
         ())
    method exp_extra : exp_extra -> unit =
      fun __value ->
        match __value with
        | Texp_constraint (__x1, __x2) ->
            (self#option self#core_type __x1;
             self#option self#core_type __x2;
             ())
        | Texp_open (__x1, __x2, __x3, __x4) -> ()
        | Texp_poly __x1 -> (self#option self#core_type __x1; ())
        | Texp_newtype __x1 -> ()
    method expression_desc : expression_desc -> unit =
      fun __value ->
        match __value with
        | Texp_ident (__x1, __x2, __x3) -> ()
        | Texp_constant __x1 -> ()
        | Texp_let (__x1, __x2, __x3) ->
            (self#list
               (fun (__x1, __x2) ->
                  (self#pattern __x1; self#expression __x2; ()))
               __x2;
             self#expression __x3;
             ())
        | Texp_function (__x1, __x2, __x3) ->
            (self#list
               (fun (__x1, __x2) ->
                  (self#pattern __x1; self#expression __x2; ()))
               __x2;
             ())
        | Texp_apply (__x1, __x2) ->
            (self#expression __x1;
             self#list
               (fun (__x1, __x2, __x3) ->
                  (self#option self#expression __x2; ()))
               __x2;
             ())
        | Texp_match (__x1, __x2, __x3) ->
            (self#expression __x1;
             self#list
               (fun (__x1, __x2) ->
                  (self#pattern __x1; self#expression __x2; ()))
               __x2;
             ())
        | Texp_try (__x1, __x2) ->
            (self#expression __x1;
             self#list
               (fun (__x1, __x2) ->
                  (self#pattern __x1; self#expression __x2; ()))
               __x2;
             ())
        | Texp_tuple __x1 -> (self#list self#expression __x1; ())
        | Texp_construct (__x1, __x2, __x3, __x4) ->
            (self#list self#expression __x3; ())
        | Texp_variant (__x1, __x2) -> (self#option self#expression __x2; ())
        | Texp_record (__x1, __x2) ->
            (self#list (fun (__x1, __x2, __x3) -> (self#expression __x3; ()))
               __x1;
             self#option self#expression __x2;
             ())
        | Texp_field (__x1, __x2, __x3) -> (self#expression __x1; ())
        | Texp_setfield (__x1, __x2, __x3, __x4) ->
            (self#expression __x1; self#expression __x4; ())
        | Texp_array __x1 -> (self#list self#expression __x1; ())
        | Texp_ifthenelse (__x1, __x2, __x3) ->
            (self#expression __x1;
             self#expression __x2;
             self#option self#expression __x3;
             ())
        | Texp_sequence (__x1, __x2) ->
            (self#expression __x1; self#expression __x2; ())
        | Texp_while (__x1, __x2) ->
            (self#expression __x1; self#expression __x2; ())
        | Texp_for (__x1, __x2, __x3, __x4, __x5, __x6) ->
            (self#expression __x3;
             self#expression __x4;
             self#expression __x6;
             ())
        | Texp_when (__x1, __x2) ->
            (self#expression __x1; self#expression __x2; ())
        | Texp_send (__x1, __x2, __x3) ->
            (self#expression __x1;
             self#meth __x2;
             self#option self#expression __x3;
             ())
        | Texp_new (__x1, __x2, __x3) -> ()
        | Texp_instvar (__x1, __x2, __x3) -> ()
        | Texp_setinstvar (__x1, __x2, __x3, __x4) ->
            (self#expression __x4; ())
        | Texp_override (__x1, __x2) ->
            (self#list (fun (__x1, __x2, __x3) -> (self#expression __x3; ()))
               __x2;
             ())
        | Texp_letmodule (__x1, __x2, __x3, __x4) ->
            (self#module_expr __x3; self#expression __x4; ())
        | Texp_assert __x1 -> (self#expression __x1; ())
        | Texp_assertfalse -> ()
        | Texp_lazy __x1 -> (self#expression __x1; ())
        | Texp_object (__x1, __x2) ->
            (self#class_structure __x1; self#list (fun _ -> ()) __x2; ())
        | Texp_pack __x1 -> (self#module_expr __x1; ())
    method meth : meth -> unit = fun __value -> ()
    method class_expr : class_expr -> unit =
      fun __value -> (self#class_expr_desc __value.cl_desc; ())
    method class_expr_desc : class_expr_desc -> unit =
      fun __value ->
        match __value with
        | Tcl_ident (__x1, __x2, __x3) -> (self#list self#core_type __x3; ())
        | Tcl_structure __x1 -> (self#class_structure __x1; ())
        | Tcl_fun (__x1, __x2, __x3, __x4, __x5) ->
            (self#pattern __x2;
             self#list (fun (__x1, __x2, __x3) -> (self#expression __x3; ()))
               __x3;
             self#class_expr __x4;
             ())
        | Tcl_apply (__x1, __x2) ->
            (self#class_expr __x1;
             self#list
               (fun (__x1, __x2, __x3) ->
                  (self#option self#expression __x2; ()))
               __x2;
             ())
        | Tcl_let (__x1, __x2, __x3, __x4) ->
            (self#list
               (fun (__x1, __x2) ->
                  (self#pattern __x1; self#expression __x2; ()))
               __x2;
             self#list (fun (__x1, __x2, __x3) -> (self#expression __x3; ()))
               __x3;
             self#class_expr __x4;
             ())
        | Tcl_constraint (__x1, __x2, __x3, __x4, __x5) ->
            (self#class_expr __x1;
             self#option self#class_type __x2;
             self#list (fun _ -> ()) __x3;
             self#list (fun _ -> ()) __x4;
             ())
    method class_structure : class_structure -> unit =
      fun __value ->
        (self#pattern __value.cstr_pat;
         self#list self#class_field __value.cstr_fields;
         ())
    method class_field : class_field -> unit =
      fun __value -> (self#class_field_desc __value.cf_desc; ())
    method class_field_kind : class_field_kind -> unit =
      fun __value ->
        match __value with
        | Tcfk_virtual __x1 -> (self#core_type __x1; ())
        | Tcfk_concrete __x1 -> (self#expression __x1; ())
    method class_field_desc : class_field_desc -> unit =
      fun __value ->
        match __value with
        | Tcf_inher (__x1, __x2, __x3, __x4, __x5) ->
            (self#class_expr __x2;
             self#option (fun _ -> ()) __x3;
             self#list (fun (__x1, __x2) -> ()) __x4;
             self#list (fun (__x1, __x2) -> ()) __x5;
             ())
        | Tcf_val (__x1, __x2, __x3, __x4, __x5, __x6) ->
            (self#class_field_kind __x5; ())
        | Tcf_meth (__x1, __x2, __x3, __x4, __x5) ->
            (self#class_field_kind __x4; ())
        | Tcf_constr (__x1, __x2) ->
            (self#core_type __x1; self#core_type __x2; ())
        | Tcf_init __x1 -> (self#expression __x1; ())
    method module_expr : module_expr -> unit =
      fun __value -> (self#module_expr_desc __value.mod_desc; ())
    method module_type_constraint : module_type_constraint -> unit =
      fun __value ->
        match __value with
        | Tmodtype_implicit -> ()
        | Tmodtype_explicit __x1 -> (self#module_type __x1; ())
    method module_expr_desc : module_expr_desc -> unit =
      fun __value ->
        match __value with
        | Tmod_ident (__x1, __x2) -> ()
        | Tmod_structure __x1 -> (self#structure __x1; ())
        | Tmod_functor (__x1, __x2, __x3, __x4) ->
            (self#module_type __x3; self#module_expr __x4; ())
        | Tmod_apply (__x1, __x2, __x3) ->
            (self#module_expr __x1;
             self#module_expr __x2;
             self#module_coercion __x3;
             ())
        | Tmod_constraint (__x1, __x2, __x3, __x4) ->
            (self#module_expr __x1;
             self#module_type_constraint __x3;
             self#module_coercion __x4;
             ())
        | Tmod_unpack (__x1, __x2) -> (self#expression __x1; ())
    method structure : structure -> unit =
      fun __value -> (self#list self#structure_item __value.str_items; ())
    method structure_item : structure_item -> unit =
      fun __value -> (self#structure_item_desc __value.str_desc; ())
    method structure_item_desc : structure_item_desc -> unit =
      fun __value ->
        match __value with
        | Tstr_eval __x1 -> (self#expression __x1; ())
        | Tstr_value (__x1, __x2) ->
            (self#list
               (fun (__x1, __x2) ->
                  (self#pattern __x1; self#expression __x2; ()))
               __x2;
             ())
        | Tstr_primitive (__x1, __x2, __x3) ->
            (self#value_description __x3; ())
        | Tstr_type __x1 ->
            (self#list
               (fun (__x1, __x2, __x3) -> (self#type_declaration __x3; ()))
               __x1;
             ())
        | Tstr_exception (__x1, __x2, __x3) ->
            (self#exception_declaration __x3; ())
        | Tstr_exn_rebind (__x1, __x2, __x3, __x4) -> ()
        | Tstr_module (__x1, __x2, __x3) -> (self#module_expr __x3; ())
        | Tstr_recmodule __x1 ->
            (self#list
               (fun (__x1, __x2, __x3, __x4) ->
                  (self#module_type __x3; self#module_expr __x4; ()))
               __x1;
             ())
        | Tstr_modtype (__x1, __x2, __x3) -> (self#module_type __x3; ())
        | Tstr_open (__x1, __x2, __x3) -> ()
        | Tstr_class __x1 ->
            (self#list
               (fun (__x1, __x2, __x3) ->
                  (self#class_declaration __x1;
                   self#list (fun _ -> ()) __x2;
                   ()))
               __x1;
             ())
        | Tstr_class_type __x1 ->
            (self#list
               (fun (__x1, __x2, __x3) ->
                  (self#class_type_declaration __x3; ()))
               __x1;
             ())
        | Tstr_include (__x1, __x2) -> (self#module_expr __x1; ())
    method module_coercion : module_coercion -> unit =
      fun __value ->
        match __value with
        | Tcoerce_none -> ()
        | Tcoerce_structure __x1 ->
            (self#list (fun (__x1, __x2) -> (self#module_coercion __x2; ()))
               __x1;
             ())
        | Tcoerce_functor (__x1, __x2) ->
            (self#module_coercion __x1; self#module_coercion __x2; ())
        | Tcoerce_primitive __x1 -> ()
    method module_type : module_type -> unit =
      fun __value -> (self#module_type_desc __value.mty_desc; ())
    method module_type_desc : module_type_desc -> unit =
      fun __value ->
        match __value with
        | Tmty_ident (__x1, __x2) -> ()
        | Tmty_signature __x1 -> (self#signature __x1; ())
        | Tmty_functor (__x1, __x2, __x3, __x4) ->
            (self#module_type __x3; self#module_type __x4; ())
        | Tmty_with (__x1, __x2) ->
            (self#module_type __x1;
             self#list
               (fun (__x1, __x2, __x3) -> (self#with_constraint __x3; ()))
               __x2;
             ())
        | Tmty_typeof __x1 -> (self#module_expr __x1; ())
    method signature : signature -> unit =
      fun __value -> (self#list self#signature_item __value.sig_items; ())
    method signature_item : signature_item -> unit =
      fun __value -> (self#signature_item_desc __value.sig_desc; ())
    method signature_item_desc : signature_item_desc -> unit =
      fun __value ->
        match __value with
        | Tsig_value (__x1, __x2, __x3) -> (self#value_description __x3; ())
        | Tsig_type __x1 ->
            (self#list
               (fun (__x1, __x2, __x3) -> (self#type_declaration __x3; ()))
               __x1;
             ())
        | Tsig_exception (__x1, __x2, __x3) ->
            (self#exception_declaration __x3; ())
        | Tsig_module (__x1, __x2, __x3) -> (self#module_type __x3; ())
        | Tsig_recmodule __x1 ->
            (self#list
               (fun (__x1, __x2, __x3) -> (self#module_type __x3; ())) __x1;
             ())
        | Tsig_modtype (__x1, __x2, __x3) ->
            (self#modtype_declaration __x3; ())
        | Tsig_open (__x1, __x2, __x3) -> ()
        | Tsig_include (__x1, __x2) -> (self#module_type __x1; ())
        | Tsig_class __x1 -> (self#list self#class_description __x1; ())
        | Tsig_class_type __x1 ->
            (self#list self#class_type_declaration __x1; ())
    method modtype_declaration : modtype_declaration -> unit =
      fun __value ->
        match __value with
        | Tmodtype_abstract -> ()
        | Tmodtype_manifest __x1 -> (self#module_type __x1; ())
    method with_constraint : with_constraint -> unit =
      fun __value ->
        match __value with
        | Twith_type __x1 -> (self#type_declaration __x1; ())
        | Twith_module (__x1, __x2) -> ()
        | Twith_typesubst __x1 -> (self#type_declaration __x1; ())
        | Twith_modsubst (__x1, __x2) -> ()
    method core_type : core_type -> unit =
      fun __value -> (self#core_type_desc __value.ctyp_desc; ())
    method core_type_desc : core_type_desc -> unit =
      fun __value ->
        match __value with
        | Ttyp_any -> ()
        | Ttyp_var __x1 -> ()
        | Ttyp_arrow (__x1, __x2, __x3) ->
            (self#core_type __x2; self#core_type __x3; ())
        | Ttyp_tuple __x1 -> (self#list self#core_type __x1; ())
        | Ttyp_constr (__x1, __x2, __x3) ->
            (self#list self#core_type __x3; ())
        | Ttyp_object __x1 -> (self#list self#core_field_type __x1; ())
        | Ttyp_class (__x1, __x2, __x3, __x4) ->
            (self#list self#core_type __x3; self#list (fun _ -> ()) __x4; ())
        | Ttyp_alias (__x1, __x2) -> (self#core_type __x1; ())
        | Ttyp_variant (__x1, __x2, __x3) ->
            (self#list self#row_field __x1;
             self#option (self#list (fun _ -> ())) __x3;
             ())
        | Ttyp_poly (__x1, __x2) ->
            (self#list (fun _ -> ()) __x1; self#core_type __x2; ())
        | Ttyp_package __x1 -> (self#package_type __x1; ())
    method package_type : package_type -> unit =
      fun __value ->
        (self#list (fun (__x1, __x2) -> (self#core_type __x2; ()))
           __value.pack_fields;
         ())
    method core_field_type : core_field_type -> unit =
      fun __value -> (self#core_field_desc __value.field_desc; ())
    method core_field_desc : core_field_desc -> unit =
      fun __value ->
        match __value with
        | Tcfield (__x1, __x2) -> (self#core_type __x2; ())
        | Tcfield_var -> ()
    method row_field : row_field -> unit =
      fun __value ->
        match __value with
        | Ttag (__x1, __x2, __x3) -> (self#list self#core_type __x3; ())
        | Tinherit __x1 -> (self#core_type __x1; ())
    method value_description : value_description -> unit =
      fun __value ->
        (self#core_type __value.val_desc;
         self#list (fun _ -> ()) __value.val_prim;
         ())
    method type_declaration : type_declaration -> unit =
      fun __value ->
        (self#list (self#option (fun _ -> ())) __value.typ_params;
         self#list
           (fun (__x1, __x2, __x3) ->
              (self#core_type __x1; self#core_type __x2; ()))
           __value.typ_cstrs;
         self#type_kind __value.typ_kind;
         self#option self#core_type __value.typ_manifest;
         self#list (fun (__x1, __x2) -> ()) __value.typ_variance;
         ())
    method type_kind : type_kind -> unit =
      fun __value ->
        match __value with
        | Ttype_abstract -> ()
        | Ttype_variant __x1 ->
            (self#list
               (fun (__x1, __x2, __x3, __x4) ->
                  (self#list self#core_type __x3; ()))
               __x1;
             ())
        | Ttype_record __x1 ->
            (self#list
               (fun (__x1, __x2, __x3, __x4, __x5) ->
                  (self#core_type __x4; ()))
               __x1;
             ())
    method exception_declaration : exception_declaration -> unit =
      fun __value -> (self#list self#core_type __value.exn_params; ())
    method class_type : class_type -> unit =
      fun __value -> (self#class_type_desc __value.cltyp_desc; ())
    method class_type_desc : class_type_desc -> unit =
      fun __value ->
        match __value with
        | Tcty_constr (__x1, __x2, __x3) ->
            (self#list self#core_type __x3; ())
        | Tcty_signature __x1 -> (self#class_signature __x1; ())
        | Tcty_fun (__x1, __x2, __x3) ->
            (self#core_type __x2; self#class_type __x3; ())
    method class_signature : class_signature -> unit =
      fun __value ->
        (self#core_type __value.csig_self;
         self#list self#class_type_field __value.csig_fields;
         ())
    method class_type_field : class_type_field -> unit =
      fun __value -> (self#class_type_field_desc __value.ctf_desc; ())
    method class_type_field_desc : class_type_field_desc -> unit =
      fun __value ->
        match __value with
        | Tctf_inher __x1 -> (self#class_type __x1; ())
        | Tctf_val __x1 ->
            ((fun (__x1, __x2, __x3, __x4) -> (self#core_type __x4; ())) __x1;
             ())
        | Tctf_virt __x1 ->
            ((fun (__x1, __x2, __x3) -> (self#core_type __x3; ())) __x1; ())
        | Tctf_meth __x1 ->
            ((fun (__x1, __x2, __x3) -> (self#core_type __x3; ())) __x1; ())
        | Tctf_cstr __x1 ->
            ((fun (__x1, __x2) ->
                (self#core_type __x1; self#core_type __x2; ()))
               __x1;
             ())
    method class_declaration : class_declaration -> unit =
      fun __value -> self#class_infos self#class_expr __value
    method class_description : class_description -> unit =
      fun __value -> self#class_infos self#class_type __value
    method class_type_declaration : class_type_declaration -> unit =
      fun __value -> self#class_infos self#class_type __value
    method class_infos : 'a. ('a -> unit) -> 'a class_infos -> unit =
      fun __tv_a __value ->
        ((fun (__x1, __x2) -> (self#list (fun _ -> ()) __x1; ()))
           __value.ci_params;
         __tv_a __value.ci_expr;
         self#list (fun (__x1, __x2) -> ()) __value.ci_variance;
         ())
  end

class ovisit = object
  inherit ovisit_pattern
  method ref f x = f !x
  method option f = function
    | None -> ()
    | Some v -> f v
  method list f xs = List.iter f xs
end

