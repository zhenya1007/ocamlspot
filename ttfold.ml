open Asttypes
open Types
open Typedtree
  
class fold =
  object ((o : 'self_type))
    method string : string -> 'self_type = o#unknown
    method ref :
      'a. ('self_type -> 'a -> 'self_type) -> 'a ref -> 'self_type =
      fun _f_a { contents = _x } -> let o = _f_a o _x in o
    method option :
      'a. ('self_type -> 'a -> 'self_type) -> 'a option -> 'self_type =
      fun _f_a -> function | None -> o | Some _x -> let o = _f_a o _x in o
    method list :
      'a. ('self_type -> 'a -> 'self_type) -> 'a list -> 'self_type =
      fun _f_a ->
        function
        | [] -> o
        | _x :: _x_i1 -> let o = _f_a o _x in let o = o#list _f_a _x_i1 in o
    method int : int -> 'self_type = o#unknown
    method bool : bool -> 'self_type = function | false -> o | true -> o
    method with_constraint : with_constraint -> 'self_type =
      function
      | Twith_type _x -> let o = o#type_declaration _x in o
      | Twith_module (_x, _x_i1) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#unknown) _x_i1 in o
      | Twith_typesubst _x -> let o = o#type_declaration _x in o
      | Twith_modsubst (_x, _x_i1) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#unknown) _x_i1 in o
    method virtual_flag : virtual_flag -> 'self_type = o#unknown
    method value_description : value_description -> 'self_type =
      fun { val_desc = _x; val_val = _x_i1; val_prim = _x_i2; val_loc = _x_i3
        } ->
        let o = o#core_type _x in
        let o = o#unknown _x_i1 in
        let o = o#list (fun o -> o#string) _x_i2 in
        let o = o#unknown _x_i3 in o
    method type_kind : type_kind -> 'self_type =
      function
      | Ttype_abstract -> o
      | Ttype_variant _x ->
          let o =
            o#list
              (fun o (_x, _x_i1, _x_i2, _x_i3) ->
                 let o = o#unknown _x in
                 let o = o#loc (fun o -> o#string) _x_i1 in
                 let o = o#list (fun o -> o#core_type) _x_i2 in
                 let o = o#unknown _x_i3 in o)
              _x
          in o
      | Ttype_record _x ->
          let o =
            o#list
              (fun o (_x, _x_i1, _x_i2, _x_i3, _x_i4) ->
                 let o = o#unknown _x in
                 let o = o#loc (fun o -> o#string) _x_i1 in
                 let o = o#mutable_flag _x_i2 in
                 let o = o#core_type _x_i3 in let o = o#unknown _x_i4 in o)
              _x
          in o
    method type_expr : type_expr -> 'self_type = o#unknown
    method type_declaration : type_declaration -> 'self_type =
      fun
        {
          typ_params = _x;
          typ_type = _x_i1;
          typ_cstrs = _x_i2;
          typ_kind = _x_i3;
          typ_private = _x_i4;
          typ_manifest = _x_i5;
          typ_variance = _x_i6;
          typ_loc = _x_i7
        } ->
        let o =
          o#list (fun o -> o#option (fun o -> o#loc (fun o -> o#string))) _x in
        let o = o#unknown _x_i1 in
        let o =
          o#list
            (fun o (_x, _x_i1, _x_i2) ->
               let o = o#core_type _x in
               let o = o#core_type _x_i1 in let o = o#unknown _x_i2 in o)
            _x_i2 in
        let o = o#type_kind _x_i3 in
        let o = o#private_flag _x_i4 in
        let o = o#option (fun o -> o#core_type) _x_i5 in
        let o =
          o#list
            (fun o (_x, _x_i1) ->
               let o = o#bool _x in let o = o#bool _x_i1 in o)
            _x_i6 in
        let o = o#unknown _x_i7 in o
    method structure_item_desc : structure_item_desc -> 'self_type =
      function
      | Tstr_eval _x -> let o = o#expression _x in o
      | Tstr_value (_x, _x_i1) ->
          let o = o#rec_flag _x in
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#pattern _x in let o = o#expression _x_i1 in o)
              _x_i1
          in o
      | Tstr_primitive (_x, _x_i1, _x_i2) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#string) _x_i1 in
          let o = o#value_description _x_i2 in o
      | Tstr_type _x ->
          let o =
            o#list
              (fun o (_x, _x_i1, _x_i2) ->
                 let o = o#unknown _x in
                 let o = o#loc (fun o -> o#string) _x_i1 in
                 let o = o#type_declaration _x_i2 in o)
              _x
          in o
      | Tstr_exception (_x, _x_i1, _x_i2) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#string) _x_i1 in
          let o = o#exception_declaration _x_i2 in o
      | Tstr_exn_rebind (_x, _x_i1, _x_i2, _x_i3) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#string) _x_i1 in
          let o = o#unknown _x_i2 in
          let o = o#loc (fun o -> o#unknown) _x_i3 in o
      | Tstr_module (_x, _x_i1, _x_i2) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#string) _x_i1 in
          let o = o#module_expr _x_i2 in o
      | Tstr_recmodule _x ->
          let o =
            o#list
              (fun o (_x, _x_i1, _x_i2, _x_i3) ->
                 let o = o#unknown _x in
                 let o = o#loc (fun o -> o#string) _x_i1 in
                 let o = o#module_type _x_i2 in
                 let o = o#module_expr _x_i3 in o)
              _x
          in o
      | Tstr_modtype (_x, _x_i1, _x_i2) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#string) _x_i1 in
          let o = o#module_type _x_i2 in o
      | Tstr_open (_x, _x_i1) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#unknown) _x_i1 in o
      | Tstr_class _x ->
          let o =
            o#list
              (fun o (_x, _x_i1, _x_i2) ->
                 let o = o#class_declaration _x in
                 let o = o#list (fun o -> o#string) _x_i1 in
                 let o = o#virtual_flag _x_i2 in o)
              _x
          in o
      | Tstr_class_type _x ->
          let o =
            o#list
              (fun o (_x, _x_i1, _x_i2) ->
                 let o = o#unknown _x in
                 let o = o#loc (fun o -> o#string) _x_i1 in
                 let o = o#class_type_declaration _x_i2 in o)
              _x
          in o
      | Tstr_include (_x, _x_i1) ->
          let o = o#module_expr _x in
          let o = o#list (fun o -> o#unknown) _x_i1 in o
    method structure_item : structure_item -> 'self_type =
      fun { str_desc = _x; str_loc = _x_i1; str_env = _x_i2 } ->
        let o = o#structure_item_desc _x in
        let o = o#unknown _x_i1 in let o = o#unknown _x_i2 in o
    method structure : structure -> 'self_type =
      fun { str_items = _x; str_type = _x_i1; str_final_env = _x_i2 } ->
        let o = o#list (fun o -> o#structure_item) _x in
        let o = o#unknown _x_i1 in let o = o#unknown _x_i2 in o
    method signature_item_desc : signature_item_desc -> 'self_type =
      function
      | Tsig_value (_x, _x_i1, _x_i2) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#string) _x_i1 in
          let o = o#value_description _x_i2 in o
      | Tsig_type _x ->
          let o =
            o#list
              (fun o (_x, _x_i1, _x_i2) ->
                 let o = o#unknown _x in
                 let o = o#loc (fun o -> o#string) _x_i1 in
                 let o = o#type_declaration _x_i2 in o)
              _x
          in o
      | Tsig_exception (_x, _x_i1, _x_i2) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#string) _x_i1 in
          let o = o#exception_declaration _x_i2 in o
      | Tsig_module (_x, _x_i1, _x_i2) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#string) _x_i1 in
          let o = o#module_type _x_i2 in o
      | Tsig_recmodule _x ->
          let o =
            o#list
              (fun o (_x, _x_i1, _x_i2) ->
                 let o = o#unknown _x in
                 let o = o#loc (fun o -> o#string) _x_i1 in
                 let o = o#module_type _x_i2 in o)
              _x
          in o
      | Tsig_modtype (_x, _x_i1, _x_i2) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#string) _x_i1 in
          let o = o#modtype_declaration _x_i2 in o
      | Tsig_open (_x, _x_i1) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#unknown) _x_i1 in o
      | Tsig_include (_x, _x_i1) ->
          let o = o#module_type _x in let o = o#unknown _x_i1 in o
      | Tsig_class _x ->
          let o = o#list (fun o -> o#class_description) _x in o
      | Tsig_class_type _x ->
          let o = o#list (fun o -> o#class_type_declaration) _x in o
    method signature_item : signature_item -> 'self_type =
      fun { sig_desc = _x; sig_env = _x_i1; sig_loc = _x_i2 } ->
        let o = o#signature_item_desc _x in
        let o = o#unknown _x_i1 in let o = o#unknown _x_i2 in o
    method signature : signature -> 'self_type =
      fun { sig_items = _x; sig_type = _x_i1; sig_final_env = _x_i2 } ->
        let o = o#list (fun o -> o#signature_item) _x in
        let o = o#unknown _x_i1 in let o = o#unknown _x_i2 in o
    method row_field : row_field -> 'self_type =
      function
      | Ttag (_x, _x_i1, _x_i2) ->
          let o = o#label _x in
          let o = o#bool _x_i1 in
          let o = o#list (fun o -> o#core_type) _x_i2 in o
      | Tinherit _x -> let o = o#core_type _x in o
    method row_desc : row_desc -> 'self_type = o#unknown
    method rec_flag : rec_flag -> 'self_type = o#unknown
    method private_flag : private_flag -> 'self_type = o#unknown
    method pattern_desc : pattern_desc -> 'self_type =
      function
      | Tpat_any -> o
      | Tpat_var (_x, _x_i1) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#string) _x_i1 in o
      | Tpat_alias (_x, _x_i1, _x_i2) ->
          let o = o#pattern _x in
          let o = o#unknown _x_i1 in
          let o = o#loc (fun o -> o#string) _x_i2 in o
      | Tpat_constant _x -> let o = o#constant _x in o
      | Tpat_tuple _x -> let o = o#list (fun o -> o#pattern) _x in o
      | Tpat_construct (_x, _x_i1, _x_i2, _x_i3, _x_i4) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#unknown) _x_i1 in
          let o = o#constructor_description _x_i2 in
          let o = o#list (fun o -> o#pattern) _x_i3 in
          let o = o#bool _x_i4 in o
      | Tpat_variant (_x, _x_i1, _x_i2) ->
          let o = o#label _x in
          let o = o#option (fun o -> o#pattern) _x_i1 in
          let o = o#ref (fun o -> o#row_desc) _x_i2 in o
      | Tpat_record (_x, _x_i1) ->
          let o =
            o#list
              (fun o (_x, _x_i1, _x_i2, _x_i3) ->
                 let o = o#unknown _x in
                 let o = o#loc (fun o -> o#unknown) _x_i1 in
                 let o = o#label_description _x_i2 in
                 let o = o#pattern _x_i3 in o)
              _x in
          let o = o#closed_flag _x_i1 in o
      | Tpat_array _x -> let o = o#list (fun o -> o#pattern) _x in o
      | Tpat_or (_x, _x_i1, _x_i2) ->
          let o = o#pattern _x in
          let o = o#pattern _x_i1 in
          let o = o#option (fun o -> o#row_desc) _x_i2 in o
      | Tpat_lazy _x -> let o = o#pattern _x in o
    method pattern : pattern -> 'self_type =
      fun
        {
          pat_desc = _x;
          pat_loc = _x_i1;
          pat_extra = _x_i2;
          pat_type = _x_i3;
          pat_env = _x_i4
        } ->
        let o = o#pattern_desc _x in
        let o = o#unknown _x_i1 in
        let o =
          o#list
            (fun o (_x, _x_i1) ->
               let o = o#pat_extra _x in let o = o#unknown _x_i1 in o)
            _x_i2 in
        let o = o#type_expr _x_i3 in let o = o#unknown _x_i4 in o
    method pat_extra : pat_extra -> 'self_type =
      function
      | Tpat_constraint _x -> let o = o#core_type _x in o
      | Tpat_type (_x, _x_i1) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#unknown) _x_i1 in o
      | Tpat_unpack -> o
    method partial : partial -> 'self_type =
      function | Partial -> o | Total -> o
    method package_type : package_type -> 'self_type =
      fun
        {
          pack_name = _x;
          pack_fields = _x_i1;
          pack_type = _x_i2;
          pack_txt = _x_i3
        } ->
        let o = o#unknown _x in
        let o =
          o#list
            (fun o (_x, _x_i1) ->
               let o = o#loc (fun o -> o#unknown) _x in
               let o = o#core_type _x_i1 in o)
            _x_i1 in
        let o = o#unknown _x_i2 in
        let o = o#loc (fun o -> o#unknown) _x_i3 in o
    method override_flag : override_flag -> 'self_type = o#unknown
    method optional : optional -> 'self_type =
      function | Required -> o | Optional -> o
    method mutable_flag : mutable_flag -> 'self_type = o#unknown
    method module_type_desc : module_type_desc -> 'self_type =
      function
      | Tmty_ident (_x, _x_i1) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#unknown) _x_i1 in o
      | Tmty_signature _x -> let o = o#signature _x in o
      | Tmty_functor (_x, _x_i1, _x_i2, _x_i3) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#string) _x_i1 in
          let o = o#module_type _x_i2 in let o = o#module_type _x_i3 in o
      | Tmty_with (_x, _x_i1) ->
          let o = o#module_type _x in
          let o =
            o#list
              (fun o (_x, _x_i1, _x_i2) ->
                 let o = o#unknown _x in
                 let o = o#loc (fun o -> o#unknown) _x_i1 in
                 let o = o#with_constraint _x_i2 in o)
              _x_i1
          in o
      | Tmty_typeof _x -> let o = o#module_expr _x in o
    method module_type_constraint : module_type_constraint -> 'self_type =
      function
      | Tmodtype_implicit -> o
      | Tmodtype_explicit _x -> let o = o#module_type _x in o
    method module_type : module_type -> 'self_type =
      fun { mty_desc = _x; mty_type = _x_i1; mty_env = _x_i2; mty_loc = _x_i3
        } ->
        let o = o#module_type_desc _x in
        let o = o#unknown _x_i1 in
        let o = o#unknown _x_i2 in let o = o#unknown _x_i3 in o
    method module_expr_desc : module_expr_desc -> 'self_type =
      function
      | Tmod_ident (_x, _x_i1) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#unknown) _x_i1 in o
      | Tmod_structure _x -> let o = o#structure _x in o
      | Tmod_functor (_x, _x_i1, _x_i2, _x_i3) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#string) _x_i1 in
          let o = o#module_type _x_i2 in let o = o#module_expr _x_i3 in o
      | Tmod_apply (_x, _x_i1, _x_i2) ->
          let o = o#module_expr _x in
          let o = o#module_expr _x_i1 in let o = o#module_coercion _x_i2 in o
      | Tmod_constraint (_x, _x_i1, _x_i2, _x_i3) ->
          let o = o#module_expr _x in
          let o = o#unknown _x_i1 in
          let o = o#module_type_constraint _x_i2 in
          let o = o#module_coercion _x_i3 in o
      | Tmod_unpack (_x, _x_i1) ->
          let o = o#expression _x in let o = o#unknown _x_i1 in o
    method module_expr : module_expr -> 'self_type =
      fun { mod_desc = _x; mod_loc = _x_i1; mod_type = _x_i2; mod_env = _x_i3
        } ->
        let o = o#module_expr_desc _x in
        let o = o#unknown _x_i1 in
        let o = o#unknown _x_i2 in let o = o#unknown _x_i3 in o
    method module_coercion : module_coercion -> 'self_type =
      function
      | Tcoerce_none -> o
      | Tcoerce_structure _x ->
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#int _x in let o = o#module_coercion _x_i1 in o)
              _x
          in o
      | Tcoerce_functor (_x, _x_i1) ->
          let o = o#module_coercion _x in
          let o = o#module_coercion _x_i1 in o
      | Tcoerce_primitive _x -> let o = o#unknown _x in o
    method modtype_declaration : modtype_declaration -> 'self_type =
      function
      | Tmodtype_abstract -> o
      | Tmodtype_manifest _x -> let o = o#module_type _x in o
    method meth : meth -> 'self_type =
      function
      | Tmeth_name _x -> let o = o#string _x in o
      | Tmeth_val _x -> let o = o#unknown _x in o
    method loc :
      'a. ('self_type -> 'a -> 'self_type) -> 'a loc -> 'self_type =
      fun _f_a -> o#unknown
    method label_description : label_description -> 'self_type = o#unknown
    method label : label -> 'self_type = o#unknown
    method expression_desc : expression_desc -> 'self_type =
      function
      | Texp_ident (_x, _x_i1, _x_i2) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#unknown) _x_i1 in
          let o = o#unknown _x_i2 in o
      | Texp_constant _x -> let o = o#constant _x in o
      | Texp_let (_x, _x_i1, _x_i2) ->
          let o = o#rec_flag _x in
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#pattern _x in let o = o#expression _x_i1 in o)
              _x_i1 in
          let o = o#expression _x_i2 in o
      | Texp_function (_x, _x_i1, _x_i2) ->
          let o = o#label _x in
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#pattern _x in let o = o#expression _x_i1 in o)
              _x_i1 in
          let o = o#partial _x_i2 in o
      | Texp_apply (_x, _x_i1) ->
          let o = o#expression _x in
          let o =
            o#list
              (fun o (_x, _x_i1, _x_i2) ->
                 let o = o#label _x in
                 let o = o#option (fun o -> o#expression) _x_i1 in
                 let o = o#optional _x_i2 in o)
              _x_i1
          in o
      | Texp_match (_x, _x_i1, _x_i2) ->
          let o = o#expression _x in
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#pattern _x in let o = o#expression _x_i1 in o)
              _x_i1 in
          let o = o#partial _x_i2 in o
      | Texp_try (_x, _x_i1) ->
          let o = o#expression _x in
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#pattern _x in let o = o#expression _x_i1 in o)
              _x_i1
          in o
      | Texp_tuple _x -> let o = o#list (fun o -> o#expression) _x in o
      | Texp_construct (_x, _x_i1, _x_i2, _x_i3, _x_i4) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#unknown) _x_i1 in
          let o = o#constructor_description _x_i2 in
          let o = o#list (fun o -> o#expression) _x_i3 in
          let o = o#bool _x_i4 in o
      | Texp_variant (_x, _x_i1) ->
          let o = o#label _x in
          let o = o#option (fun o -> o#expression) _x_i1 in o
      | Texp_record (_x, _x_i1) ->
          let o =
            o#list
              (fun o (_x, _x_i1, _x_i2, _x_i3) ->
                 let o = o#unknown _x in
                 let o = o#loc (fun o -> o#unknown) _x_i1 in
                 let o = o#label_description _x_i2 in
                 let o = o#expression _x_i3 in o)
              _x in
          let o = o#option (fun o -> o#expression) _x_i1 in o
      | Texp_field (_x, _x_i1, _x_i2, _x_i3) ->
          let o = o#expression _x in
          let o = o#unknown _x_i1 in
          let o = o#loc (fun o -> o#unknown) _x_i2 in
          let o = o#label_description _x_i3 in o
      | Texp_setfield (_x, _x_i1, _x_i2, _x_i3, _x_i4) ->
          let o = o#expression _x in
          let o = o#unknown _x_i1 in
          let o = o#loc (fun o -> o#unknown) _x_i2 in
          let o = o#label_description _x_i3 in
          let o = o#expression _x_i4 in o
      | Texp_array _x -> let o = o#list (fun o -> o#expression) _x in o
      | Texp_ifthenelse (_x, _x_i1, _x_i2) ->
          let o = o#expression _x in
          let o = o#expression _x_i1 in
          let o = o#option (fun o -> o#expression) _x_i2 in o
      | Texp_sequence (_x, _x_i1) ->
          let o = o#expression _x in let o = o#expression _x_i1 in o
      | Texp_while (_x, _x_i1) ->
          let o = o#expression _x in let o = o#expression _x_i1 in o
      | Texp_for (_x, _x_i1, _x_i2, _x_i3, _x_i4, _x_i5) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#string) _x_i1 in
          let o = o#expression _x_i2 in
          let o = o#expression _x_i3 in
          let o = o#direction_flag _x_i4 in let o = o#expression _x_i5 in o
      | Texp_when (_x, _x_i1) ->
          let o = o#expression _x in let o = o#expression _x_i1 in o
      | Texp_send (_x, _x_i1, _x_i2) ->
          let o = o#expression _x in
          let o = o#meth _x_i1 in
          let o = o#option (fun o -> o#expression) _x_i2 in o
      | Texp_new (_x, _x_i1, _x_i2) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#unknown) _x_i1 in
          let o = o#unknown _x_i2 in o
      | Texp_instvar (_x, _x_i1, _x_i2) ->
          let o = o#unknown _x in
          let o = o#unknown _x_i1 in
          let o = o#loc (fun o -> o#string) _x_i2 in o
      | Texp_setinstvar (_x, _x_i1, _x_i2, _x_i3) ->
          let o = o#unknown _x in
          let o = o#unknown _x_i1 in
          let o = o#loc (fun o -> o#string) _x_i2 in
          let o = o#expression _x_i3 in o
      | Texp_override (_x, _x_i1) ->
          let o = o#unknown _x in
          let o =
            o#list
              (fun o (_x, _x_i1, _x_i2) ->
                 let o = o#unknown _x in
                 let o = o#loc (fun o -> o#string) _x_i1 in
                 let o = o#expression _x_i2 in o)
              _x_i1
          in o
      | Texp_letmodule (_x, _x_i1, _x_i2, _x_i3) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#string) _x_i1 in
          let o = o#module_expr _x_i2 in let o = o#expression _x_i3 in o
      | Texp_assert _x -> let o = o#expression _x in o
      | Texp_assertfalse -> o
      | Texp_lazy _x -> let o = o#expression _x in o
      | Texp_poly (_x, _x_i1) ->
          let o = o#expression _x in
          let o = o#option (fun o -> o#core_type) _x_i1 in o
      | Texp_object (_x, _x_i1) ->
          let o = o#class_structure _x in
          let o = o#list (fun o -> o#string) _x_i1 in o
      | Texp_newtype (_x, _x_i1) ->
          let o = o#string _x in let o = o#expression _x_i1 in o
      | Texp_pack _x -> let o = o#module_expr _x in o
    method expression : expression -> 'self_type =
      fun
        {
          exp_desc = _x;
          exp_loc = _x_i1;
          exp_extra = _x_i2;
          exp_type = _x_i3;
          exp_env = _x_i4
        } ->
        let o = o#expression_desc _x in
        let o = o#unknown _x_i1 in
        let o =
          o#list
            (fun o (_x, _x_i1) ->
               let o = o#exp_extra _x in let o = o#unknown _x_i1 in o)
            _x_i2 in
        let o = o#type_expr _x_i3 in let o = o#unknown _x_i4 in o
    method exp_extra : exp_extra -> 'self_type =
      function
      | Texp_constraint (_x, _x_i1) ->
          let o = o#option (fun o -> o#core_type) _x in
          let o = o#option (fun o -> o#core_type) _x_i1 in o
      | Texp_open (_x, _x_i1, _x_i2) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#unknown) _x_i1 in
          let o = o#unknown _x_i2 in o
    method exception_declaration : exception_declaration -> 'self_type =
      fun { exn_params = _x; exn_exn = _x_i1; exn_loc = _x_i2 } ->
        let o = o#list (fun o -> o#core_type) _x in
        let o = o#unknown _x_i1 in let o = o#unknown _x_i2 in o
    method direction_flag : direction_flag -> 'self_type = o#unknown
    method core_type_desc : core_type_desc -> 'self_type =
      function
      | Ttyp_any -> o
      | Ttyp_var _x -> let o = o#string _x in o
      | Ttyp_arrow (_x, _x_i1, _x_i2) ->
          let o = o#label _x in
          let o = o#core_type _x_i1 in let o = o#core_type _x_i2 in o
      | Ttyp_tuple _x -> let o = o#list (fun o -> o#core_type) _x in o
      | Ttyp_constr (_x, _x_i1, _x_i2) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#unknown) _x_i1 in
          let o = o#list (fun o -> o#core_type) _x_i2 in o
      | Ttyp_object _x -> let o = o#list (fun o -> o#core_field_type) _x in o
      | Ttyp_class (_x, _x_i1, _x_i2, _x_i3) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#unknown) _x_i1 in
          let o = o#list (fun o -> o#core_type) _x_i2 in
          let o = o#list (fun o -> o#label) _x_i3 in o
      | Ttyp_alias (_x, _x_i1) ->
          let o = o#core_type _x in let o = o#string _x_i1 in o
      | Ttyp_variant (_x, _x_i1, _x_i2) ->
          let o = o#list (fun o -> o#row_field) _x in
          let o = o#bool _x_i1 in
          let o = o#option (fun o -> o#list (fun o -> o#label)) _x_i2 in o
      | Ttyp_poly (_x, _x_i1) ->
          let o = o#list (fun o -> o#string) _x in
          let o = o#core_type _x_i1 in o
      | Ttyp_package _x -> let o = o#package_type _x in o
    method core_type : core_type -> 'self_type =
      fun
        {
          ctyp_desc = _x;
          ctyp_type = _x_i1;
          ctyp_env = _x_i2;
          ctyp_loc = _x_i3
        } ->
        let o = o#core_type_desc _x in
        let o = o#type_expr _x_i1 in
        let o = o#unknown _x_i2 in let o = o#unknown _x_i3 in o
    method core_field_type : core_field_type -> 'self_type =
      fun { field_desc = _x; field_loc = _x_i1 } ->
        let o = o#core_field_desc _x in let o = o#unknown _x_i1 in o
    method core_field_desc : core_field_desc -> 'self_type =
      function
      | Tcfield (_x, _x_i1) ->
          let o = o#string _x in let o = o#core_type _x_i1 in o
      | Tcfield_var -> o
    method constructor_description : constructor_description -> 'self_type =
      o#unknown
    method constant : constant -> 'self_type = o#unknown
    method closed_flag : closed_flag -> 'self_type = o#unknown
    method class_type_field_desc : class_type_field_desc -> 'self_type =
      function
      | Tctf_inher _x -> let o = o#class_type _x in o
      | Tctf_val _x ->
          let o =
            (fun (_x, _x_i1, _x_i2, _x_i3) ->
               let o = o#string _x in
               let o = o#mutable_flag _x_i1 in
               let o = o#virtual_flag _x_i2 in let o = o#core_type _x_i3 in o)
              _x
          in o
      | Tctf_virt _x ->
          let o =
            (fun (_x, _x_i1, _x_i2) ->
               let o = o#string _x in
               let o = o#private_flag _x_i1 in let o = o#core_type _x_i2 in o)
              _x
          in o
      | Tctf_meth _x ->
          let o =
            (fun (_x, _x_i1, _x_i2) ->
               let o = o#string _x in
               let o = o#private_flag _x_i1 in let o = o#core_type _x_i2 in o)
              _x
          in o
      | Tctf_cstr _x ->
          let o =
            (fun (_x, _x_i1) ->
               let o = o#core_type _x in let o = o#core_type _x_i1 in o)
              _x
          in o
    method class_type_field : class_type_field -> 'self_type =
      fun { ctf_desc = _x; ctf_loc = _x_i1 } ->
        let o = o#class_type_field_desc _x in let o = o#unknown _x_i1 in o
    method class_type_desc : class_type_desc -> 'self_type =
      function
      | Tcty_constr (_x, _x_i1, _x_i2) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#unknown) _x_i1 in
          let o = o#list (fun o -> o#core_type) _x_i2 in o
      | Tcty_signature _x -> let o = o#class_signature _x in o
      | Tcty_fun (_x, _x_i1, _x_i2) ->
          let o = o#label _x in
          let o = o#core_type _x_i1 in let o = o#class_type _x_i2 in o
    method class_type_declaration : class_type_declaration -> 'self_type =
      o#class_infos (fun o -> o#class_type)
    method class_type : class_type -> 'self_type =
      fun
        {
          cltyp_desc = _x;
          cltyp_type = _x_i1;
          cltyp_env = _x_i2;
          cltyp_loc = _x_i3
        } ->
        let o = o#class_type_desc _x in
        let o = o#unknown _x_i1 in
        let o = o#unknown _x_i2 in let o = o#unknown _x_i3 in o
    method class_structure : class_structure -> 'self_type =
      fun
        {
          cstr_pat = _x;
          cstr_fields = _x_i1;
          cstr_type = _x_i2;
          cstr_meths = _x_i3
        } ->
        let o = o#pattern _x in
        let o = o#list (fun o -> o#class_field) _x_i1 in
        let o = o#unknown _x_i2 in let o = o#unknown _x_i3 in o
    method class_signature : class_signature -> 'self_type =
      fun
        {
          csig_self = _x;
          csig_fields = _x_i1;
          csig_type = _x_i2;
          csig_loc = _x_i3
        } ->
        let o = o#core_type _x in
        let o = o#list (fun o -> o#class_type_field) _x_i1 in
        let o = o#unknown _x_i2 in let o = o#unknown _x_i3 in o
    method class_infos :
      'a. ('self_type -> 'a -> 'self_type) -> 'a class_infos -> 'self_type =
      fun _f_a
        {
          ci_virt = _x;
          ci_params = _x_i1;
          ci_id_name = _x_i2;
          ci_id_class = _x_i3;
          ci_id_class_type = _x_i4;
          ci_id_object = _x_i5;
          ci_id_typesharp = _x_i6;
          ci_expr = _x_i7;
          ci_decl = _x_i8;
          ci_type_decl = _x_i9;
          ci_variance = _x_i10;
          ci_loc = _x_i11
        } ->
        let o = o#virtual_flag _x in
        let o =
          (fun (_x, _x_i1) ->
             let o = o#list (fun o -> o#loc (fun o -> o#string)) _x in
             let o = o#unknown _x_i1 in o)
            _x_i1 in
        let o = o#loc (fun o -> o#string) _x_i2 in
        let o = o#unknown _x_i3 in
        let o = o#unknown _x_i4 in
        let o = o#unknown _x_i5 in
        let o = o#unknown _x_i6 in
        let o = _f_a o _x_i7 in
        let o = o#unknown _x_i8 in
        let o = o#unknown _x_i9 in
        let o =
          o#list
            (fun o (_x, _x_i1) ->
               let o = o#bool _x in let o = o#bool _x_i1 in o)
            _x_i10 in
        let o = o#unknown _x_i11 in o
    method class_field_kind : class_field_kind -> 'self_type =
      function
      | Tcfk_virtual _x -> let o = o#core_type _x in o
      | Tcfk_concrete _x -> let o = o#expression _x in o
    method class_field_desc : class_field_desc -> 'self_type =
      function
      | Tcf_inher (_x, _x_i1, _x_i2, _x_i3, _x_i4) ->
          let o = o#override_flag _x in
          let o = o#class_expr _x_i1 in
          let o = o#option (fun o -> o#string) _x_i2 in
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#string _x in let o = o#unknown _x_i1 in o)
              _x_i3 in
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#string _x in let o = o#unknown _x_i1 in o)
              _x_i4
          in o
      | Tcf_val (_x, _x_i1, _x_i2, _x_i3, _x_i4, _x_i5) ->
          let o = o#string _x in
          let o = o#loc (fun o -> o#string) _x_i1 in
          let o = o#mutable_flag _x_i2 in
          let o = o#unknown _x_i3 in
          let o = o#class_field_kind _x_i4 in let o = o#bool _x_i5 in o
      | Tcf_meth (_x, _x_i1, _x_i2, _x_i3, _x_i4) ->
          let o = o#string _x in
          let o = o#loc (fun o -> o#string) _x_i1 in
          let o = o#private_flag _x_i2 in
          let o = o#class_field_kind _x_i3 in let o = o#bool _x_i4 in o
      | Tcf_constr (_x, _x_i1) ->
          let o = o#core_type _x in let o = o#core_type _x_i1 in o
      | Tcf_init _x -> let o = o#expression _x in o
    method class_field : class_field -> 'self_type =
      fun { cf_desc = _x; cf_loc = _x_i1 } ->
        let o = o#class_field_desc _x in let o = o#unknown _x_i1 in o
    method class_expr_desc : class_expr_desc -> 'self_type =
      function
      | Tcl_ident (_x, _x_i1, _x_i2) ->
          let o = o#unknown _x in
          let o = o#loc (fun o -> o#unknown) _x_i1 in
          let o = o#list (fun o -> o#core_type) _x_i2 in o
      | Tcl_structure _x -> let o = o#class_structure _x in o
      | Tcl_fun (_x, _x_i1, _x_i2, _x_i3, _x_i4) ->
          let o = o#label _x in
          let o = o#pattern _x_i1 in
          let o =
            o#list
              (fun o (_x, _x_i1, _x_i2) ->
                 let o = o#unknown _x in
                 let o = o#loc (fun o -> o#string) _x_i1 in
                 let o = o#expression _x_i2 in o)
              _x_i2 in
          let o = o#class_expr _x_i3 in let o = o#partial _x_i4 in o
      | Tcl_apply (_x, _x_i1) ->
          let o = o#class_expr _x in
          let o =
            o#list
              (fun o (_x, _x_i1, _x_i2) ->
                 let o = o#label _x in
                 let o = o#option (fun o -> o#expression) _x_i1 in
                 let o = o#optional _x_i2 in o)
              _x_i1
          in o
      | Tcl_let (_x, _x_i1, _x_i2, _x_i3) ->
          let o = o#rec_flag _x in
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#pattern _x in let o = o#expression _x_i1 in o)
              _x_i1 in
          let o =
            o#list
              (fun o (_x, _x_i1, _x_i2) ->
                 let o = o#unknown _x in
                 let o = o#loc (fun o -> o#string) _x_i1 in
                 let o = o#expression _x_i2 in o)
              _x_i2 in
          let o = o#class_expr _x_i3 in o
      | Tcl_constraint (_x, _x_i1, _x_i2, _x_i3, _x_i4) ->
          let o = o#class_expr _x in
          let o = o#option (fun o -> o#class_type) _x_i1 in
          let o = o#list (fun o -> o#string) _x_i2 in
          let o = o#list (fun o -> o#string) _x_i3 in
          let o = o#unknown _x_i4 in o
    method class_expr : class_expr -> 'self_type =
      fun { cl_desc = _x; cl_loc = _x_i1; cl_type = _x_i2; cl_env = _x_i3 }
        ->
        let o = o#class_expr_desc _x in
        let o = o#unknown _x_i1 in
        let o = o#unknown _x_i2 in let o = o#unknown _x_i3 in o
    method class_description : class_description -> 'self_type =
      o#class_infos (fun o -> o#class_type)
    method class_declaration : class_declaration -> 'self_type =
      o#class_infos (fun o -> o#class_expr)
    method unknown : 'a. 'a -> 'self_type = fun _ -> o
  end
  

