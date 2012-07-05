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

open Typedtree

class fold : object ('self)
  method class_declaration      : class_declaration -> 'self
  method class_description      : class_description -> 'self
  method class_expr             : class_expr -> 'self
  method class_expr_desc        : class_expr_desc -> 'self
  method class_field            : class_field -> 'self
  method class_field_desc       : class_field_desc -> 'self
  method class_field_kind       : class_field_kind -> 'self
  method class_infos            : ('self -> 'a -> 'self) -> 'a class_infos -> 'self
  method class_signature        : class_signature -> 'self
  method class_structure        : class_structure -> 'self
  method class_type             : class_type -> 'self
  method class_type_declaration : class_type_declaration -> 'self
  method class_type_desc        : class_type_desc -> 'self
  method class_type_field       : class_type_field -> 'self
  method class_type_field_desc  : class_type_field_desc -> 'self
  method core_field_desc        : core_field_desc -> 'self
  method core_field_type        : core_field_type -> 'self
  method core_type              : core_type -> 'self
  method core_type_desc         : core_type_desc -> 'self
  method exception_declaration  : exception_declaration -> 'self
  method exp_extra              : exp_extra -> 'self
  method expression             : expression -> 'self
  method expression_desc        : expression_desc -> 'self
  method meth                   : meth -> 'self
  method modtype_declaration    : modtype_declaration -> 'self
  method module_coercion        : module_coercion -> 'self
  method module_expr            : module_expr -> 'self
  method module_expr_desc       : module_expr_desc -> 'self
  method module_type            : module_type -> 'self
  method module_type_constraint : module_type_constraint -> 'self
  method module_type_desc       : module_type_desc -> 'self
  method mutable_flag           : Asttypes.mutable_flag -> 'self
  method package_type           : package_type -> 'self
  method partial                : partial -> 'self
  method pat_extra              : pat_extra -> 'self
  method pattern                : pattern -> 'self
  method pattern_desc           : pattern_desc -> 'self
  method row_field              : row_field -> 'self
  method signature              : signature -> 'self
  method signature_item         : signature_item -> 'self
  method signature_item_desc    : signature_item_desc -> 'self
  method structure              : structure -> 'self
  method structure_item         : structure_item -> 'self
  method structure_item_desc    : structure_item_desc -> 'self
  method type_declaration       : type_declaration -> 'self
  method type_kind              : type_kind -> 'self
  method value_description      : value_description -> 'self
  method with_constraint        : with_constraint -> 'self

  method unknown : 'a -> 'self

  method direction_flag          : Asttypes.direction_flag -> 'self
  method string                  : string -> 'self
  method private_flag            : Asttypes.private_flag -> 'self
  method option                  : ('self -> 'a -> 'self) -> 'a option -> 'self
  method optional                : optional -> 'self
  method override_flag           : Asttypes.override_flag -> 'self
  method int                     : int -> 'self
  method loc                     : ('self -> 'a -> 'self) -> 'a Asttypes.loc -> 'self
  method label                   : Asttypes.label -> 'self
  method list                    : ('self -> 'a -> 'self) -> 'a list -> 'self
  method label_description       : Types.label_description -> 'self
  method rec_flag                : Asttypes.rec_flag -> 'self
  method ref                     : ('self -> 'a -> 'self) -> 'a ref -> 'self
  method row_desc                : Types.row_desc -> 'self
  method bool                    : bool -> 'self
  method closed_flag             : Asttypes.closed_flag -> 'self
  method constant                : Asttypes.constant -> 'self
  method constructor_description : Types.constructor_description -> 'self
  method type_expr               : Types.type_expr -> 'self
  method virtual_flag            : Asttypes.virtual_flag -> 'self
end
