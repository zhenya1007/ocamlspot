(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2014 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

(* extend the original module *)
open Ident

(* extend the original Ident module *)
  
let name id =
  let binding_time = binding_time id in
  Name.create (name id) binding_time
      
let create_with_stamp ?(flags=0) name stamp =
  { stamp = stamp; name = name; flags = flags }

let format ppf id = Format.pp_print_string ppf (name id)

let parse s =
  let s, pos = Name.parse s in
  let id = create_with_stamp s pos in
  (* CR jfuruse: actually 0 is global and should be printed as 'G'
     Current 'G' means -1 *)
  if pos = 0 then make_global id;
  id

