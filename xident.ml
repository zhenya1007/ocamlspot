(* extend the original module *)
open Ident

(* extend the original Ident module *)
  
let name id =
  let binding_time = binding_time id in
  Name.create (name id) binding_time
      
module Ident_internal : sig
  val unsafe_create_with_stamp : ?flags: int -> string -> int -> Ident.t
end= struct
  (* Stamp is untouchable outside of ident.ml. A dirty
     workaround *)
  type t = { stamp: int; name: string; mutable flags: int }
  let to_ident (id : t) = (Obj.magic id : Ident.t)
    
  (* It is dangerous operation! *)        
    let unsafe_create_with_stamp ?(flags=0) name stamp =
      to_ident { stamp = stamp; name = name; flags = flags }
end
    
let unsafe_create_with_stamp = Ident_internal.unsafe_create_with_stamp

let parse s =
  let s, pos = Name.parse s in
  let id = unsafe_create_with_stamp s pos in
  (* CR jfuruse: actually 0 is global and should be printed as 'G'
     Current 'G' means -1 *)
  if pos = 0 then make_global id;
  id

