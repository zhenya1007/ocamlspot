let z = 1

type (* type t => *) t (* <= type t *) = (* constr E => *) E (* <= constr E *)

module (* module E => *) E (* <= module E *) = struct let (* E.x => *) x (* <= E.x *) = 1 end 

module type (* modtype E => *) E (* <= modtype E *) = sig val x : int end 

let _ = E (* ? constr E *)

exception (* exception E => *) E (* <= exception E *)

let _ = raise E (* ? exception E *)

let _ = E.x (* ? E.x *)

module (* module M => *) M (* <= module M *): E (* ? modtype E *) = struct
  let x = 1
end

