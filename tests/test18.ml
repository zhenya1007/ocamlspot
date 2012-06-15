type (* constr E => *) t = E (* <= constr E *)

module E = struct let (* module E => *) x (* <= module E *) = 1 end

module type (* modtype E => *) E (* <= modtype E *) = sig val x : int end 

let _ = E (* ? constr E *)

exception (* exception E => *) E (* <= exception E *)

let _ = raise E (* ? exception E *)

let _ = E.x (* ? module E *)

module M : E (* ? modtype E *) = struct
  let x = 1
end
