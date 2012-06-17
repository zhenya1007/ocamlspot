module M0 : sig
  class (* M0.c => *) c (* <= M0.c *): object end 
end
(* <= M0 *)

module Test : sig
  val v : M0.c (* ? M0.c *)
  class type (* ct => *) ct (* <= ct *)= M0.c 
  class type ct' = M0.c (* ? M0.c *) 
  val z : ct (* ? ct *)
end
