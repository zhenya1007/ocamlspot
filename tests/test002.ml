module M = struct
  let m1 = 1
  let m2 = 2
end

include (M : sig val m1 : int end)

