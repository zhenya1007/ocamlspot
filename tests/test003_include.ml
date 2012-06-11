module M = struct
  type t = int
  let v = 1
  type u = float
  let w = 2
  class c = object end
  exception E
end

include M
