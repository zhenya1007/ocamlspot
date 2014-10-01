module (* M => *) M (* <= M *) = struct let x = 1 end
module type S = sig module M = M (* ? M *) end

