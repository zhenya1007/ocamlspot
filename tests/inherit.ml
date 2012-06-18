class (* c => *) c (* <= c *) = object
  val (* x => *) x (* <= x *) = 1 
end 

class nc = object
  inherit let _x = 1 in c (* ? c *)
  val y = 1
  method m = x
end

class nnc = object
  inherit let _y = 1 in (* nc => *) nc (* <= nc *)
  method n = y (* ? nc *)
end
