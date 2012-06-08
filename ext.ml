module Longident = struct
  include Longident
  include Xlongident
end

module Ident0 = Ident

module Ident = struct
  include Ident
  include Xident
end

module Path0 = Path

module Path = struct
  include Path
  include Xpath
end

module Printtyp = struct
  include Printtyp
  include Xprinttyp
end

module Parser = struct
  include Parser
  let locident _ _ = assert false
end

