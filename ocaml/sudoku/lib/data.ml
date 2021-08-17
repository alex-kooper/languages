
module Cell = struct
  type t = int * int
  let compare = compare
end

module Grid = struct
  module Map = Stdlib.Map.Make(Cell)

  type t = int Map.t

  let empty = Map.empty
  let set ~row ~col ~digit = Map.add (row, col) digit
  let get ~row ~col = Map.find_opt (row, col)
end
