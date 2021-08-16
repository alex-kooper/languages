
module Cell = struct
  type t = int * int
  let compare = compare
  let create row column = (row, column)
end

module Grid = struct
  module Map = Stdlib.Map.Make(Cell)

  type t = int Map.t
  type cell = Cell.t

  let empty = Map.empty
  let set = Map.add
  let get = Map.find_opt
end
