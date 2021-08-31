module Cell = struct
  type t = int * int

  let compare = compare
end

module Grid = struct
  module Map = Map.Make (Cell)

  type 'a t = 'a Map.t

  let get ~row ~col = Map.find_opt (row, col)

  let empty = Map.empty

  let set ~row ~col ~value = Map.add (row, col) value

  let remove ~row ~col = Map.remove (row, col)
end

