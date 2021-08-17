module Cell = struct
  type t = int * int

  let compare = compare
end

module Grid = struct
  module Map = Map.Make (Cell)

  type t = int Map.t

  let empty = Map.empty

  let set ~row ~col ~digit = Map.add (row, col) digit

  let get ~row ~col = Map.find_opt (row, col)
end

module GridConstraints = struct
  module Digits = Set.Make (Int)
  module Map = Map.Make (Cell)

  type t = Digits.t Map.t

  let empty = Map.empty

  let set ~row ~col ~digits = Map.add (row, col) digits

  let remove ~row ~col = Map.remove (row, col)

  let remove_digit ~row ~col ~digit =
    Map.update (row, col) (fun digits ->
        Option.map (Digits.remove digit) digits)
end
