module Cell : sig
  type t
  val create : int -> int -> t
end

module Grid : sig
  type t
  type cell = Cell.t

  val empty : t
  val set : cell -> int -> t -> t
  val get : cell -> t -> int option
end
