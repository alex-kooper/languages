module Grid : sig
  type t

  val empty : t

  val get : row:int -> col:int -> t -> int option

  val set : row:int -> col:int -> digit:int -> t -> t
end

module Grid_constraints : sig
  type t

  module Digits : Set.S with type elt = int

  val empty : t

  val get : row:int -> col:int -> t -> Digits.t option

  val set : row:int -> col:int -> digits:Digits.t -> t -> t

  val remove : row:int -> col:int -> t -> t

  val remove_digit : row:int -> col:int -> digit:int -> t -> t
end
