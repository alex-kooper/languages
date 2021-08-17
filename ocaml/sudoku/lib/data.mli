module Grid : sig
  type t

  val empty : t
  val set : row:int -> col:int -> digit:int -> t -> t
  val get : row:int -> col:int -> t -> int option
end

module GridConstraints : sig 
  type t

  module Digits : Set.S with type elt = int

  val empty : t
  val set : row:int -> col:int -> digits:Digits.t -> t -> t
  val remove : row:int -> col:int -> t -> t
  val remove_digit : row:int -> col:int -> digit:int -> t -> t
end
