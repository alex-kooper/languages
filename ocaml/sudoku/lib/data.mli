module Grid : sig
  type t

  val empty : t
  val set : row:int -> col:int  -> digit:int -> t -> t
  val get : row:int -> col:int -> t -> int option
end
