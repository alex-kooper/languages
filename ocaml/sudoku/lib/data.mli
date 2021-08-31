module Grid : sig
  type 'a t

  val empty : 'a t

  val get : row:int -> col:int -> 'a t -> 'a option

  val set : row:int -> col:int -> value:'a -> 'a t -> 'a t

  val remove : row:int -> col:int -> 'a t -> 'a t
end
