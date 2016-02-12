module Data =
struct
  module type Compare = sig type t  val compare : t -> t -> int   end
  module type Copy    = sig type t  val dcopy   : t -> t          end
  module type Equal   = sig type t  val equal   : t -> t -> bool  end
  module type Hash    = sig type t  val shash   : int -> t -> int end
  module type Show    = sig type t  val show    : t -> string     end
  
  module type S =
  sig type t
      include Compare with type t := t  include Hash with type t := t
      include Copy    with type t := t  include Show with type t := t
      include Equal   with type t := t
  end
end
