module Data =
struct
  module type Compare = sig type t  val compare : t -> t -> int   end
  module type Copy    = sig type t  val dcopy   : t -> t          end
  module type Equal   = sig type t  val equal   : t -> t -> bool  end
  module type Hash    = sig type t  val shash   : int -> t -> int end
  module type PP      = sig type t   end
  module type Show    = sig type t
                            val show : t -> string
                            val pp   : Format.formatter -> t -> unit
                        end
  
  module type S =
  sig type t
      include Compare with type t := t  include Hash with type t := t
      include Copy    with type t := t
      include Equal   with type t := t  include Show with type t := t
  end
end

module Poly1 =
struct
  module type Compare =
  sig type 'a t
      val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  end
  module type Copy =
  sig type 'a t
      val dcopy   : ('a -> 'a) -> 'a t -> 'a t
  end
  module type Equal =
  sig type 'a t
      val equal   : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
  module type Hash =
  sig type 'a t
      val shash   : (int -> 'a -> int) ->  int -> 'a t -> int
  end
  module type Show =
  sig type 'a t
      val show    : ('a -> string) -> 'a t -> string
  end
  
  module type S =
  sig type 'a t
      include Compare with type 'a t := 'a t
      include Copy    with type 'a t := 'a t
      include Equal   with type 'a t := 'a t
      include Hash    with type 'a t := 'a t
      include Show    with type 'a t := 'a t
  end
end


module Poly2 =
struct
  module type Compare =
  sig type ('a, 'b) t
      val compare : ('a -> 'a -> int) ->  ('b -> 'b -> int) ->
                      ('a, 'b) t -> ('a, 'b) t -> int
  end
  module type Copy =
  sig type ('a, 'b) t
      val dcopy   : ('a -> 'a) -> ('b -> 'b) -> ('a, 'b) t -> ('a, 'b) t
  end
  module type Equal =
  sig type ('a, 'b) t
      val equal   : ('a -> 'a -> bool) -> ('b -> 'b -> bool) ->
                      ('a, 'b) t -> ('a, 'b) t -> bool
  end
  module type Hash =
  sig type ('a, 'b) t
      val shash   : (int -> 'a -> int) -> (int -> 'b -> int) ->
                      int -> ('a, 'b) t -> int
  end
  module type Show =
  sig type ('a, 'b) t
      val show    : ('a -> string) -> ('b -> string) -> ('a, 'b) t -> string
  end
  
  module type S =
  sig type ('a, 'b) t
      include Compare with type ('a, 'b) t := ('a, 'b) t
      include Copy    with type ('a, 'b) t := ('a, 'b) t
      include Equal   with type ('a, 'b) t := ('a, 'b) t
      include Hash    with type ('a, 'b) t := ('a, 'b) t
      include Show    with type ('a, 'b) t := ('a, 'b) t
  end
end

module Monoid =
struct
  module type S =
  sig
    type t
    val both : t -> t -> t
    val id   : t
  end
end
