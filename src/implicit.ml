module MakeHeavy(A : sig type 'a t end) :
sig
  type 'a t = Packed of 'a A.t
  module Instances : sig
    val pack     : _x:'a A.t -> 'a t
    val pack_opt : _x:'a A.t -> 'a t option
  end
  val unpack_opt : 'a t option -> 'a A.t
end = struct
  [@@@warning "-16"]
  type 'a t = Packed of 'a A.t
  module Instances = struct
    let pack     ~_x:(_x:'a A.t) = (Packed _x)
    let pack_opt ~_x:(_x:'a A.t) = Some (Packed _x)
  end
  let unpack_opt = function
    | None -> assert false
    | Some (Packed x) -> x
end

(* Avoids both the Packed boxing and the first class module of
 * typeclasses. *)

module Make(A : sig type 'a t end) :
sig
  type 'a t = private 'a A.t
  module Instances : sig
    val pack     : _x:'a A.t -> 'a t
    val pack_opt : _x:'a A.t -> 'a t option
  end
  val unpack_opt : 'a t option -> 'a A.t
  val i_inj : 'a A.t -> 'a t
  val i_prj : 'a t -> 'a A.t
end = struct
  [@@@warning "-16"]
  type 'a t = 'a A.t
  module Instances = struct
    let pack     ~_x:(_x:'a A.t) = _x
    let pack_opt ~_x:(_x:'a A.t) = Some _x
  end
  let unpack_opt = function
    | None -> assert false
    | Some x -> (x : 'a A.t)
  let i_inj (a_t : 'a A.t) = (a_t :> 'a t)
  let i_prj (a_t : 'a   t) = (a_t :> 'a A.t)
end



(* Monad stuff *)

module Id =
struct
  include Make (struct type 'a t = 'a end)
  [%%imp_spec aggressive(name "id" related)
            , aggressive(name "id" Monad)
            , name "id" related
            , name "id" Monad]
  let id (type a) ?_id = unpack_opt _id
end
