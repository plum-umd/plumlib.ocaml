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
end

open Types

module Compare =
struct
  include Make(struct type 'a t = 'a -> 'a -> int end)
  [%%imp_spec aggressive(name "compare" related)
            , aggressive(name "compare" Types)]
  let compare (type a) ?_imp = unpack_opt _imp
end

module Copy =
struct
  include Make(struct type 'a t = 'a -> 'a end)
  [%%imp_spec aggressive(name "dcopy" related)
            , aggressive(name "dcopy" Types)]
  let dcopy (type a) ?_imp = unpack_opt _imp
end

module Equal =
struct
  include Make(struct type 'a t = 'a -> 'a -> bool end)
  [%%imp_spec aggressive(name "equal" related)
            , aggressive(name "equal" Types)]
  let equal (type a) ?_imp = unpack_opt _imp
end

module Hash =
struct
  include Make(struct type 'a t = int -> 'a -> int end)
  [%%imp_spec aggressive(name "shash" related)
            , aggressive(name "shash" Types)]
  let shash (type a) ?_imp = unpack_opt _imp
end

module Show =
struct
  include Make(struct type 'a t = 'a -> string end)
  [%%imp_spec aggressive(name "show" related)
            , aggressive(name "show" Types)]
  let show (type a) ?_imp = unpack_opt _imp
end

let compare = Compare.compare
let equal   =   Equal.equal
let shash   =    Hash.shash
let show    =    Show.show
let dcopy   =    Copy.dcopy
