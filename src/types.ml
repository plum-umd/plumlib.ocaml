open Sigs

module Imp =
struct

(* Is a Data.S, but also exports Big_int to avoid shadowing. *)
module Big_int (*: Data.S with type t = Big_int.big_int*) =
struct
  type t = Big_int.big_int
  include Big_int
  let equal = Big_int.eq_big_int
  let compare = Big_int.compare_big_int
  let show = Big_int.string_of_big_int
  let pp ff (t : t) = Format.pp_print_string ff (show t)
  let shash s (t : t) = Hashtbl.seeded_hash s t
  let dcopy : t -> t = fun t -> t
end

module Bool: Data.S with type t = bool =
struct
  type t = bool [@@deriving eq, ord, show]
  let shash s (b : t) = Hashtbl.seeded_hash s b
  let dcopy : t -> t = fun x -> x
end

(* Is a Data.S, but also exports Char to avoid shadowing. *)
module Char (*: Data.S with type t = char*) =
struct
  type t = char [@@deriving eq, show]
  include (Char : module type of Char with type t := t)
  let show = escaped
  let pp ff (t : t) = Format.pp_print_string ff (show t)
  let shash s (c : t) = Hashtbl.seeded_hash s c
  let dcopy : t -> t = fun x -> x
end

module Exn =
struct
  type t = exn
  let show : exn -> string = Printexc.to_string
  let pp ff (t : t) = Format.pp_print_string ff (show t)
  let equal (e : exn) (e' : exn) = e = e'
  let compare (e : exn) (e' : exn) = Pervasives.compare e e'
  let shash s (e : exn) = Hashtbl.seeded_hash s e
  let dcopy (t:t):t = match t with
    | Match_failure _ | Assert_failure _ | Invalid_argument _ | Failure _
    | Not_found | Out_of_memory | Stack_overflow | Sys_error _ | End_of_file
    | Division_by_zero | Sys_blocked_io | Undefined_recursive_module _ ->
      t
    | _ -> failwith ("Cannot copy user-defined exn: " ^ (show t))
      (* can't guarantee that users' don't require deep copies *)
end

module Float: Data.S with type t = float =
struct
  type t = float [@@deriving eq, ord]
  let show = string_of_float
  let pp ff (t : t) = Format.pp_print_string ff (show t)
  let shash s (f : t) = Hashtbl.seeded_hash s f
  let dcopy : t -> t = fun x -> x
end

module Int: Data.S with type t = int =
struct
  type t = int [@@deriving eq, ord]
  let show (t : t) : string = string_of_int t
  let pp ff (t : t) = Format.pp_print_string ff (show t)
  let shash s (i : t) = Hashtbl.seeded_hash s i
  let dcopy : t -> t = fun x -> x
end

(* Is a Data.S, but also exports Int32 to avoid shadowing. *)
module Int32 (*: Data.S with type t = int32*) =
struct
  type t = int32 [@@deriving eq, ord]
  include (Int32 : module type of Int32 with type t := t)
  let show = Int32.to_string
  let pp ff (t : t) = Format.pp_print_string ff (show t)
  let shash s (i : int32) = Hashtbl.seeded_hash s i
  let dcopy : t -> t = fun x -> x
end

(* Is a Data.S, but also exports Int64 to avoid shadowing. *)
module Int64 (*: Data.S with type t = int64*) =
struct
  type t = int64 [@@deriving eq, ord]
  include (Int64 : module type of Int64 with type t := t)
  let show = to_string
  let pp ff (t : t) = Format.pp_print_string ff (show t)
  let shash s (i : t) = Hashtbl.seeded_hash s i
  let dcopy : t -> t = fun x -> x
end

(* Is a Data.S, but also exports Nativeint to avoid shadowing. *)
module Nativeint (*: Data.S with type t = nativeint*) =
struct
  type t = nativeint [@@deriving eq, ord]
  include (Nativeint : module type of Nativeint with type t := t)
  let show = to_string
  let pp ff (t : t) = Format.pp_print_string ff (show t)
  let shash s (i : nativeint) = Hashtbl.seeded_hash s i
  let dcopy : t -> t = fun x -> x      
end

(* Is a Data.S, but also exports String to avoid shadowing. *)
module String (* : Data.S with type t = string *) =
struct 
  type t = string [@@deriving eq, ord]
  include (String : module type of String with type t := t)
  let show (x : string) : string = x
  let pp ff (t : t) = Format.pp_print_string ff (show t)
  let shash s (t : string) = Hashtbl.seeded_hash s t
  let dcopy : t -> t = fun x -> x
end

module Unit: Data.S with type t = unit =
struct
  type t = unit
  let show () = "()"
  let pp ff (t : t) = Format.pp_print_string ff (show t)
  let equal () () = true
  let compare () () = 0
  let shash seed () = Hashtbl.seeded_hash seed ()
  let dcopy : t -> t = fun x -> x
end

(* Early term. for equal/compare *)
exception False
exception Cmp of int

module ArrayP : Poly1.S  with type 'a t = 'a array =
struct
  type 'a t = 'a array
  
  let show ashow aa =
    Util.string_of_list ~sep:"; " ~border:(Printf.sprintf "[|%s|]")
      ashow (Array.to_list aa)
    
  let equal aequal aa aa' =
    Array.length aa = Array.length aa' &&
    try
      ignore
        (Array.fold_left
           (fun i a ->
              if aequal a (Array.get aa' i)
              then i+1
              else raise False)
           0
           aa) ;
      true
    with False -> false

  let compare (acompare : 'a -> 'a -> int) (t : 'a t) (t' : 'a t) =
    let tl = Array.length t and t'l = Array.length t' in
    if tl = t'l
    then try let _ =
               Array.fold_left
                 (fun i a ->
                    let c = acompare a (Array.get t' i) in
                    if c = 0 then i+1 else raise (Cmp c))
                 0
                 t
        in
        0
      with Cmp c -> c
    else if tl > t'l then ~-1
    else (* tl < t'l *)     1
      
  let shash (ahash : int -> 'a -> int) (s : int) (t : 'a array) : int =
    Array.fold_left
      ahash
      (Hashtbl.seeded_hash s "Types.ArrayP.shash")
      t
      
  let dcopy acopy x =
      let a = Array.copy x in
      Array.map acopy a
end

module LazyP : Poly1.S  with type 'a t = 'a Lazy.t =
struct
  type 'a t = 'a Lazy.t
  let show (ashow : 'a -> string) (la : 'a Lazy.t) =
    if Lazy.is_val la
    then ashow (Lazy.force_val la)
    else "<not evaluated>"
  let equal aequal la la' =
    la == la' || aequal (Lazy.force_val la) (Lazy.force_val la')
  let compare acompare la la' =
    if la == la' then 0
    else acompare (Lazy.force_val la) (Lazy.force_val la')
  let shash ahash s la =
    ahash
      (Hashtbl.seeded_hash s "Types.LazyP.shash")
      (Lazy.force la)
  let dcopy acopy la = lazy (acopy (Lazy.force_val la))
end

module ListP : Poly1.S  with type 'a t = 'a list =
struct
  type 'a t = 'a list
  
  let (equal, compare) :
    (('a -> 'a -> bool) -> 'a t -> 'a t -> bool) *
    (('a -> 'a -> int)  -> 'a t -> 'a t -> int) =
    let loop2 succ fail l r b f =
      let rec loop = function
        | a::l, a'::l' -> let o = f a a' in
          if   succ o
          then loop (l, l')
          else fail o
        | [], _::_ -> fail l
        | _::_, [] -> fail r
        | _ -> b
      in loop
    in
    (fun aequal t t' ->
       let i x = x in
       try loop2 i (fun _->raise False) false false true aequal (t,t')
       with False -> false),
    (fun acompare t t' ->
       let eq0 i = 0 = i in
       try loop2 eq0 (fun c->raise (Cmp c)) ~-1 1 0 acompare (t,t')
       with Cmp c -> c)
  let shash   (ahash : int -> 'a -> int) (s : int) : 'a t -> int =
    List.fold_left ahash (Hashtbl.seeded_hash s "Types.ListP.shash")
  let show    (ashow : 'a -> string) (t : 'a t) : string =
    Util.string_of_list ashow t
  let dcopy   (acopy : 'a -> 'a) (t : 'a t) : 'a t = List.map acopy t

end

module OptionP : Poly1.S  with type 'a t = 'a option =
struct
  type 'a t = 'a option
  let show (ashow : 'a -> string) : 'a t -> string = function
    | Some a -> Printf.sprintf "(Some %s)" (ashow a)
    | None   -> "None"
  let equal (aequal : 'a -> 'a -> bool) (oa : 'a t) (oa' : 'a t) =
    match oa, oa' with
    | Some a, Some a' -> aequal a a'
    | None,   None    -> true
    | _ -> false
  let compare (acompare : 'a -> 'a -> int) (oa : 'a t) (oa' : 'a t) =
    match oa, oa' with
    | Some a, Some a' -> acompare a a'
    | None,   None    ->   0
    | None,   Some _  -> ~-1
    | Some _, None    ->   1
  let shash (ahash : int -> 'a -> int) seed : 'a t -> int = function
    | Some a -> ahash (Hashtbl.seeded_hash seed "Types.OptionP.shash~Some") a
    | None   -> Hashtbl.seeded_hash seed "Types.OptionP.shash~None"
  let dcopy (acopy : 'a -> 'a) (x : 'a t) : 'a t = match x with
    | None   -> None
    | Some x -> Some (acopy x)
end

module FunP =
struct
  type ('a, 'b) t = 'a -> 'b
  let show  (_ : ('a, 'b) t) : string = "(fun:?)"
  let equal (t : ('a, 'b) t) (t' : ('a, 'b) t) = t == t'
  let shash (s : int) (t : ('a, 'b) t) : int =
    Hashtbl.seeded_hash (Hashtbl.seeded_hash s t) "Types.FunP.shash"
end

module RefP =
struct
  type 'a t = 'a ref

  let show (ashow : 'a -> string) (ar : 'a t) =
    Printf.sprintf "(ref %s)" (ashow !ar)

  let equal (aequal : 'a -> 'a -> bool) (ar : 'a t) (ar' : 'a t) =
    ar == ar' || aequal !ar !ar'

  let shash (ahash : int -> 'a -> int) (s : int) (ar : 'a t) =
    Hashtbl.seeded_hash (ahash s !ar) ar
end

module HMS (K : Data.S) =
  Hashtbl.MakeSeeded(struct include K let hash = shash end)


module HashtblP1 (K : Data.S) : Poly1.S with type 'v t = 'v HMS(K).t =
struct
  module H = HMS(K)
  type 'v t = 'v H.t

  let for_all (p : K.t -> 'v -> bool) (t : 'v t) =
    try
      H.iter
        (fun k v -> if p k v then () else raise False)
        t ;
      true
    with False -> false

  let equal (veq : 'v -> 'v -> bool) (t : 'v t) (t' : 'v t) =
    (H.length t = H.length t') &&
    (for_all
       (fun k v -> match H.find t' k with
          | v' -> veq v v'
          | exception Not_found -> false)
       t)
    
  let show (vshow : 'v -> string) (t : 'v t) =
    Util.string_of_list ~sep:", " ~border:(Printf.sprintf "{%s}")
      (function (k, v) -> K.show k ^ " --> " ^ vshow v)
      (H.fold (fun k v a -> (k, v)::a) t [])

  let compare (vcmp : 'v -> 'v -> int) (t : 'v t) (t' : 'v t) =    
    let tl = H.length t and t'l = H.length t' in
    if tl = t'l
    then try ignore (H.iter
                       (fun k v -> match H.find t' k with
                          | v' -> let c = vcmp v v' in
                                  if c <> 0 then raise (Cmp c)
                          | exception Not_found -> raise (Cmp ~-1))
                       t) ;
             0
         with Cmp c -> c
    else if tl > t'l then ~-1
    else (* tl < t'l *)     1

  let dcopy (vcopy : 'v -> 'v) (t : 'v t) =
    let t' = H.create 100 in
    H.iter (fun k v -> H.add t' (K.dcopy k) (vcopy v)) t ;
    t'

  let shash (vhash : int -> 'v -> int) s (t : 'v t) =
    H.fold
      (fun k v s -> K.shash (vhash s v) k)
      t
      (Hashtbl.seeded_hash s "Types.HashtblP1.shash")
   
end

module HashtblD (K : Data.S) (V : Data.S) : Data.S with type t = V.t HMS(K).t =
struct
  module H = HMS(K)
  type t = V.t H.t

  let for_all (p : K.t -> V.t -> bool) (t : t) =
    try
      H.iter
        (fun k v -> if p k v then () else raise False)
        t ;
      true
    with False -> false

  let equal (t : t) (t' : t) =
    (H.length t = H.length t') &&
    (for_all
       (fun k v -> match H.find t' k with
          | v' -> V.equal v v'
          | exception Not_found -> false)
       t)
    
  let show (t : t) =
    Util.string_of_list ~sep:", " ~border:(Printf.sprintf "{%s}")
      (function (k, v) -> K.show k ^ " --> " ^ V.show v)
      (H.fold (fun k v a -> (k, v)::a) t [])
  let pp ff (t : t) = Format.pp_print_string ff (show t)

  let compare (t : t) (t' : t) =    
    let tl = H.length t and t'l = H.length t' in
    if tl = t'l
    then try ignore (H.iter
                       (fun k v -> match H.find t' k with
                          | v' -> let c = V.compare v v' in
                                  if c <> 0 then raise (Cmp c)
                          | exception Not_found -> raise (Cmp ~-1))
                       t) ;
             0
         with Cmp c -> c
    else if tl > t'l then ~-1
    else (* tl < t'l *)     1

  let dcopy (t : t) =
    let t' = H.create 100 in
    H.iter (fun k v -> H.add t' (K.dcopy k) (V.dcopy v)) t ;
    t'

  let shash s (t : t) =
    H.fold
      (fun k v s -> K.shash (V.shash s v) k)
      t
      (Hashtbl.seeded_hash s "Types.HashtblD.shash")
   
end

module HashtblP2 : Poly2.S with type ('k, 'v) t = ('k, 'v) Hashtbl.t =
struct
  type ('k, 'v) t = ('k, 'v) Hashtbl.t
  let for_all (p : 'k -> 'v -> bool) (t : ('k, 'v) Hashtbl.t) =
    try
      Hashtbl.iter
        (fun k v -> if p k v then () else raise False)
        t ;
      true
    with False -> false

  let equal (keq : 'k -> 'k -> bool) (veq : 'v -> 'v -> bool)
      (t : ('k, 'v) Hashtbl.t) (t' : ('k, 'v) Hashtbl.t) =
    (Hashtbl.length t = Hashtbl.length t') &&
    (for_all
       (fun k v -> match Hashtbl.find t' k with
          | v' -> veq v v'
          | exception Not_found -> false)
       t)
    
  let show (kshow : 'k -> string) (vshow : 'v -> string) =
    fun (t : ('k, 'v) Hashtbl.t) ->
      Util.string_of_list ~sep:", " ~border:(Printf.sprintf "{%s}")
        (function (k, v) -> kshow k ^ " --> " ^ vshow v)
        (Hashtbl.fold (fun k v a -> (k, v)::a) t [])

  let compare (kcmp : 'k -> 'k -> int) (vcmp : 'v -> 'v -> int)
      (t : ('k, 'v) Hashtbl.t) (t' : ('k, 'v) Hashtbl.t) =    
    let tl = Hashtbl.length t and t'l = Hashtbl.length t' in
    if tl = t'l
    then try ignore (Hashtbl.iter
                       (fun k v -> match Hashtbl.find t' k with
                          | v' -> let c = vcmp v v' in
                                  if c <> 0 then raise (Cmp c)
                          | exception Not_found -> raise (Cmp ~-1))
                       t) ;
             0
         with Cmp c -> c
    else if tl > t'l then ~-1
    else (* tl < t'l *)     1

  let dcopy (kcopy : 'k -> 'k) (vcopy : 'v -> 'v)
      (t : ('k, 'v) Hashtbl.t) =
    let t' = Hashtbl.create 100 in
    Hashtbl.iter (fun k v -> Hashtbl.add t' (kcopy k) (vcopy v)) t ;
    t'

  let shash
      (khash : int -> 'k -> int)
      (vhash : int -> 'v -> int)
      s (t : ('k, 'v) Hashtbl.t) =
    Hashtbl.fold
      (fun k v s -> khash (vhash s v) k)
      t
      (Hashtbl.seeded_hash s "Types.HashtblP2.shash")
   
end

module Tuple2P : Poly2.S with type ('a, 'b) t = 'a * 'b =
struct
  type ('a, 'b) t = 'a * 'b [@@deriving eq, ord]
  let show : ('a -> string) -> ('b -> string) -> ('a * 'b) -> string =
    fun ashow bshow (a, b) -> Printf.sprintf "(%s, %s)" (ashow a) (bshow b)

  let shash : (int -> 'a -> int) -> (int -> 'b -> int) ->
    int -> ('a * 'b) -> int =
    fun ahash bhash seed (a, b) -> bhash (ahash seed a) b

  let dcopy : ('a -> 'a) -> ('b -> 'b) -> ('a * 'b) -> ('a * 'b) =
    fun acopy bcopy (a, b) -> (acopy a, bcopy b)
end

module Tuple3P =
struct
  type ('a, 'b, 'c) t = 'a * 'b * 'c
    [@@deriving eq, ord]

  let show : ('a -> string) -> ('b -> string) ->
             ('c -> string) ->
    ('a * 'b * 'c) ->
    string =
    fun ashow bshow cshow
      (a, b, c) ->
      Printf.sprintf "(%s, %s, %s)"
        (ashow a) (bshow b) (cshow c)

  let shash : (int -> 'a -> int) -> (int -> 'b -> int) ->
              (int -> 'c -> int) ->
    int -> ('a * 'b * 'c) -> int =
    fun ahash bhash chash
      seed (a, b, c) -> chash (bhash (ahash seed a) b) c

  let dcopy : ('a -> 'a) -> ('b -> 'b) -> ('c -> 'c) ->
    ('a * 'b * 'c) -> ('a * 'b * 'c) =
    fun acopy bcopy ccopy
      (a, b, c) ->
      (acopy a, bcopy b, ccopy c)
end

module Tuple4P =
struct
  type ('a, 'b, 'c, 'd) t = 'a * 'b * 'c * 'd
    [@@deriving eq, ord]

  let show : ('a -> string) -> ('b -> string) ->
             ('c -> string) -> ('d -> string) ->
    ('a * 'b * 'c * 'd) ->
    string =
    fun ashow bshow cshow dshow
      (a, b, c, d) ->
      Printf.sprintf "(%s, %s, %s, %s)"
        (ashow a) (bshow b) (cshow c) (dshow d)

  let shash : (int -> 'a -> int) -> (int -> 'b -> int) ->
              (int -> 'c -> int) -> (int -> 'd -> int) ->
    int -> ('a * 'b * 'c * 'd) -> int =
    fun ahash bhash chash dhash
      seed (a, b, c, d) ->
      dhash (chash (bhash (ahash seed a) b) c) d

  let dcopy : ('a -> 'a) -> ('b -> 'b) -> ('c -> 'c) ->
              ('d -> 'd) ->
    ('a * 'b * 'c * 'd) ->
    ('a * 'b * 'c * 'd) =
    fun acopy bcopy ccopy dcopy
      (a, b, c, d) ->
      (acopy a, bcopy b, ccopy c, dcopy d)
end

module Tuple5P =
struct
  type ('a, 'b, 'c, 'd, 'e) t = 'a * 'b * 'c * 'd * 'e
    [@@deriving eq, ord]

  let show : ('a -> string) -> ('b -> string) ->
             ('c -> string) -> ('d -> string) ->
             ('e -> string) ->
    ('a * 'b * 'c * 'd * 'e) ->
    string =
    fun ashow bshow cshow dshow eshow
      (a, b, c, d, e) ->
      Printf.sprintf "(%s, %s, %s, %s, %s)"
        (ashow a) (bshow b) (cshow c) (dshow d) (eshow e)

  let shash : (int -> 'a -> int) -> (int -> 'b -> int) ->
              (int -> 'c -> int) -> (int -> 'd -> int) ->
              (int -> 'e -> int) ->
    int -> ('a * 'b * 'c * 'd * 'e) -> int =
    fun ahash bhash chash dhash ehash
      seed (a, b, c, d, e) ->
      ehash (dhash (chash (bhash (ahash seed a) b) c) d) e

  let dcopy : ('a -> 'a) -> ('b -> 'b) -> ('c -> 'c) ->
              ('d -> 'd) -> ('e -> 'e) ->
    ('a * 'b * 'c * 'd * 'e) ->
    ('a * 'b * 'c * 'd * 'e) =
    fun acopy bcopy ccopy dcopy ecopy
      (a, b, c, d, e) ->
      (acopy a, bcopy b, ccopy c, dcopy d, ecopy e)
end

module Tuple6P =
struct
  type ('a, 'b, 'c, 'd, 'e, 'f) t =
    'a * 'b * 'c * 'd * 'e * 'f
    [@@deriving eq, ord]

  let show : ('a -> string) -> ('b -> string) ->
             ('c -> string) -> ('d -> string) ->
             ('e -> string) -> ('f -> string) ->
    ('a * 'b * 'c * 'd * 'e * 'f) ->
    string =
    fun ashow bshow cshow dshow eshow fshow
      (a, b, c, d, e, f) ->
      Printf.sprintf "(%s, %s, %s, %s, %s, %s)"
        (ashow a) (bshow b) (cshow c) (dshow d) (eshow e)
        (fshow f)

  let shash : (int -> 'a -> int) -> (int -> 'b -> int) ->
              (int -> 'c -> int) -> (int -> 'd -> int) ->
              (int -> 'e -> int) -> (int -> 'f -> int) ->
    int -> ('a * 'b * 'c * 'd * 'e * 'f) -> int =
    fun ahash bhash chash dhash ehash fhash
      seed (a, b, c, d, e, f) ->
      fhash (ehash (dhash (chash (bhash (ahash seed a) b) c) d) e) f

  let dcopy : ('a -> 'a) -> ('b -> 'b) -> ('c -> 'c) ->
              ('d -> 'd) -> ('e -> 'e) -> ('f -> 'f) ->
    ('a * 'b * 'c * 'd * 'e * 'f) ->
    ('a * 'b * 'c * 'd * 'e * 'f) =
    fun acopy bcopy ccopy dcopy ecopy fcopy
      (a, b, c, d, e, f) ->
      (acopy a, bcopy b, ccopy c, dcopy d, ecopy e, fcopy f)
end

module Tuple7P =
struct
  type ('a, 'b, 'c, 'd, 'e, 'f, 'g) t =
    'a * 'b * 'c * 'd * 'e * 'f * 'g
    [@@deriving eq, ord]

  let show : ('a -> string) -> ('b -> string) ->
             ('c -> string) -> ('d -> string) ->
             ('e -> string) -> ('f -> string) ->
             ('g -> string) ->
    ('a * 'b * 'c * 'd * 'e * 'f * 'g) ->
    string =
    fun ashow bshow cshow dshow eshow fshow gshow
      (a, b, c, d, e, f, g) ->
      Printf.sprintf "(%s, %s, %s, %s, %s, %s, %s)"
        (ashow a) (bshow b) (cshow c) (dshow d) (eshow e)
        (fshow f) (gshow g)

  let shash : (int -> 'a -> int) -> (int -> 'b -> int) ->
              (int -> 'c -> int) -> (int -> 'd -> int) ->
              (int -> 'e -> int) -> (int -> 'f -> int) ->
              (int -> 'g -> int) ->
    int -> ('a * 'b * 'c * 'd * 'e * 'f * 'g) -> int =
    fun ahash bhash chash dhash ehash fhash ghash
      seed (a, b, c, d, e, f, g) ->
      ghash (fhash (ehash (dhash (chash (bhash (ahash seed a) b) c) d) e) f) g

  let dcopy : ('a -> 'a) -> ('b -> 'b) -> ('c -> 'c) ->
              ('d -> 'd) -> ('e -> 'e) -> ('f -> 'f) ->
              ('g -> 'g) ->
    ('a * 'b * 'c * 'd * 'e * 'f * 'g) ->
    ('a * 'b * 'c * 'd * 'e * 'f * 'g) =
    fun acopy bcopy ccopy dcopy ecopy fcopy gcopy
      (a, b, c, d, e, f, g) ->
      (acopy a, bcopy b, ccopy c, dcopy d, ecopy e, fcopy f, gcopy g)
end

module Tuple8P =
struct
  type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) t =
    'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h
    [@@deriving eq, ord]

  let show : ('a -> string) -> ('b -> string) ->
             ('c -> string) -> ('d -> string) ->
             ('e -> string) -> ('f -> string) ->
             ('g -> string) -> ('h -> string) ->
    ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) ->
    string =
    fun ashow bshow cshow dshow eshow fshow gshow hshow
      (a, b, c, d, e, f, g, h) ->
      Printf.sprintf "(%s, %s, %s, %s, %s, %s, %s, %s)"
        (ashow a) (bshow b) (cshow c) (dshow d) (eshow e)
        (fshow f) (gshow g) (hshow h)

  let shash : (int -> 'a -> int) -> (int -> 'b -> int) ->
              (int -> 'c -> int) -> (int -> 'd -> int) ->
              (int -> 'e -> int) -> (int -> 'f -> int) ->
              (int -> 'g -> int) -> (int -> 'h -> int) -> 
    int -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) -> int =
    fun ahash bhash chash dhash ehash fhash ghash hhash
      seed (a, b, c, d, e, f, g, h) ->
      hhash
        (ghash
           (fhash
              (ehash
                 (dhash
                    (chash
                       (bhash
                          (ahash seed a) b) c) d) e) f) g) h

  let dcopy : ('a -> 'a) -> ('b -> 'b) -> ('c -> 'c) ->
              ('d -> 'd) -> ('e -> 'e) -> ('f -> 'f) ->
              ('g -> 'g) -> ('h -> 'h) ->
    ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) ->
    ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) =
    fun acopy bcopy ccopy dcopy ecopy fcopy gcopy hcopy
      (a, b, c, d, e, f, g, h) ->
      (acopy a, bcopy b, ccopy c, dcopy d, ecopy e, fcopy f, gcopy g, hcopy h)
end

module Tuple9P =
struct
  type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i) t =
    'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i
    [@@deriving eq, ord]

  let show : ('a -> string) -> ('b -> string) ->
             ('c -> string) -> ('d -> string) ->
             ('e -> string) -> ('f -> string) ->
             ('g -> string) -> ('h -> string) ->
             ('i -> string) ->
    ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) ->
    string =
    fun ashow bshow cshow dshow eshow fshow gshow hshow ishow
      (a, b, c, d, e, f, g, h, i) ->
      Printf.sprintf "(%s, %s, %s, %s, %s, %s, %s, %s, %s)"
        (ashow a) (bshow b) (cshow c) (dshow d) (eshow e)
        (fshow f) (gshow g) (hshow h) (ishow i)

  let shash : (int -> 'a -> int) -> (int -> 'b -> int) ->
              (int -> 'c -> int) -> (int -> 'd -> int) ->
              (int -> 'e -> int) -> (int -> 'f -> int) ->
              (int -> 'g -> int) -> (int -> 'h -> int) -> 
              (int -> 'i -> int) -> 
    int -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) -> int =
    fun ahash bhash chash dhash ehash fhash ghash hhash ihash
      seed (a, b, c, d, e, f, g, h, i) ->
      ihash
        (hhash
         (ghash
          (fhash
           (ehash
            (dhash
             (chash
              (bhash
               (ahash seed a) b) c) d) e) f) g) h) i

  let dcopy : ('a -> 'a) -> ('b -> 'b) -> ('c -> 'c) ->
              ('d -> 'd) -> ('e -> 'e) -> ('f -> 'f) ->
              ('g -> 'g) -> ('h -> 'h) -> ('i -> 'i) ->
    ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) ->
    ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) =
    fun acopy bcopy ccopy dcopy ecopy fcopy gcopy hcopy icopy
      (a, b, c, d, e, f, g, h, i) ->
      (acopy a, bcopy b, ccopy c, dcopy d, ecopy e,
       fcopy f, gcopy g, hcopy h, icopy i)
end



module ArrayF (A : Data.S) : Data.S with type t = A.t array =
struct
  type t = A.t array
  
  let show aa =
    Util.string_of_list ~sep:"; " ~border:(Printf.sprintf "[|%s|]")
      A.show (Array.to_list aa)

  let pp ff t = Format.pp_print_string ff (show t)
    
  let equal aa aa' =
    Array.length aa = Array.length aa' &&
    try
      ignore
        (Array.fold_left
           (fun i a ->
              if A.equal a (Array.get aa' i)
              then i+1
              else raise False)
           0
           aa) ;
      true
    with False -> false

  let compare t t' =
    let tl = Array.length t and t'l = Array.length t' in
    if tl = t'l
    then try let _ =
               Array.fold_left
                 (fun i a ->
                    let c = A.compare a (Array.get t' i) in
                    if c = 0 then i+1 else raise (Cmp c))
                 0
                 t
        in
        0
      with Cmp c -> c
    else if tl > t'l then ~-1
    else (* tl < t'l *)     1
      
  let shash (s : int) (t : 'a array) : int =
    Array.fold_left
      A.shash
      (Hashtbl.seeded_hash s "FTypes.Array.shash")
      t
      
  let dcopy x =
      let a = Array.copy x in
      Array.map A.dcopy a
end

module LazyF (A : Data.S) : Data.S with type t = A.t Lazy.t =
struct
  type t = A.t Lazy.t
  let show (la : 'a Lazy.t) =
    if Lazy.is_val la
    then A.show (Lazy.force_val la)
    else "<not evaluated>"
  let pp ff t = Format.pp_print_string ff (show t)
  let equal la la' =
    la == la' || A.equal (Lazy.force_val la) (Lazy.force_val la')
  let compare la la' =
    if la == la' then 0
    else A.compare (Lazy.force_val la) (Lazy.force_val la')
  let shash s la =
    A.shash
      (Hashtbl.seeded_hash s "FTypes.Lazy.shash")
      (Lazy.force la)
  let dcopy la = lazy (A.dcopy (Lazy.force_val la))
end

module OptionF (A : Data.S) : Data.S with type t = A.t option =
struct
  type t = A.t option [@@deriving eq, ord, show]
  let shash seed = function
    | Some a -> A.shash (Hashtbl.seeded_hash seed "Some") a
    | None   -> Hashtbl.seeded_hash seed "None"
  let dcopy x = match x with
    | None   -> None
    | Some x -> Some (A.dcopy x)
end

module FunF (A : Data.S) (B : Data.S) =
struct
  type t = A.t -> B.t
  let show  (_ : t) : string = "(fun:?)"
  let pp ff t = Format.pp_print_string ff (show t)
  let equal (t : t) (t' : t) = t == t'
  let shash s (t : t) = Hashtbl.seeded_hash s t
end

module RefF (A : Data.S) : Data.S with type t = A.t ref =
struct
  type t = A.t ref
  let show ar = Printf.sprintf "(ref %s)" (A.show !ar)
  let pp ff t = Format.pp_print_string ff (show t)
  let equal (ar : 'a ref) (ar' : 'a ref) = ar == ar'
  let compare ar ar' = if equal ar ar' then 0 else A.compare !ar !ar'
  let shash s ar = Hashtbl.seeded_hash (A.shash s !ar) "Ref.shash"
  let dcopy ar = ref (A.dcopy !ar)
end

module HashtblF (K : Data.S) (V : Data.S) :
  Data.S with type t = (K.t, V.t) Hashtbl.t =
struct
  
  
  type t = (K.t, V.t) Hashtbl.t
  let for_all (p : K.t -> V.t -> bool) (t : t) =
    try
      Hashtbl.iter
        (fun k v -> if p k v then () else raise False)
        t ;
      true
    with False -> false

  let equal t t' =
    (Hashtbl.length t = Hashtbl.length t') &&
    (for_all
       (fun k v -> match Hashtbl.find t' k with
          | v' -> V.equal v v'
          | exception Not_found -> false)
       t)
    
  let show (t : t) =
      Util.string_of_list ~sep:", " ~border:(Printf.sprintf "{%s}")
        (function (k, v) -> K.show k ^ " --> " ^ V.show v)
        (Hashtbl.fold (fun k v a -> (k, v)::a) t [])
  let pp ff t = Format.pp_print_string ff (show t)

  let compare t t' =    
    let tl = Hashtbl.length t and t'l = Hashtbl.length t' in
    if tl = t'l
    then try ignore (Hashtbl.iter
                       (fun k v -> match Hashtbl.find t' k with
                          | v' -> let c = V.compare v v' in
                                  if c <> 0 then raise (Cmp c)
                          | exception Not_found -> raise (Cmp ~-1))
                       t) ;
             0
         with Cmp c -> c
    else if tl > t'l then ~-1
    else (* tl < t'l *)     1

  let dcopy (t : (K.t, V.t) Hashtbl.t) =
    let t' = Hashtbl.create 100 in
    Hashtbl.iter (fun k v -> Hashtbl.add t' (K.dcopy k) (V.dcopy v)) t ;
    t'

  let shash s t =
    Hashtbl.fold
      (fun k v s -> K.shash (V.shash s v) k)
      t
      (Hashtbl.seeded_hash s "FTypes.Hashtbl.shash")
   
end

module Tuple2(A : Data.S)(B : Data.S) = struct
    type t = A.t * B.t [@@deriving eq, ord, show]
    let shash seed ( a, b ) = B.shash (A.shash seed a) b
    let copy (a, b) = (A.dcopy a, B.dcopy b)
end

module Tuple3 (A : Data.S) (B : Data.S) (C : Data.S) = struct
    type t = A.t * B.t * C.t [@@deriving eq, ord, show]
    let shash seed ( a, b, c ) = C.shash (B.shash (A.shash seed a) b) c
    let copy (a, b, c) = (A.dcopy a, B.dcopy b, C.dcopy c)
end

module Tuple4 (A : Data.S) (B : Data.S) (C : Data.S) (D : Data.S) = struct
    type t = A.t * B.t * C.t * D.t [@@deriving eq, ord, show]
    let shash seed ( a, b, c, d ) = D.shash (C.shash (B.shash (A.shash seed a) b) c) d
    let copy (a, b, c, d) = (A.dcopy a, B.dcopy b, C.dcopy c, D.dcopy d)
end

module Tuple5 (A : Data.S) (B : Data.S) (C : Data.S) (D : Data.S) (E: Data.S) = struct
    type t = A.t * B.t * C.t * D.t * E.t [@@deriving eq, ord, show]
    let shash seed ( a, b, c, d, e ) = E.shash (D.shash (C.shash (B.shash (A.shash seed a) b) c) d) e
    let copy (a, b, c, d, e) = (A.dcopy a, B.dcopy b, C.dcopy c, D.dcopy d, E.dcopy e)
end

module Tuple6 (A : Data.S) (B : Data.S) (C : Data.S) (D : Data.S) (E: Data.S) (F : Data.S) = struct
    type t = A.t * B.t * C.t * D.t * E.t * F.t [@@deriving eq, ord, show]
    let shash seed ( a, b, c, d, e, f ) = F.shash (E.shash (D.shash (C.shash (B.shash (A.shash seed a) b) c) d) e) f
    let copy (a, b, c, d, e, f) = (A.dcopy a, B.dcopy b, C.dcopy c, D.dcopy d, E.dcopy e, F.dcopy f)
end

module Tuple7 (A : Data.S) (B : Data.S) (C : Data.S) (D : Data.S) (E: Data.S) (F : Data.S) (G: Data.S) = struct
    type t = A.t * B.t * C.t * D.t * E.t * F.t * G.t [@@deriving eq, ord, show]
    let shash seed ( a, b, c, d, e, f, g ) = G.shash (F.shash (E.shash (D.shash (C.shash (B.shash (A.shash seed a) b) c) d) e) f) g
    let copy (a, b, c, d, e, f, g) = (A.dcopy a, B.dcopy b, C.dcopy c, D.dcopy d, E.dcopy e, F.dcopy f, G.dcopy g)
end

module Tuple8 (A : Data.S) (B : Data.S) (C : Data.S) (D : Data.S) (E: Data.S) (F : Data.S) (G: Data.S) (H: Data.S) =
struct
  type t = A.t * B.t * C.t * D.t * E.t * F.t * G.t * H.t
    [@@deriving eq, ord, show]
  let shash seed ( a, b, c, d, e, f, g, h ) =
    H.shash (G.shash (F.shash (E.shash (D.shash (C.shash (B.shash (A.shash seed a) b) c) d) e) f) g) h
  let copy (a, b, c, d, e, f, g, h) =
    (A.dcopy a, B.dcopy b, C.dcopy c, D.dcopy d, E.dcopy e, F.dcopy f, G.dcopy g, H.dcopy h)
end

end

include Imp

module Compare =
struct
  include Implicit.Make(struct type 'a t = 'a -> 'a -> int end)
  [%%imp_spec aggressive(name "compare" related)
            , aggressive(name "compare" Imp)
            , name "compare" related]
  let compare (type a) ?_cmp = unpack_opt _cmp
end

module Copy =
struct
  include Implicit.Make(struct type 'a t = 'a -> 'a end)
  [%%imp_spec aggressive(name "dcopy" related)
            , aggressive(name "dcopy" Imp)
            , name "dcopy" related]
  let dcopy (type a) ?_dcp = unpack_opt _dcp
end

module Equal =
struct
  include Implicit.Make(struct type 'a t = 'a -> 'a -> bool end)
  [%%imp_spec aggressive(name "equal" related)
            , aggressive(name "equal" Imp)
            , name "equal" related]
  let equal (type a) ?_eq = unpack_opt _eq
end

module Hash =
struct
  include Implicit.Make(struct type 'a t = int -> 'a -> int end)
  [%%imp_spec aggressive(name "shash" related)
            , aggressive(name "shash" Imp)
            , name "shash" related]
  let shash (type a) ?_hash = unpack_opt _hash
end

module Show =
struct
  include Implicit.Make(struct type 'a t = 'a -> string end)
  [%%imp_spec aggressive(name "show" related)
            , aggressive(name "show" Imp)
            , name "show" related]
  let show (type a) ?_show = unpack_opt _show
end

let compare = Compare.compare
let equal   =   Equal.equal
let shash   =    Hash.shash
let show    =    Show.show
let dcopy   =    Copy.dcopy

let print ?_imp:(_ : 'a Show.t option) x = Printf.printf "%s\n%!" (show x)
