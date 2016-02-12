open Sigs

module BigInt : Data.S with type t = Big_int.big_int =
struct
  type t = Big_int.big_int
  let equal = Big_int.eq_big_int
  let compare = Big_int.compare_big_int
  let show = Big_int.string_of_big_int
  let pp ff t = Format.pp_print_string ff (show t)
  let shash s t = Hashtbl.seeded_hash s t
  let dcopy : t -> t = fun t -> t
end

module Bool : Data.S with type t = bool =
struct
  type t = bool [@@deriving eq, ord, show]
  let show = string_of_bool
  let shash s (b : t) = Hashtbl.seeded_hash s b
  let dcopy : t -> t = fun x -> x
end

module Char =
struct
  type t = char [@@deriving eq, show]
  include (Char : module type of Char with type t := t)
  let show = escaped
  let shash s (c : t) = Hashtbl.seeded_hash s c
  let dcopy : t -> t = fun x -> x
end

module Exn : Data.S with type t = exn =
struct
  type t = exn
  let show : exn -> string = Printexc.to_string
  let pp ff t = Format.pp_print_string ff (show t)
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

module Float : Data.S with type t = float =
struct
  type t = float [@@deriving eq, ord]
  let show = string_of_float
  let pp ff t = Format.pp_print_string ff (show t)
  let shash s (f : t) = Hashtbl.seeded_hash s f
  let dcopy : t -> t = fun x -> x
end

module Int : Data.S with type t = int =
struct
  type t = int [@@deriving eq, ord]
  let show = string_of_int
  let pp ff t = Format.pp_print_string ff (show t)
  let shash s (i : t) = Hashtbl.seeded_hash s i
  let dcopy : t -> t = fun x -> x
end

module Int32 =
struct
  type t = int32 [@@deriving eq, ord]
  include (Int32 : module type of Int32 with type t := t)
  let show = Int32.to_string
  let pp ff t = Format.pp_print_string ff (show t)
  let shash s (i : int32) = Hashtbl.seeded_hash s i
  let dcopy : t -> t = fun x -> x
end

module Int64 =
struct
  type t = int64 [@@deriving eq, ord]
  include (Int64 : module type of Int64 with type t := t)
  let show = to_string
  let pp ff t = Format.pp_print_string ff (show t)
  let shash s (i : t) = Hashtbl.seeded_hash s i
  let dcopy : t -> t = fun x -> x
end

module Nativeint =
struct
  type t = nativeint [@@deriving eq, ord]
  include (Nativeint : module type of Nativeint with type t := t)
  let show = to_string
  let pp ff t = Format.pp_print_string ff (show t)
  let shash s (i : nativeint) = Hashtbl.seeded_hash s i
  let dcopy : t -> t = fun x -> x      
end

module String =
struct
  type t = string [@@deriving eq, ord]
  include (String : module type of String with type t := t)
  let show (x : string) : string = x
  let pp ff t = Format.pp_print_string ff (show t)
  let shash s (t : string) = Hashtbl.seeded_hash s t
  let dcopy : t -> t = fun x -> x
end

module Unit : Data.S with type t = unit =
struct
  type t = unit
  let show () = "()"
  let pp ff t = Format.pp_print_string ff (show t)
  let equal () () = true
  let compare () () = 0
  let shash seed () = Hashtbl.seeded_hash seed ()
  let dcopy : t -> t = fun x -> x
end

(* Early term. for equal/compare *)
exception False
exception Cmp of int

module Array =
struct include Array
  
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

  let compare acompare t t' =
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
      (Hashtbl.seeded_hash s "Types.Array.shash")
      t
      
  let dcopy acopy x =
      let a = Array.copy x in
      Array.map acopy a
end

module Lazy =
struct include Lazy
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
      (Hashtbl.seeded_hash s "Types.Lazy.shash")
      (Lazy.force la)
  let dcopy acopy la = lazy (acopy (Lazy.force_val la))
end

module List =
struct include List
  let equal, compare =
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
  let shash    ahash s    = List.fold_left ahash (Hashtbl.seeded_hash s "Types.List.shash")
  let show    ashow t    = Util.string_of_list ashow t
  let dcopy    acopy t = List.map acopy t

end

module Option =
struct
  type 'a t = 'a option
  let show ashow = function
    | Some a -> Printf.sprintf "(Some %s)" (ashow a)
    | None   -> "None"
  let equal aequal oa oa' = match oa, oa' with
    | Some a, Some a' -> aequal a a'
    | None,   None    -> true
    | _ -> false
  let compare acompare oa oa' = match oa, oa' with
    | Some a, Some a' -> acompare a a'
    | None,   None    ->   0
    | None,   Some _  -> ~-1
    | Some _, None    ->   1
  let shash ahash seed = function
    | Some a -> ahash (Hashtbl.seeded_hash seed "Types.Option.shash~Some") a
    | None   -> Hashtbl.seeded_hash seed "Types.Option.shash~None"
  let dcopy acopy x = match x with
    | None   -> None
    | Some x -> Some (acopy x)
end

module Thunk =
struct
  let show (_ : unit -> 'a) : string = "<thunk>"
  let equal (t : unit -> 'a) (t' : unit -> 'a) = t == t'
end

module Ref =
struct
  let show ashow ar = Printf.sprintf "(ref %s)" (ashow !ar)
  let equal (aequal : 'a -> 'a -> bool)
      (ar : 'a ref) (ar' : 'a ref) = ar == ar'
  let shash ahash s ar =
    Hashtbl.seeded_hash (ahash s !ar) "Ref.shash"
  let dcopy acopy ar = ref (acopy !ar)
end

module PHashtbl =
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
      (Hashtbl.seeded_hash s "Types.Hashtbl.shash")
   
end

module Tuple2 =
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

module Tuple3 =
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

module Tuple4 =
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

module Tuple5 =
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

module Tuple6 =
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

module Tuple7 =
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

module Tuple8 =
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

module Tuple9 =
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
