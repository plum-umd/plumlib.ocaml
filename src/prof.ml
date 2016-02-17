open Gc
open Sigs

[@@@warning "-26"]

(**

   All from here:
   https://realworldocaml.org/v1/en/html/memory-representation-of-values.html
   
   Types have no runtime representation in ocaml. All variables are stored as a
   /value/, where the /value/ is one of: /integer/, /pointer/.

   The lowest bit is the tag distinguishing /pointers/ (0) from /integers/ (1).
   There are two types of /pointers/, those that point to /ocaml values/ and
   those that point to the system heap's /C pointers/. Only the former are
   followed by the GC. Heap blocks themselves are marked as either containing
   /ocaml values/ or opaque /C pointers/.

   A /block/ is the basic unit of heap allocation, always led by a one-word
   header (either 32 or 64 bits, CPU dependent).

    <------------------------------1 word------------------------------>
   +------------------------+----------+----------+----------+----------+----
   | size of block in words |  color   | tag byte | value[0] | value[1] | ...
   +------------------------+----------+----------+----------+----------+----
    <-either 22 or 54 bits-> <-2 bits-> <-8 bits->

   The first 22 or 54 bits encodes the length of the block (i.e. including all
   values of an array, record, etc).

   Color is used by the garbage collector.

   The tag byte has a number of purposes:

   - If tag > Obj.no_scan_tag, the data are all opaque blocks not followed by
     the GC. (this includes strings) Hmm, look into opaque custom blocks.
     Maybe I can have a GC that does what I want without hacking the runtime.
   - ...
   
*)

module Mem : sig
  type t = | Bit  of int
           | Word of int
           | Byte of int
           | KiB  of int
           | MiB  of int
           | GiB  of int
  include Data.S with type t := t
  val  bits : int -> t  val proj_bits  : t -> int
  val words : int -> t  val proj_words : t -> int
  val bytes : int -> t  val proj_bytes : t -> int
  val  kib  : int -> t  val proj_kib   : t -> int
  val  mib  : int -> t  val proj_mib   : t -> int
  val  gib  : int -> t  val proj_gib   : t -> int

  val lift  : (int -> int) -> (t -> t)
  val free  : ?print:bool -> unit -> t
  val proj  : t -> int
  val of_string : string -> t option

  val  (<!)   : t -> t -> bool
  val  (>!)   : t -> t -> bool
  val  (+!)   : t -> t -> t
  val  (-!)   : t -> t -> t
end =
struct
  type t = | Bit  of int
           | Word of int
           | Byte of int
           | KiB  of int
           | MiB  of int
           | GiB  of int
    [@@deriving eq, ord]
  let proj = function Bit n | Word n | Byte n |  KiB n |  MiB n |  GiB n -> n
  
  let suff t =
    let withs pre n = Printf.sprintf "%s%s" pre (if n = 1 then "" else "s")
    in match t with
    |  Bit n -> withs "bit"  n |  KiB _ -> "KiB"
    | Word n -> withs "word" n |  MiB _ -> "MiB"
    | Byte n -> withs "byte" n |  GiB _ -> "GiB"
  let show  t = Printf.sprintf "%s %s" (string_of_int (proj t)) (suff t)
  let pp ff t = Format.pp_print_string ff (show t)
  let shash s (t : t) = Hashtbl.seeded_hash s (show t)
  let dcopy x = x

  let digits = (* for rough safety tests *)
    let rec loop a n = if n < 10 then a else loop (a+1) (n/10) in
    loop 1

  let max_dig = digits max_int
  let ws = Sys.word_size
  let ( >.> ) = (lsr)
  let ( <.< ) = (lsl)
    
  let proj_bits = function
    |  Bit n -> n                 | KiB n -> n <.< 13
    | Word n -> n * Sys.word_size | MiB n -> n <.< 23
    | Byte n -> n <.<  3          | GiB n -> n <.< 33

  let proj_words = function
    |  Bit n -> n / ws         | KiB n -> (n <.< 13) / ws
    | Word n -> n              | MiB n -> (n <.< 23) / ws
    | Byte n -> (n <.< 3) / ws | GiB n -> (n <.< 33) / ws

  let proj_bytes = function
    |  Bit n ->  n >.> 3       | KiB n -> n <.< 10
    | Word n -> (n >.> 3) * ws | MiB n -> n <.< 20
    | Byte n ->  n             | GiB n -> n <.< 30

  let proj_kib = function
    |  Bit n ->  n >.> 13       | KiB n -> n
    | Word n -> (n >.> 13) * ws | MiB n -> n <.< 10
    | Byte n ->  n >.> 10       | GiB n -> n <.< 20

  let proj_mib = function
    |  Bit n ->  n >.> 23       | KiB n -> n >.> 10
    | Word n -> (n >.> 23) * ws | MiB n -> n
    | Byte n ->  n >.> 20       | GiB n -> n <.< 10

  let proj_gib = function
    |  Bit n ->  n >.> 33       | KiB n -> n >.> 20
    | Word n -> (n >.> 33) * ws | MiB n -> n >.> 10
    | Byte n ->  n >.> 30       | GiB n -> n

  let drop m = match m with
    |  Bit _ -> m                   | KiB n -> Byte (proj_bytes m)
    | Word n ->  Bit (proj_bits  m) | MiB n ->  KiB (proj_kib   m)
    | Byte n -> Word (proj_words m) | GiB n ->  MiB (proj_mib   m)

  let bump m =
    match m with
    |  Bit n -> Word (proj_words m) |  KiB n ->  MiB (proj_mib   m)
    | Word n -> Byte (proj_bytes m) |  MiB n ->  GiB (proj_gib   m)
    | Byte n ->  KiB (proj_kib   m) |  GiB _ -> m

  let rec norm m =
    let modki = (proj m) mod 1024 in
    if proj m = 0 then m else match m with
      |  GiB _ -> m
      | _ when modki = 0 -> norm (bump m)
      | _ -> m

  let check i =
    if i < 0
    then failwith "Need a positve amount of memory"
    else i
  
  let  bits i =  Bit (check i) |> norm
  let words i = Word (check i) |> norm
  let bytes i = Byte (check i) |> norm
  let   kib i =  KiB (check i) |> norm
  let   mib i =  MiB (check i) |> norm
  let   gib i =  GiB (check i) |> norm

  type unit = BitU | WordU | ByteU | KiBU | MiBU | GiBU [@@deriving ord]
  let unit = function
    |  Bit _ ->  BitU | KiB _ -> KiBU
    | Word _ -> WordU | MiB _ -> MiBU
    | Byte _ -> ByteU | GiB _ -> GiBU
  let cmp = compare_unit
  let ult a b = cmp a b < 0
  let ugt a b = cmp a b > 0

  (* keeps as much precision as possible *)
  let same_units origm origm' =
    let rec loop m m' =
      let um  = unit m  and md  = m  |> proj |> digits
      and u'm = unit m' and m'd = m' |> proj |> digits in
      if um = u'm then m, m'
      else if ult um u'm
      then if proj m = 0 then loop (bump m) origm'
           else if m'd < max_dig - 2 && ugt u'm BitU && proj (drop m') > 0
           then loop m (drop m')
           else loop (bump m) m'
      else if proj m' = 0 then loop origm (bump m')
      else if md < max_dig - 2 && ugt um  BitU && proj (drop m) > 0
      then loop (drop m) m'
      else loop m (bump m')
    in loop (norm origm) (norm origm')

  let (=!) m m' = let m, m' = same_units m m' in m = m'

  let (>!) m m' =
    let m, m' = same_units m m' in
    proj m > proj m'

  let (<!) m m' =
    let m, m' = same_units m m' in
    proj m < proj m'

  let (<=!) m m' = m < m' || m =! m'
  let (>=!) m m' = m > m' || m =! m'

  let equal = (=!)

  let lift f m = norm (match m with
      |  Bit i ->  Bit (f i)
      | Word i -> Word (f i)
      | Byte i -> Byte (f i)
      |  KiB i ->  KiB (f i)
      |  MiB i ->  MiB (f i)
      |  GiB i ->  GiB (f i))

  let (+!) origm origm' =
    let rec loop m m' =
      if      proj m  = 0 then origm'
      else if proj m' = 0 then origm
      else if max (digits (proj m)) (digits (proj m')) + 1 >= max_dig
      then if ult (unit m) GiBU
           then loop (bump m) (bump m')
           else failwith "+! overflow"
      else lift ((+) (proj m)) m'
    in 
    let m, m' = same_units origm origm' in
    loop m m'

  let (-!) origm origm' =
    let m, m' = same_units origm origm' in
    if      proj m  = 0 then origm'
    else if proj m' = 0 then origm
    else if m >=! m'
    then lift ((-) (proj m)) m'
    else failwith "-! refusing to create negative memory"

  let of_string : string -> t option =
    (* the kb, mb, gbs are technically wrong *)
    let suffs = [ ["b";"bi";"bit";"bits"],   bits
                ; ["B";"byte";"bytes"],      bytes
                ; ["w";"W";"word";"words"],  words
                ; ["k";"K";"kb";"KB";"kib";"KiB"], kib
                ; ["m";"M";"mb";"MB";"mib";"MiB"], mib
                ; ["g";"G";"gb";"GB";"gib";"GiB"], gib
                ]
    in
    fun s ->
      match int_of_string (String.trim s) with
      | i -> Some (kib i) (* assume kilobytes if no unit given *)
      | exception (Failure "int_of_string")->
        let slen = String.length s in
        List.fold_right
          (fun (suffs, inj) -> function
             | Some _ as acc -> acc
             | acc ->
               let index =
                 List.fold_right
                   (fun suff ind ->
                      if ind > 0 then ind
                      else try Str.search_backward (Str.regexp suff) s (slen-1)
                        with Not_found -> ind)
                   suffs ~-1 in
               if index > 0
               then let before = Str.string_before s index in
                 let trim   = String.trim before in
                 let ios    = int_of_string trim in
                 Some (inj ios)
               else acc)
          suffs
          None

  let free ?(print=false) () =
    let default = mib 512 in
    if Sys.os_type = "Win32"
    then (Printf.printf "Can't get freemem without free, grep, and awk.\n%!" ;
          default)
    else
      try
        let o =
          Util.run_capture "free -k | grep Mem | awk -F' ' '{print $7}'" in
        let m =
          kib (int_of_string (match o with Some s -> s | _ -> assert false))
        in
        (if print then Printf.printf "Free says you have %s.\n%!" (show m));
        m
      with exn ->
        Printf.printf "Error occurred while checking free memory:\n%s\n%!"
          (Printexc.to_string exn) ;
        default

end

module Stat : sig
  type t = Gc.stat = {
    minor_words : float;
    promoted_words : float;
    major_words : float;
    minor_collections : int;
    major_collections : int;
    heap_words : int;
    heap_chunks : int;
    live_words : int;
    live_blocks : int;
    free_words : int;
    free_blocks : int;
    largest_free : int;
    fragments : int;
    compactions : int;
    top_heap_words : int;
    stack_size : int;
  } include Data.S with type t := t

  val empty : unit -> t
  val diff  : t -> t -> t

  val show_diff : t -> string
end = struct
  
  type t = Gc.stat = {
    minor_words : float;
    promoted_words : float;
    major_words : float;
    minor_collections : int;
    major_collections : int;
    heap_words : int;
    heap_chunks : int;
    live_words : int;
    live_blocks : int;
    free_words : int;
    free_blocks : int;
    largest_free : int;
    fragments : int;
    compactions : int;
    top_heap_words : int;
    stack_size : int;
  } [@@deriving eq, ord, show, make]

  let make =
    let def = Gc.quick_stat () in
    fun ?(minor_words=def.minor_words)
        ?(promoted_words=def.promoted_words)
        ?(major_words=def.major_words)
        ?(minor_collections=def.minor_collections)
        ?(major_collections=def.major_collections)
        ?(heap_words=def.heap_words)
        ?(heap_chunks=def.heap_chunks)
        ?(live_words=def.live_words)
        ?(live_blocks=def.live_blocks)
        ?(free_words=def.free_words)
        ?(free_blocks=def.free_blocks)
        ?(largest_free=def.largest_free)
        ?(fragments=def.fragments)
        ?(compactions=def.compactions)
        ?(top_heap_words=def.top_heap_words)
        ?(stack_size=def.stack_size)
        () ->
      {
        minor_words ; promoted_words ; major_words ;
        minor_collections ; major_collections ; heap_words ;
        heap_chunks ; live_words ; live_blocks ;
        free_words ; free_blocks ; largest_free ;
        fragments ; compactions ; top_heap_words ;
        stack_size ;
      }

  let empty () =
    {
      minor_words=0. ; promoted_words=0. ; major_words=0. ;
      minor_collections=0 ; major_collections=0 ; heap_words=0 ;
      heap_chunks=0 ; live_words=0 ; live_blocks=0 ;
      free_words=0 ; free_blocks=0 ; largest_free=0 ;
      fragments=0 ; compactions=0 ; top_heap_words=0 ;
      stack_size=0 ;
    }

  let diff big small = {
    minor_words = big.minor_words -. small.minor_words
  ; promoted_words = big.promoted_words -. small.promoted_words
  ; major_words = big.major_words -. small.major_words
  ; minor_collections = big.minor_collections - small.minor_collections
  ; major_collections = big.major_collections - small.major_collections
  ; heap_words = big.heap_words - small.heap_words
  ; heap_chunks = big.heap_chunks - small.heap_chunks
  ; live_words = big.live_words - small.live_words
  ; live_blocks = big.live_blocks - small.live_blocks
  ; free_words = big.free_words - small.free_words
  ; free_blocks = big.free_blocks - small.free_blocks
  ; largest_free = big.largest_free - small.largest_free
  ; fragments = big.fragments - small.fragments
  ; compactions = big.compactions - small.compactions
  ; top_heap_words = big.top_heap_words - small.top_heap_words
  ; stack_size = big.stack_size - small.stack_size
  }

  let dcopy {
      minor_words ; promoted_words ; major_words ;
      minor_collections ; major_collections ; heap_words ;
      heap_chunks ; live_words ; live_blocks ;
      free_words ; free_blocks ; largest_free ;
      fragments ; compactions ; top_heap_words ;
      stack_size ;
    } = {
    minor_words ; promoted_words ; major_words ;
    minor_collections ; major_collections ; heap_words ;
    heap_chunks ; live_words ; live_blocks ;
    free_words ; free_blocks ; largest_free ;
    fragments ; compactions ; top_heap_words ;
    stack_size ;
  }

  let shash s (t : t) = Hashtbl.seeded_hash s t

  let itc i ?(prefix=if i >= 0 then "+" else "-") ?(suffix="") name acc =
    if i = 0 then acc else (name, prefix ^ (string_of_int i), suffix)::acc
  let wtc i  = itc i ~suffix:"words"
      
  let show_diff
      {
       minor_words ; promoted_words ; major_words ;
       minor_collections ; major_collections ; heap_words ;
       heap_chunks ; live_words ; live_blocks ;
       free_words ; free_blocks ; largest_free ;
       fragments ; compactions ; top_heap_words ;
       stack_size
      } =
    let diffs =
      [] |> wtc stack_size "stack_size"
         |> wtc (minor_words   |> truncate) "minor_words"
         |> wtc (promoted_words|> truncate) "promoted_words"
         |> wtc (major_words   |> truncate) "major_words"
         |> itc compactions "compactions"
         |> itc major_collections "major_collections" 
         |> itc minor_collections "minor_collections"
         |> wtc top_heap_words "top_heap_words"
         |> itc fragments "fragments"
         |> wtc largest_free "largest_free"
         |> itc free_blocks "free_blocks"
         |> wtc free_words "free_words"
         |> itc live_blocks "live_blocks"
         |> wtc live_words "live_words"
         |> wtc heap_words "heap_words"
    in
    let maxnm, maxv, maxu =
      List.fold_left
        (fun (maxnm, maxv, maxu) (nm, v, u) ->
           let nmlen = String.length nm
           and  vlen = String.length  v
           and  ulen = String.length  u in
           (if nmlen > maxnm then nmlen else maxnm),
           (if  vlen > maxv  then vlen  else maxv),
           (if  ulen > maxu  then ulen  else maxu))
        (0, 0, 0)
        diffs
    in
    if List.length diffs = 0 then "NO MEMORY FOOTPRINT"
    else diffs |> List.map (fun (nm, v, u) ->
                  Printf.sprintf
                    (Scanf.format_from_string
                       ("%" ^ (string_of_int maxnm) ^ "s: " ^
                        "%" ^ (string_of_int maxv)  ^ "s " ^
                        "%" ^ (string_of_int maxu)  ^ "s")
                       "%1s %1s %1s")
                    nm v u)
               |> String.concat "\n"
       
  
end

let time ?(print=false) ?(prefix="") f =
  let t0 = Unix.gettimeofday () in
  let y  = f () in
  let t  = Unix.gettimeofday () in
  (if print then Printf.printf "%s took %f s\n%!" prefix (t -. t0)) ;
  (y, t -. t0)

let optdef y = function Some x -> x | _ -> y
  
let gc_mod
    ?minor_heap_size
    ?major_heap_increment
    ?space_overhead
    ?verbose
    ?max_overhead
    ?stack_limit
    ?allocation_policy () =
  let conf = Gc.get () in
  Gc.set ({ minor_heap_size      = optdef conf.minor_heap_size minor_heap_size
          ; major_heap_increment = optdef conf.major_heap_increment
                                     major_heap_increment
          ; space_overhead       = optdef conf.space_overhead space_overhead
          ; verbose              = optdef conf.verbose verbose
          ; max_overhead         = optdef conf.max_overhead max_overhead
          ; stack_limit          = optdef conf.stack_limit stack_limit
          ; allocation_policy    = optdef conf.allocation_policy
                                     allocation_policy
          })

module Conf =
struct
  type t = Gc.control = {
    mutable minor_heap_size : int;
    mutable major_heap_increment : int;
    mutable space_overhead : int;
    mutable verbose : int;
    mutable max_overhead : int;
    mutable stack_limit : int;
    mutable allocation_policy : int;
  } [@@deriving show]
end


(* .01 < frac_free < 1  *)
let gc_halt (type a) : ?rel:float -> ?free:float -> ?minor:Mem.t ->
  (unit -> a) -> a = fun ?rel ?free ?minor f ->
  let default_conf = Gc.get () in
  let default = Mem.words default_conf.minor_heap_size in
  let minor_heap_size =
    (match free with
     | Some frac when frac > 0.01 ->
       Mem.free () |> Mem.lift (fun m -> float m *. frac |> truncate)
     | _ ->
       (match rel with         
        | Some rel when rel > 0. ->
          Mem.lift (fun m -> float m *. rel |> truncate) default
        | _ ->  optdef default minor))
    |> Mem.proj_words
  in
  gc_mod ~minor_heap_size
         ~major_heap_increment:10
         ~space_overhead:max_int
         ~max_overhead:max_int
         ~allocation_policy:1
         () ;
  Gc.full_major () ;
  let out = f () in
  Gc.set default_conf ;
  Gc.full_major () ;
  out

let mk_memweigh () =  
  (* empty overhead initially *)
  let overhead = ref (Stat.empty ()) in
  let memweigh (type a) ?(print=false) (f : unit -> a) () : a * Stat.t =
    let last = Gc.quick_stat () in
    let out  = f () in
    let curr = Gc.quick_stat () in
    let diff = Stat.diff (Stat.diff curr last) !overhead in
    (if print then Printf.printf "%s%!" (Stat.show_diff diff)) ;
    (out, diff)
  in
  (* Set the space overhead. Should only be a word on the stack if the
   * target is bytecode *)
  let nada () = () in
  overhead := snd (gc_halt (memweigh nada)) ;
  (* Return (memweigh : ?print:bool -> (unit -> 'a) -> 'a) *)
  memweigh

let mk_memabort () =  
  (* empty overhead initially *)
  let overhead = ref (Stat.empty ()) in
  let memabort ?(print=false) (f : unit -> Stat.t) () : Stat.t =
    let last = Gc.quick_stat () in
    let curr = f () in
    let diff = Stat.diff (Stat.diff curr last) !overhead in
    (if print then print_endline (Stat.show_diff diff)) ;
    diff
  in
  overhead := gc_halt (memabort Gc.quick_stat) ;
  (* return (memabort : ?print:bool -> (unit -> Stat.t) -> Stat.t) *)
  memabort

let nativep =
  let cache = ref None in
  let abort = ref None in
  fun () -> match !cache, !abort with
    | None, None ->
      let abortf = mk_memabort () in abort := Some abortf ;
      let litopt =
        0 = (gc_halt (abortf (fun () ->
             let intlit = 1 in Gc.quick_stat ()))).stack_size
      in
      abort := None ;
      cache := Some litopt ;
      litopt
    | Some litopt, _ -> litopt
    | _ -> assert false
  
let bytep () = not (nativep ())
