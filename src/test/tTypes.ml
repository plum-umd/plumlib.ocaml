let h = Hashtbl.seeded_hash

open Suite
open Types
open Implicit

let bool_suite =
  "bool",
  List.map (check_equal2 "Basic.equal" equal)
    [ true, false, false ; false, true, false
    ; false, false, true ; true, true, true
    ] @
  List.map (check_equal2 "Basic.compare" compare)
    [ true, true, 0 ; false, false, 0
    ; true, false, 1 ; false, true, ~-1
    ] @
  List.map (check_equal2 "Basic.shash" (shash : int -> bool -> int))
    [ 42, false, h 42 false
    ; ~-7, true, h ~-7 true
    ] @
  List.map (check_equal "Basic.show" show)
    [ true, "true" ; false, "false" ] @
  List.map (check_equal "Basic.dcopy" (dcopy : bool -> bool)) (* <- necessary *)
    [ true, true ; false, false ]

let char_suite =
  "char",
  List.map (check_equal2 "Basic.equal" equal)
    [ 'a', 'a', true ; 'b', 'c', false
    ; ' ', ' ', true ; Char.chr 255, Char.chr 255, true
    ; Char.chr 254, Char.chr 253, false
    ] @
  List.map (check_equal2 "Basic.compare" compare)
    [ 'a', 'a', 0 ; 'b', 'c', ~-1
    ; ' ', ' ', 0 ; Char.chr 255, Char.chr 255, 0
    ; Char.chr 254, Char.chr 253, 1
    ] @
  List.map (check_equal2 "Basic.shash" shash)
    [ 42, 'a', h 42 'a'
    ; 42, 'b', h 42 'b'
    ; ~-1, ' ', h ~-1 ' '
    ; 0, Char.chr 254, h 0 (Char.chr 254)
    ] @
  List.map (check_equal "Basic.show" show)
    [ 'a', "a" ; ' ', " " ; Char.chr 254, "\\254"
    ] @
  List.map (check_equal "Basic.dcopy" (dcopy : char -> char))
    [ 'a', 'a' ; ' ', ' ' ; Char.chr 254, Char.chr 254 ]

let exn_suite =
  "exn",
  List.map (check_equal2 "Basic.equal" equal)
    [ Not_found, Not_found, true
    ; Not_found, Match_failure ("foo", 1, 0), false
    ; Match_failure ("foo", 1, 1), Match_failure ("foo", 1, 0), false
    ] @
  List.map (check_equal2 "Basic.compare" compare)
    [ Not_found, Not_found, 0
    ; Not_found, Match_failure ("foo", 1, 0), 1
    ; Match_failure ("foo", 1, 1), Match_failure ("foo", 1, 2), ~-1
    ] @
  List.map (check_equal2 "Basic.shash" shash)
    [ 42, Not_found, h 42 Not_found
    ; ~-1, Match_failure ("foo", 0, 1),
      h ~-1 (Match_failure ("foo", 0, 1))
    ] @
  List.map (check_equal "Basic.show" show)
    [ Not_found, "Not_found" ;
      Match_failure ("foo", 0, 1),
      "File \"foo\", line 0, characters 1-6: Pattern matching failed"
    ] @
  List.map (check_equal "Basic.dcopy" (dcopy : exn -> exn))
    [ Not_found, Not_found
    ; let e = Match_failure ("foo", 0, 1) in e, e
    ]

let float_suite =
  "float",
  List.map (check_equal2 "Basic.equal" equal)
    [ 0., 0., true ; ~-.3.14159, ~-.3.1415, false
    ] @
  List.map (check_equal2 "Basic.compare" compare)
    [ 0., 0., 0 ; 1., 0., 1 ; ~-.3.14159, ~-.3.1415, ~-1
    ] @
  List.map (check_equal2 "Basic.shash" shash)
    [ 42, 0.00001, h 42 0.00001
    ; ~-7, 14.423, h ~-7 14.423
    ] @
  List.map (check_equal "Basic.show" show)
    [ ~-.3.14159, "-3.14159" ; 0., "0."
    ] @
  List.map (check_equal "Basic.dcopy" (dcopy : float -> float))
    [ ~-.3.14159, ~-.3.14159 ; 0., 0. ]

let int_suite =
  "int",
  List.map (check_equal2 "Basic.equal" equal)
    [ 0, 0, true ; 0, 1, false ; 1, 0, false ; ~-1, ~-1, true
    ] @
  List.map (check_equal2 "Basic.compare" compare)
    [ 0, 0, 0 ; 0, 1, ~-1
    ; 1, 0, 1 ; ~-1, ~-1, 0
    ] @
  List.map (check_equal2 "Basic.shash" shash)
    [ 42, 0, h 42 0
    ; ~-7, 14, h ~-7 14
    ] @
  List.map (check_equal "Basic.show" show)
    [ 42, "42" ; ~-1, "-1" ; 0, "0"
    ] @
  List.map (check_equal "Basic.dcopy" (dcopy : int -> int))
    [ 42, 42 ; ~-1, ~-1 ; 0, 0 ]

let int32_suite =
  "int32",
  Int32.(List.map (check_equal2 "Basic.equal" equal)
             [ of_int 0, of_int 0, true
             ; of_int ~-1, of_int 1, false
             ; max_int, max_int, true
             ] @
           List.map (check_equal2 "Basic.compare" compare)
             [ of_int 0, of_int 0, 0
             ; of_int ~-1, of_int 1, ~-1
             ; max_int, max_int, 0
             ; max_int, min_int, 1
             ] @
           List.map (check_equal2 "Basic.shash" shash)
             [ 42, of_int 0, h 42 (of_int 0)
             ; ~-1, max_int, h ~-1 max_int
             ] @
           List.map (check_equal "Basic.show" show)
             [ of_int 0, "0" ; max_int, "2147483647" ] @
           List.map (check_equal "Basic.dcopy" (dcopy : int32 -> int32))
             [ of_int 0, of_int 0 ; max_int, max_int ])

let int64_suite =
  "int64",
  Int64.(List.map (check_equal2 "Basic.equal" equal)
             [ of_int 0, of_int 0, true
             ; of_int ~-1, of_int 1, false
             ; max_int, max_int, true
             ] @
           List.map (check_equal2 "Basic.compare" compare)
             [ of_int 0, of_int 0, 0
             ; of_int ~-1, of_int 1, ~-1
             ; max_int, max_int, 0
             ; max_int, min_int, 1
             ] @
           List.map (check_equal2 "Basic.shash" shash)
             [ 42, of_int 0, h 42 (of_int 0)
             ; ~-1, max_int, h ~-1 max_int
             ] @
           List.map (check_equal "Basic.show" show)
             [ of_int 0, "0" ; max_int, "9223372036854775807" ] @
           List.map (check_equal "Basic.dcopy" (dcopy : int64 -> int64))
             [ of_int 0, of_int 0 ; max_int, max_int ])

let nativeint_suite =
  "nativeint",
  Nativeint.(List.map (check_equal2 "Basic.equal" equal)
                 [ of_int 0, of_int 0, true
                 ; of_int ~-1, of_int 1, false
                 ; max_int, max_int, true
                 ] @
               List.map (check_equal2 "Basic.compare" compare)
                 [ of_int 0, of_int 0, 0
                 ; of_int ~-1, of_int 1, ~-1
                 ; max_int, max_int, 0
                 ; max_int, min_int, 1
                 ] @
               List.map (check_equal2 "Basic.shash" shash)
                 [ 42, of_int 0, h 42 (of_int 0)
                 ; ~-1, max_int, h ~-1 max_int
                 ] @
               List.map (check_equal "Basic.show" show)
                 [ of_int 0, "0" ; max_int, "9223372036854775807" ] @
               List.map (check_equal "Basic.dcopy" (dcopy : nativeint -> nativeint))
                 [ of_int 0, of_int 0 ; max_int, max_int ])

let string_suite =
  "string",
  List.map (check_equal2 "Basic.equal" equal)
    [ "foo", "foo", true ; "bar", "baz", false
    ; "", "", true ; "baz", "bazoinks", false
    ] @
  List.map (check_equal2 "Basic.compare" compare)
    [ "foo", "foo", 0 ; "baz", "bar", 1
    ; "", "", 0 ; "baz", "bazoinks", ~-1
    ] @
  List.map (check_equal2 "Basic.shash" shash)
    [ 42, "foo", h 42 "foo"
    ; ~-7, "bar", h ~-7 "bar"
    ; 0, "bazoinks", h 0 "bazoinks"
    ] @
  List.map (check_equal "Basic.show" show)
    [ "foo", "foo" ; "bar", "bar" ; "", ""
    ] @
  List.map (check_equal "Basic.dcopy" (dcopy : string -> string))
    [ "foo", "foo" ; "bar", "bar" ; "", "" ]

let unit_suite =
  "unit",
  List.map (check_equal2 "Basic.equal" equal)
    [ (), (), true ] @
  List.map (check_equal2 "Basic.compare" compare)
    [ (), (), 0 ] @
  List.map (check_equal2 "Basic.shash" shash)
    [ 42, (), h 42 () ] @
  List.map (check_equal "Basic.show" show)
    [ (), "()" ] @
  List.map (check_equal "Basic.dcopy" (dcopy : unit -> unit))
    [ (), () ]

let array_suite =
  "array",
  List.map (check_equal2 "Basic.equal" equal)
    [ [||], [||], true
    ; [|1;2;3;4|], [|1;2;3;4|], true
    ; [|1;2;3;4|], [|1;2;3|], false
    ] @
  List.map (check_equal2 "Basic.equal" equal)
    [ [|[[[]]];[[[]]]|], [|[[[]]];[[[]]]|], true
    ; [|[[["foo"]]];[[]];[]|], [|[[["foo"]]];[[]];[]|], true
    ; [|[[["foo"]]];[[]];[]|], [|[[["foo"]]]; [] ;[]|], false
    ] @
  List.map (check_equal2 "Basic.compare" compare)
    [ [||], [||], 0
    ; [|1;2;3;4|], [|1;2;3;4|], 0
    ; [|1;2;3;4|], [|1;2;3|], ~-1
    ] @
  List.map (check_equal2 "Basic.compare" compare)
    [ [|[[[]]];[[[]]]|], [|[[[]]];[[[]]]|], 0
    ; [|[[["foo"]]];[[]];[]|], [|[[["foo"]]];[[]];[]|], 0
    ; [|[[["foo"]]];[[]];[]|], [|[[["foo"]]]; [] ;[]|], 1
    ]  @
  List.map (check_equal2 "Basic.shash" shash)
    [ 42, [||], h 42 "Types.ArrayP.shash"
    ; 0, [|1;2;3;4|],
      Array.fold_left h
        (h 0 "Types.ArrayP.shash")
        [|1;2;3;4|]
    ] @
  List.map (check_equal "Basic.show" show)
    [ [||], "[||]"
    ; [|1;2;3;4|], "[|1; 2; 3; 4|]"
    ] @
  List.map (check_equal "Basic.show" show)
    [ [|[[[]]];[[[]]]|], "[|[[[]]]; [[[]]]|]"
    ; [|[[["foo"]]];[[]];[]|], "[|[[[foo]]]; [[]]; []|]"
    ; [|[[["foo"]]];[];[]|], "[|[[[foo]]]; []; []|]"
    ]  @
  List.map (check_equal "Basic.dcopy" (dcopy : int array -> int array))
    [ [||], [||]
    ; [|1;2;3;4|], [|1;2;3;4|]
    ] @
  List.map (check_equal "Basic.dcopy" (dcopy : string list list list array ->
                                       string list list list array))
    [ [|[[[]]];[[[]]]|], [|[[[]]];[[[]]]|]
    ; [|[[["foo"]]];[[]];[]|], [|[[["foo"]]];[[]];[]|]
    ; [|[[["foo"]]];[];[]|], [|[[["foo"]]];[];[]|]
    ]

let fun_suite =
  let id (type a) : a -> a = fun x->x in
  let i_to_i1 = fun (i : int) -> i in
  let i_to_i2 = (( * ) 2) in
  let yuck = Printf.sprintf "%s %a %d %f %a %!" in
  let i_to_s1 = string_of_int in
  let i_to_s2 = fun n -> string_of_int (n-1) in
  "fun",
  (check_equal2 "Basic.equal" equal (yuck, yuck, true)) ::
  List.map (check_equal2 "Basic.equal" (equal : (int -> string) -> (int -> string) -> bool))
    [ i_to_s1, i_to_s1, true
    ; i_to_s1, i_to_s2, false
    ] @
  List.map (check_equal2 "Basic.equal" (equal : (int -> int) -> (int -> int) -> bool))
    [ i_to_i1, i_to_i1, true
    ; i_to_i1, i_to_i2, false
    ; i_to_i1, id,       false
    ] @
  List.map (check_equal2 "Basic.shash" shash)
    [ 42, i_to_s1, h (h 42 i_to_s1) "Types.FunP.shash"
    ;  0, i_to_s1, h (h  0 i_to_s1) "Types.FunP.shash"
    ;  0, i_to_s2, h (h  0 i_to_s2) "Types.FunP.shash"
    ] @
  List.map (check_equal2 "Basic.shash" shash)
    [ 42, i_to_i1, h (h 42 i_to_i1) "Types.FunP.shash"
    ;  0, i_to_i1, h (h  0 i_to_i1) "Types.FunP.shash"
    ;  0, i_to_i2, h (h  0 i_to_i2) "Types.FunP.shash"
    ;  0, id, h (h  0 id) "Types.FunP.shash"
    ] @
  List.map (check_equal "Basic.show" show)
    [ i_to_i1, "(fun:?)"
    ; i_to_i2, "(fun:?)"
    ; id, "(fun:?)"
    ] @
  List.map (check_equal "Basic.show" show)
    [ i_to_s1, "(fun:?)"
    ; i_to_s2, "(fun:?)"
    ]

let lazy_suite =
  "lazy",
  List.map (check_equal2 "Basic.equal" equal)
    [ lazy [], lazy [], true
    ; lazy [4+5], lazy [5+5], false
    ; lazy [4+5], lazy [4+5;0], false
    ; lazy [4+5+6], lazy [15], true
    ] @
  List.map (check_equal2 "Basic.compare" compare)
    [ lazy [], lazy [], 0
    ; lazy [4+5], lazy [5+5], ~-1
    ; lazy [4+5;0], lazy [4+5], 1
    ; lazy [4+5+6], lazy [15], 0
    ] @
  List.map (check_equal2 "Basic.shash" shash)
    [ 42, lazy 4, (shash (h 42 "Types.LazyP.shash") 4)
    ] @
  List.map (check_equal "Basic.show" show)
    [ lazy "foo", "foo"
    ; lazy ("hello, " ^ "world"), "<not evaluated>"
    ]

let list_suite =
  "list",
  List.map (check_equal2 "Basic.equal" equal)
    [ [], [], true
    ; [1;2;3;4], [1;2;3;4], true
    ; [1;2;3;4], [1;2;3], false
    ] @
  List.map (check_equal2 "Basic.equal" equal)
    [ [[[[]]];[[[]]]], [[[[]]];[[[]]]], true
    ; [[[["foo"]]];[[]];[]], [[[["foo"]]];[[]];[]], true
    ; [[[["foo"]]];[[]];[]], [[[["foo"]]]; [] ;[]], false
    ] @
  List.map (check_equal2 "Basic.compare" compare)
    [ [], [], 0
    ; [1;2;3;4], [1;2;3;4], 0
    ; [1;2;3;4], [1;2;3], 1
    ] @
  List.map (check_equal2 "Basic.compare" compare)
    [ [[[[]]];[[[]]]], [[[[]]];[[[]]]], 0
    ; [[[["foo"]]];[[]];[]], [[[["foo"]]];[[]];[]], 0
    ; [[[["foo"]]];[[]];[]], [[[["foo"]]]; [] ;[]], 1
    ] @
  List.map (check_equal2 "Basic.shash" shash)
    [ 42, [], h 42 "Types.ListP.shash"
    ; 0, [1;2;3;4],
      List.fold_left h
        (h 0 "Types.ListP.shash")
        [1;2;3;4]
    ] @
  List.map (check_equal "Basic.show" show)
    [ [], "[]"
    ; [1;2;3;4], "[1; 2; 3; 4]"
    ] @
  List.map (check_equal "Basic.show" show)
    [ [[[[]]];[[[]]]], "[[[[]]]; [[[]]]]"
    ; [[[["foo"]]];[[]];[]], "[[[[foo]]]; [[]]; []]"
    ; [[[["foo"]]];[];[]], "[[[[foo]]]; []; []]"
    ] @
  List.map (check_equal "Basic.dcopy" (dcopy : int list -> int list))
    [ [], []
    ; [1;2;3;4], [1;2;3;4]
    ] @
  List.map (check_equal "Basic.dcopy" (dcopy : string list list list list ->
                                       string list list list list))
    [ [[[[]]];[[[]]]], [[[[]]];[[[]]]]
    ; [[[["foo"]]];[[]];[]], [[[["foo"]]];[[]];[]]
    ; [[[["foo"]]];[];[]], [[[["foo"]]];[];[]]
    ]

let option_suite =
  "option",
  List.map (check_equal2 "Basic.equal" equal)
    [ None, None, true
    ; Some [1;2;3;4], Some [1;2;3;4], true
    ; Some [1;2;3;4], Some [1;2;3], false
    ] @
  List.map (check_equal2 "Basic.equal" equal)
    [ Some (Some true), Some (Some true), true
    ; Some None, Some None, true
    ; Some (Some true), Some (Some false), false
    ] @
  List.map (check_equal2 "Basic.compare" compare)
    [ Some [], Some [], 0
    ; None, Some [], ~-1
    ; Some [], None, 1
    ; Some [1;2;3;4], Some [1;2;3;4], 0
    ; Some [1;2;3;4], Some [1;2;3], 1
    ] @
  List.map (check_equal2 "Basic.compare" compare)
    [ Some (Some true), Some (Some true), 0
    ; Some None, Some None, 0
    ; Some (Some true), Some (Some false), 1
    ; Some (Some false), Some (Some true), ~-1
    ] @
  List.map (check_equal2 "Basic.shash" shash)
    [ 42, Some [4], h (h (h 42 "Types.OptionP.shash~Some") "Types.ListP.shash") 4
    ; 0, None, h 0 "Types.OptionP.shash~None"
    ] @
  List.map (check_equal "Basic.show" show)
    [ Some [], "(Some [])"
    ; Some [1;2;3;4], "(Some [1; 2; 3; 4])"
    ] @
  List.map (check_equal "Basic.show" show)
    [ Some [[[[]]];[[[]]]], "(Some [[[[]]]; [[[]]]])"
    ; Some [[[["foo"]]];[];[]], "(Some [[[[foo]]]; []; []])"
    ; None, "None"
    ] @
  List.map (check_equal "Basic.dcopy" (dcopy : int list option -> int list option))
    [ Some [], Some []
    ; Some [1;2;3;4], Some [1;2;3;4]
    ; None, None
    ] @
  List.map (check_equal "Basic.dcopy" (dcopy : string list list list list option ->
                                       string list list list list option))
    [ Some [[[[]]];[[[]]]], Some [[[[]]];[[[]]]]
    ; Some [[[["foo"]]];[[]];[]], Some [[[["foo"]]];[[]];[]]
    ; Some [[[["foo"]]];[];[]], Some [[[["foo"]]];[];[]]
    ; None, None
    ]
(*
let ref_suite =
  "ref",
  List.map (check_equal2 "Basic.equal" equal)
    [ ref [1;2;3;4], ref [1;2;3;4], true
    ; ref [1;2;3;4], ref [1;2;3], false
    ] @
  List.map (check_equal2 "Basic.equal" equal)
    [ ref (Some true), ref (Some true), true
    ; ref None, ref None, true
    ; ref (Some true), ref (Some false), false
    ] @
  List.map (check_equal2 "Basic.compare" (compare : int list ref -> int list ref -> int))
    [ ref [], ref [], 0
    ; ref [], ref [0], ~-1
    ; ref [1;2;3;4], ref [1;2;3;4], 0
    ; ref [1;2;3;4], ref [1;2;3], 1
    ] @
  List.map (check_equal2 "Basic.compare" compare)
    [ ref (Some true), ref (Some true), 0
    ; ref None, ref None, 0
    ; ref (Some true), ref (Some false), 1
    ; ref (Some false), ref (Some true), ~-1
    ] @
  List.map (check_equal2 "Basic.shash" shash)
    [ 42, ref [4], h (h (h 42 "Types.RefP") "Types.ListP.shash") 4
    ; 0, ref [], h (h 0 "Types.RefP") "Types.ListP.shash"
    ] @
  List.map (check_equal "Basic.show" show)
    [ ref [], "(ref [])"
    ; ref [1;2;3;4], "(ref [1; 2; 3; 4])"
    ] @
  List.map (check_equal "Basic.show" show)
    [ ref [[[[]]];[[[]]]], "(ref [[[[]]]; [[[]]]])"
    ; ref [[[["foo"]]];[];[]], "(ref [[[[foo]]]; []; []])"
    ] @
  List.map (check_equal "Basic.dcopy" (dcopy : int list ref -> int list ref))
    [ ref [], ref []
    ; ref [1;2;3;4], ref [1;2;3;4]
    ] @
  List.map (check_equal "Basic.dcopy" (dcopy : string list list list list ref ->
                                       string list list list list ref))
    [ ref [[[[]]];[[[]]]], ref [[[[]]];[[[]]]]
    ; ref [[[["foo"]]];[[]];[]], ref [[[["foo"]]];[[]];[]]
    ] *)

let tuple2_suite =
  "tuple2",
  List.map (check_equal2 "Basic.equal" equal)
    [ ("foo", 42), ("foo", 42), true
    ; ("foo", 42), ("foo",  0), false
    ; ("baz", 42), ("bar", 42), false
    ] @
  List.map (check_equal2 "Basic.equal" equal)
    [ ("foo", (42, (3.14159, ()))), ("foo", (42, (3.14159, ()))), true
    ; ("foo", (42, (3.14159, ()))), ("foo", (41, (3.14159, ()))), false
    ; ("foo", (42, (3.14159, ()))), ("foo", (42, (3.1415,  ()))), false
    ] @
  List.map (check_equal2 "Basic.compare" compare)
    [ ("foo", 42), ("foo", 42), 0
    ; ("foo", 42), ("foo",  0), 1
    ; ("bar", 42), ("baz", 42), ~-1
    ] @
  List.map (check_equal2 "Basic.compare" compare)
    [ ("foo", (42, (3.14159, ()))), ("foo", (42, (3.14159, ()))), 0
    ; ("foo", (42, (3.14159, ()))), ("foo", (43, (3.14159, ()))), ~-1
    ; ("foo", (42, (3.14159, ()))), ("foo", (42, (3.1415,  ()))), 1
    ] @
  List.map (check_equal2 "Basic.shash" shash)
    [ 42, ("foo", 42), h (h 42 "foo") 42
    ; 42, ("foo",  0), h (h 42 "foo")  0
    ;  0, ("bar",  0), h (h  0 "bar")  0
    ] @
  List.map (check_equal "Basic.show" show)
    [ ("foo", 42), "(foo, 42)" ] @
  List.map (check_equal "Basic.show" show)
    [ ("foo", (42, (3.14159, ()))), "(foo, (42, (3.14159, ())))"] @
  List.map (check_equal "Basic.dcopy" (dcopy : (string * int) -> string * int))
    [ ("foo", 42), ("foo", 42) ] @
  List.map (check_equal "Basic.dcopy"
              (dcopy : string * (int * (float * unit)) ->
               string * (int * (float * unit))))
    [ ("foo", (42, (3.14159, ()))), ("foo", (42, (3.14159, ()))) ]

let tuple3_suite =
  "tuple3",
  List.map (check_equal2 "Basic.equal" equal)
    [ ("foo", [3.14], 42), ("foo", [3.14], 42), true
    ; ("foo", [3.14], 42), ("foo", [3.14],  0), false
    ; ("baz", [3.14], 42), ("baz", [3.14159], 42), false
    ] @
  List.map (check_equal2 "Basic.equal" equal)
    [ ("foo", 42, (3.14159, ())), ("foo", 42, (3.14159, ())), true
    ; ("foo", 42, (3.14159, ())), ("foo", 41, (3.14159, ())), false
    ; ("foo", 42, (3.14159, ())), ("foo", 42, (3.1415,  ())), false
    ] @
  List.map (check_equal2 "Basic.compare" compare)
    [ ([[1]], "foo", 42), ([[1]], "foo", 42), 0
    ; ([[1]], "foo", 42), ([[1]], "foo",  0), 1
    ; ([[ ]], "foo", 42), ([[1]], "foo", 42), ~-1
    ] @
  List.map (check_equal2 "Basic.compare" compare)
    [ ("foo", 42, (3.14159, ())), ("foo", 42, (3.14159, ())), 0
    ; ("foo", 42, (3.14159, ())), ("foo", 43, (3.14159, ())), ~-1
    ; ("foo", 42, (3.14159, ())), ("foo", 42, (3.1415,  ())), 1
    ] @
  List.map (check_equal2 "Basic.shash" shash)
    [ 42, ('a', "foo", 42), h (h (h 42 'a') "foo") 42
    ; 42, ('b', "foo",  0), h (h (h 42 'b') "foo")  0
    ] @
  List.map (check_equal "Basic.show" show)
    [ ("foo", 42, [[1]]), "(foo, 42, [[1]])" ] @
  List.map (check_equal "Basic.show" show)
    [ ("foo", 42, (3.14159, ())), "(foo, 42, (3.14159, ()))"] @
  List.map (check_equal "Basic.dcopy" (dcopy : (string * int * int list list) ->
                                       string * int * int list list))
    [ ("foo", 42, [[1];[]]), ("foo", 42, [[1];[]]) ] @
  List.map (check_equal "Basic.dcopy"
              (dcopy : string * int * (float * unit) ->
               string * int * (float * unit)))
    [ ("foo", 42, (3.14159, ())), ("foo", 42, (3.14159, ())) ]

let tuple4_suite =
  "tuple4",
  List.map (check_equal2 "Basic.equal" equal)
    [ ("foo", [3.14], 42, 'a'), ("foo", [3.14], 42, 'a'), true
    ; ("foo", [3.14], 42, 'a'), ("foo", [3.14],  0, 'a'), false
    ; ("baz", [3.14], 42, 'b'), ("baz", [3.14159], 42, 'a'), false
    ] @
  List.map (check_equal2 "Basic.equal" equal)
    [ ("foo", 42, 3.14159, ()), ("foo", 42, 3.14159, ()), true
    ; ("foo", 42, 3.14159, ()), ("foo", 41, 3.14159, ()), false
    ; ("foo", 42, 3.14159, ()), ("foo", 42, 3.1415,  ()), false
    ] @
  List.map (check_equal2 "Basic.compare" compare)
    [ ([[1]], "foo", 42, 'a'), ([[1]], "foo", 42, 'a'), 0
    ; ([[1]], "foo", 42, 'a'), ([[1]], "foo",  0, 'a'), 1
    ; ([[ ]], "foo", 42, 'a'), ([[1]], "foo", 42, 'a'), ~-1
    ] @
  List.map (check_equal2 "Basic.compare" compare)
    [ ("foo", 42, 3.14159, ()), ("foo", 42, 3.14159, ()), 0
    ; ("foo", 42, 3.14159, ()), ("foo", 43, 3.14159, ()), ~-1
    ; ("foo", 42, 3.14159, ()), ("foo", 42, 3.1415,  ()), 1
    ] @
  List.map (check_equal2 "Basic.shash" shash)
    [ 42, ('a', "foo", 42, 'a'), h (h (h (h 42 'a') "foo") 42) 'a'
    ; 42, ('b', "foo",  0, 'a'), h (h (h (h 42 'b') "foo")  0) 'a'
    ] @
  List.map (check_equal "Basic.show" show)
    [ ("foo", 42, [[1]], ()), "(foo, 42, [[1]], ())" ] @
  List.map (check_equal "Basic.show" show)
    [ ("foo", 42, 3.14159, ()), "(foo, 42, 3.14159, ())"] @
  List.map (check_equal "Basic.dcopy" (dcopy : (string * int * int list * char) ->
                                       string * int * int list * char))
    [ ("foo", 42, [1;3;2], 'a'), ("foo", 42, [1;3;2], 'a') ] @
  List.map (check_equal "Basic.dcopy"
              (dcopy : string * int * float * unit ->
               string * int * float * unit))
    [ ("foo", 42, 3.14159, ()), ("foo", 42, 3.14159, ()) ]

let tuple9_suite =
  "tuple9",
  List.map (check_equal2 "Basic.equal" equal)
    [ ("foo", [3.14], 42, 'a', (), [[[1]]], ('a', "b"), Some 42, lazy (3 + 4)),
      ("foo", [3.14], 42, 'a', (), [[[1]]], ('a', "b"), Some 42, lazy (3 + 4)),
      true
    ; ("foo", [3.14], 42, 'a', (), [[[ ]]], ('a', "b"), Some 42, lazy (3 + 4)),
      ("foo", [3.14], 42, 'a', (), [[[1]]], ('a', "b"), Some 42, lazy (3 + 4)),
      false
    ; ("foo", [3.14], 42, 'a', (), [[[1]]], ('a', "b"), Some 42, lazy (3 + 4)),
      ("foo", [3.14], 42, 'a', (), [[[1]]], ('a', "b"), Some 42, lazy 7),
      true
    ] @
  List.map (check_equal2 "Basic.compare" compare)
    [ ("foo", [3.14], 42, 'a', (), [[[1]]], ('a', "b"), Some 42, lazy (3 + 4)),
      ("foo", [3.14], 42, 'a', (), [[[1]]], ('a', "b"), Some 42, lazy (3 + 4)),
      0
    ; ("foo", [3.14], 42, 'a', (), [[[1]]], ('a', "b"), Some 42, lazy (3 + 4)),
      ("foo", [3.14], 42, 'a', (), [[[ ]]], ('a', "b"), Some 42, lazy (3 + 4)),
      1
    ; ("foo", [3.14], 42, 'a', (), [[[1]]], ('b', "b"), Some 42, lazy (3 + 4)),
      ("foo", [3.14], 42, 'a', (), [[[1]]], ('c', "b"), Some 42, lazy (3 + 4)),
      ~-1
    ] @
  List.map (check_equal2 "Basic.shash" shash)
    [ 42, ("foo", [3.14], 42, 'a', (), [[[1]]], ('a', "b"), Some 42, lazy (3 + 4)),
      shash (shash (shash (shash (shash (h (h (shash (h 42 "foo") [3.14]) 42) 'a') ())
                          [[[1]]])
                    ('a', "b"))
              (Some 42))
        (lazy (3 + 4))
    ] @
  List.map (check_equal "Basic.show" show)
    [ ("foo", [3.14], 42, 'a', (), [[[1]]], ('a', "b"), Some 42, lazy (3 + 4)),
      "(foo, [3.14], 42, a, (), [[[1]]], (a, b), (Some 42), <not evaluated>)"
    ] @
  List.map (check_equal "Basic.dcopy"
              (dcopy : (string * float list * int * char * unit * int list list list *
                        (char * string) * int option * int Lazy.t) ->
                       (string * float list * int * char * unit * int list list list *
                        (char * string) * int option * int Lazy.t)))
    [ ("foo", [3.14], 42, 'a', (), [[[1]]], ('a', "b"), Some 42, lazy (3 + 4)),
      ("foo", [3.14], 42, 'a', (), [[[1]]], ('a', "b"), Some 42, lazy (3 + 4)) ]

let hashtbl_suite =
  "hashtbl",
  (let h0 : (int, string) Hashtbl.t = Hashtbl.create 10
   and h1 : (int, string) Hashtbl.t = Hashtbl.create 100 in
   let t0 =
     let h0 = dcopy h0
     and h1 = dcopy h1 in
     List.map (check_equal2 "Basic.equal" equal)
       [ h0, h0, true ; h0, h1, true ; h1, h0, true ; h1, h1, true ] @
     List.map (check_equal2 "Basic.compare" compare)
       [ h0, h0, 0 ; h0, h1, 0 ; h1, h0, 0 ; h1, h1, 0 ] @
     List.map (check_equal2 "Basic.shash" shash)
       [ 42, h0, h 42 "Types.HashtblP2.shash"
       ;  0, h1, h  0 "Types.HashtblP2.shash"
       ] @
     List.map (check_equal "Basic.show" show)
       [ h0, "{}" ; h1, "{}" ] @
     List.map (check_equal "Basic.dcopy" (dcopy : (int, string) Hashtbl.t ->
                                          (int, string) Hashtbl.t))
       [ h0, h0 ; h1, h1 ]
   in
   Hashtbl.add h0 5 "foo" ; Hashtbl.add h1 0 "bar" ;
   Hashtbl.add h0 0 "bar" ; Hashtbl.add h1 5 "foo" ;
   let t1 =
     let h0 = dcopy h0
     and h1 = dcopy h1 in (* doesn't matter *)
     List.map (check_equal2 "Basic.equal" equal)
       [ h0, h0, true ; h0, h1, true ; h1, h0, true ; h1, h1, true ] @
     List.map (check_equal2 "Basic.compare" compare)
       [ h0, h0, 0 ; h0, h1, 0 ; h1, h0, 0 ; h1, h1, 0 ] @
     List.map (check_equal2 "Basic.shash" shash)
       [ 42, h0, h (h (h (h (h 42 "Types.HashtblP2.shash") "bar") 0) "foo") 5
       ;  0, h1, h (h (h (h (h  0 "Types.HashtblP2.shash") "bar") 0) "foo") 5
       ] @
     List.map (check_equal "Basic.show" show)
       [ h0, "{5 --> foo, 0 --> bar}"
       ; h1, "{5 --> foo, 0 --> bar}" ] @
     List.map (check_equal "Basic.dcopy" (dcopy : (int, string) Hashtbl.t ->
                                          (int, string) Hashtbl.t))
       [ h0, h0 ; h1, h1 ]
   in
   Hashtbl.replace h0 0 "bar" ; Hashtbl.replace h1 0 "baz" ;
   let t2 =
     List.map (check_equal2 "Basic.equal" equal)
       [ h0, h0, true ; h0, h1, false ; h1, h0, false ; h1, h1, true ] @
     List.map (check_equal2 "Basic.compare" compare)
       [ h0, h0, 0 ; h0, h1, ~-1 ; h1, h0, 1 ; h1, h1, 0 ] @
     List.map (check_equal2 "Basic.shash" shash)
       [ 42, h0, h (h (h (h (h 42 "Types.HashtblP2.shash") "foo") 5) "bar") 0
       ;  0, h1, h (h (h (h (h  0 "Types.HashtblP2.shash") "baz") 0) "foo") 5
       ] @
     List.map (check_equal "Basic.show" show)
       [ h0, "{0 --> bar, 5 --> foo}"
       ; h1, "{5 --> foo, 0 --> baz}" ] @
     List.map (check_equal "Basic.dcopy" (dcopy : (int, string) Hashtbl.t ->
                                          (int, string) Hashtbl.t))
       [ h0, h0 ; h1, h1 ]
   in
   t0@t1@t2)

  
open Types
let _ =
  print 5 ;
  print ("foo", 'a', [1;2;3;4;5])

