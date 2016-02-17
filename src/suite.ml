type result = [ `Succ
              | `Fail  of string
              | `Error of string
              ]
type test   = string * (unit -> result)
type suite  = string * test list

open Prof
open Types

let check_equal (type i) (type o)
    ?_ishow:   (_ishow :  i Show.t  option)
    ?_oequal:  (_oequal : o Equal.t option)
    ?_oshow:   (_oshow :  o Show.t  option)
  : string -> (i -> o) -> (i * o) -> test =
  fun name f (i, expect) ->
    let name = Printf.sprintf "%s %s" name (show ?_show:_ishow i) in
    let thunk () =
      try
        let o = f i in
        if equal ?_eq:_oequal o expect
        then `Succ
        else (`Fail (Printf.sprintf
                       "'%s': Expected `%s`, but got `%s`."
                       name
                       (show ?_show:_oshow expect)
                       (show ?_show:_oshow o)))
      with exc ->
        `Error (Printf.sprintf "%s: Exception: %s\n(expected `%s`)"
                  name
                  (Printexc.to_string exc)
                  (show ?_show:_oshow expect))
    in
    name, thunk

let check_equal1 = check_equal

let check_equal2 (type i1) (type i2) (type o)
  ?_i1show:(_i1show : i1 Show.t option) ?_i2show ?_oequal ?_oshow
  : string -> (i1 -> i2 -> o) -> (i1 * i2 * o) -> test =
  fun name f (i1, i2, o) ->
    check_equal1
      ?_ishow:_i2show ?_oequal ?_oshow
      (Printf.sprintf "%s %s" name (show ?_show:_i1show i1))
      (f i1) (i2, o)

let check_equal3 (type i1) (type i2) (type i3) (type o)
    ?_i1show:(_i1show : i1 Show.t option) ?_i2show ?_i3show
    ?_oequal ?_oshow
  : string -> (i1 -> i2 -> i3 -> o) -> (i1 * i2 * i3 * o) -> test =
  fun name f (i1, i2, i3, o) ->
    check_equal2
      ?_i1show:_i2show ?_i2show:_i3show ?_oequal ?_oshow
      (Printf.sprintf "%s %s" name (show ?_show:_i1show i1))
      (f i1) (i2, i3, o)

let test : suite -> unit =
  fun (name, tests) ->
    Printf.printf "Running test suite %s%!" name ;
    (if List.length tests = 0 then print_newline ()) ;
    let (nfail, nerr, nsucc, last), duration =
      time (fun () ->
      List.fold_left
        (fun (nfail, nerr, nsucc, last) (_, thunk) -> match thunk () with
           | `Succ      -> Printf.printf ".%!" ;
                           (nfail, nerr, nsucc+1, true)
           | `Fail  msg -> Printf.printf "\nFailure: %s\n%!" msg ;
                           (nfail+1, nerr, nsucc, false)
           | `Error msg -> Printf.printf "\nError:   %s\n%!" msg ;
                           (nfail, nerr+1, nsucc, false))
        (0, 0, 0, false)
        tests)
    in
    Printf.printf
      "%sRan %3i tests: %2i failures, %2i errors, and %3i successes in %f seconds.\n%!"
      (if last then "\n" else "")
      (nfail + nerr + nsucc)
      nfail
      nerr
      nsucc
      duration
