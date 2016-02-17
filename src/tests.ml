include TSuite
(* include TMonad *)
include TTypes

let run_tests () =

  List.iter Suite.test
    (TTypes.([
            bool_suite
     ;      char_suite
     ;       exn_suite
     ;     float_suite
     ;       int_suite
     ;     int32_suite
     ;     int64_suite
     ; nativeint_suite
     ;    string_suite
     ;      unit_suite
     ;     array_suite
     ;       fun_suite
     ;      lazy_suite
     ;      list_suite
     ;    option_suite
     ;   hashtbl_suite
     ;    tuple2_suite
     ;    tuple3_suite
     ;    tuple4_suite
     ;    tuple9_suite
     ]) @
     [    TSuite.suite
(*     ;    TMonad.suite *)
     ])

  ;
  


  ()



let _ = run_tests ()
