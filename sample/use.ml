open PLUM
open Implicit
open Types

let _ =
  assert (show "foo" = "foo") ;
  assert (show ("bar", 3.14, [1; 2; 3]) = "(bar, 3.14, [1; 2; 3])") ;
  assert (show "foo" = "foo") ;
  ()
