open Js
open Table

let _ =
  register_closure
    132
    (fun (s, i) -> alert ("test : " ^ s ^ ", " ^ string_of_int i))
