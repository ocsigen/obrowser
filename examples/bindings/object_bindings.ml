type id

class external store : <
  add : string -> id ;
  get : id -> string
> ;;

let _ =
  print_endline "oh 1 !" ;
  let s = new store in
  print_endline "oh 2 !" ;
  let bob = s # add "bob" in
    print_endline "oh 3 !" ;
    print_endline (">> " ^ (s # get bob))
;;

class external number : int -> string ->
  < get : int ; 
    dup : number ;
    incr : int -> unit ;
    to_string : string ;
    print : unit -> unit >
;;

class external calculator : number ->
  < add : number -> unit ;
    result : number ;
    print : unit -> unit >
  = "Calculator"
;;

class external super_calculator inherit calculator : string ->
  < alert : unit >
  = "SuperCalculator"
;;

class number' =
object (self)
  inherit number 10 "bob" as mom
  method incr n = mom # incr (n * 2)
  method print () = print_endline (self # to_string)
end
;;

let _ =
  let v = new number' in
  let c = new calculator v in
  let c' = new super_calculator "haha" in
    c # add (new number 7 "marc") ;
    c # print () ;
    c' # add (c # result) ;
    c' # alert
;;
