class external number : int ->
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
;;

class number' =
object (self)
  inherit number 13 as mom
  method incr n = mom # incr (n * 2)
  method print () = print_endline (self # to_string)
end
;;

let _ =
  let v = new number' in
  let c = new calculator v in
    c # add (new number 3) ;
    c # print () ;
;;
