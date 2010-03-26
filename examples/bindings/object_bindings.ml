class external number : int -> < get : int ; incr : int -> unit ; to_string : string ; print : unit -> unit >

class number' =
object (self)
  inherit number 13 as mom
  method incr n = mom # incr (n * 2)
  method to_string = string_of_int (self # get) ^ " !"
end

let _ =
  let v = new number' in
    v # incr 2 ;
    v # print () ;
    v # incr 3 ;
    v # print ()

