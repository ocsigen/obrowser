class external number = object
  method get : int
  method incr : int -> unit
  method to_string : string
  method print : unit -> unit
end

class number' = object (self)
	inherit number as mom
	method incr n = mom # incr (n * 2)
	method to_string = string_of_int (self # get) ^ " !"
end

let _ =
  let v = new number' in
    v # incr 2 ;
    v # print () ;
    v # incr 3 ;
    v # print ()

