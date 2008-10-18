let int_input name value =
  let res = Js.Fragment.create () in
    Js.Fragment.append res (Js.Node.text name) ;
    let input = Js.Node.element "input" in
      Js.Node.set_attribute input "type" "text" ;
      Js.Node.set_attribute input "value" (string_of_int !value) ;
      Js.Node.register_event input "onchange"
	(fun () ->
	   (value :=
	      try
		int_of_string (Js.Node.get_attribute input "value")
	      with _ -> !value) ;
	   Js.Node.set_attribute input "value" (string_of_int !value)
	) () ;
      Js.Fragment.append res input ;
      res

let button name callback =
  let res = Js.Fragment.create () in
  let input = Js.Node.element "input" in
    Js.Node.set_attribute input "type" "submit" ;
    Js.Node.set_attribute input "value" name ;
    Js.Node.register_event input "onclick" callback ();
    Js.Fragment.append res input ;
    res

let div id =
  let div = Js.Node.element "div" in
    Js.Node.set_attribute div "id" id ;
    div
      
let uid = let uid = ref 0 in fun () -> incr uid ; "caml__" ^ string_of_int  !uid

let _ =
  let main = Js.get_element_by_id "main" in
  let nbr, nbc, nbm = ref 10, ref 12, ref 15 in
    Js.Fragment.flush main (int_input "Number of columns" nbr) ;
    Js.Node.append main (Js.Node.element "br") ;
    Js.Fragment.flush main (int_input "Number of rows" nbc) ;
    Js.Node.append main (Js.Node.element "br") ;
    Js.Fragment.flush main (int_input "Number of mines" nbm) ;
    Js.Node.append main (Js.Node.element "br") ;
    Js.Fragment.flush main
      (button "nouvelle partie"
	 (fun () ->
	    let id = uid () in
	      Js.Node.append main (div id) ;
	      Js.exec
		"minesweeper.exe.uue"
		[| id ; string_of_int !nbc ; string_of_int !nbr ; string_of_int !nbm |]
	 )) ;
    
    
