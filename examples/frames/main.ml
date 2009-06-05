open JSOO
open Boot
open Events

(* example *)

let body = get_element_by_id "body"

let left_t = ref 0
let max_zindex = ref 0

let frame color =
  let frame = create "div" in
    frame >>> call_method "appendChild" [| text color |] >>> ignore ;
    frame >>> get "style" >>> set "position" (string "absolute") ;
    frame >>> get "style" >>> set "left" (string (string_of_int (!left_t + 10) ^  "px")) ;
    frame >>> get "style" >>> set "border" (string "1px black solid") ;
    frame >>> get "style" >>> set "padding" (string "10px") ;
    frame >>> get "style" >>> set "background" (string color) ;
    body >>> call_method "appendChild" [| frame |] >>> ignore ;
    left_t := (frame >>> get "clientWidth" >>> as_int) + 10 + !left_t ;
    incr max_zindex ;
    let sbx = ref 0 and sby = ref 0 and smx = ref 0 and smy = ref 0 in
    let move_handler (mx, my) = 
      let nx = !sbx + mx - !smx in
      let ny = !sby + my - !smy in
	frame >>> get "style" >>> set "left" (string (string_of_int nx ^ "px")) ;
	frame >>> get "style" >>> set "top" (string (string_of_int ny ^ "px"))
    in
      frame >>> Mouse_down.bind
	(fun (mx, my) ->
	 let bx = frame >>> get "offsetLeft" >>> as_int in
	 let by = frame >>> get "offsetTop" >>> as_int in
	   sbx := bx ;
	   sby := by ;
	   smx := mx ;
	   smy := my ;
	   window >>> Mouse_move.bind move_handler) ;
      frame >>> Mouse_down.bind
	(fun _ ->
	   incr max_zindex ;
	   frame >>> get "style" >>> set "zIndex" (int !max_zindex)
	) ;
      frame >>> Mouse_up.bind
	(fun _ -> window >>> Mouse_move.unbind move_handler)
;;

let _ =
  List.iter frame [ "red" ; "green" ; "blue" ; "yellow" ; "pink" ; "grey" ; "white"]
;;

