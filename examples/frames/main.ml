open JSOO
open Boot
open Tk

(* example *)

let body = get_element_by_id "body"

let left_t = ref 0

let frame color =
  let frame = create "div" in
    frame >>> call_method "appendChild" [| text color |] >>> ignore ;
    frame >>> get "style" >>> set "position" (string "absolute") ;
    frame >>> get "style" >>> set "left" (string (string_of_int (!left_t + 10) ^  "px")) ;
    frame >>> get "style" >>> set "border" (string "1px black solid") ;
    frame >>> get "style" >>> set "padding" (string "10px") ;
    frame >>> get "style" >>> set "background" (string color) ;
    body >>> call_method "appendChild" [| frame |] >>> ignore ;
    let sbx = ref 0 in
    let sby = ref 0 in
    let smx = ref 0 in
    let smy = ref 0 in
    let rec move_handler (mx, my) = 
      let nx = !sbx + mx - !smx in
      let ny = !sby + my - !smy in
	frame >>> get "style" >>> set "left" (string (string_of_int nx ^ "px")) ;
	frame >>> get "style" >>> set "top" (string (string_of_int ny ^ "px"))
    in
    let down_handler evt = 
      let bx = frame >>> get "offsetLeft" >>> as_int in
      let by = frame >>> get "offsetTop" >>> as_int in
      let mx = evt >>> get "clientX" >>> as_int in
      let my = evt >>> get "clientY" >>> as_int in
	sbx := bx ;
	sby := by ;
	smx := mx ;
	smy := my ;
	window_mouse_move_event +:= move_handler
    and up_handler _ =
      window_mouse_move_event -:= move_handler
    in
      frame >>> set "onmousedown" (wrap_event down_handler) ;
      frame >>> set "onmouseup" (wrap_event up_handler) ;
      left_t := (frame >>> get "clientWidth" >>> as_int) + 10 + !left_t
 ;;

let _ =
  List.iter frame [ "red" ; "green" ; "blue" ; "yellow" ; "pink" ; "grey" ; "white"]
;;

