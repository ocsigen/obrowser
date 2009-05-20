open JSOO

(* boot *)
let window = eval "window"
let document = eval "document"
let alert msg =
  window >>> call_method "alert" [| string msg |] >>> ignore 
let get_element_by_id id =
  document >>> call_method "getElementById" [| string id |]
let create name =
  document >>> call_method "createElement" [| string name |]
let text content =
  document >>> call_method "createTextNode" [| string content |]

(* example *)

let body = get_element_by_id "body"

let move_handlers = ref []
let _ = window >>> set "onmousemove" (wrap_event (fun evt -> List.iter (fun f -> f evt) !move_handlers))
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
    let dragging = ref false in
    let rec down_handler evt = 
      let bx = frame >>> get "offsetLeft" >>> as_int in
      let by = frame >>> get "offsetTop" >>> as_int in
      let mx = evt >>> get "clientX" >>> as_int in
      let my = evt >>> get "clientY" >>> as_int in
	sbx := bx ;
	sby := by ;
	smx := mx ;
	smy := my ;
	dragging := true
    and up_handler evt = 
      dragging := false
    and move_handler evt = 
      if !dragging then begin
	let mx = evt >>> get "clientX" >>> as_int in
	let my = evt >>> get "clientY" >>> as_int in
	let nx = !sbx + mx - !smx in
	let ny = !sby + my - !smy in
	  frame >>> get "style" >>> set "left" (string (string_of_int nx ^ "px")) ;
	  frame >>> get "style" >>> set "top" (string (string_of_int ny ^ "px"))
      end
    in
      frame >>> set "onmousedown" (wrap_event down_handler) ;
      frame >>> set "onmouseup" (wrap_event up_handler) ;
      move_handlers := move_handler :: !move_handlers ;
      left_t := (frame >>> get "clientWidth" >>> as_int) + 10 + !left_t
 ;;

let _ =
  List.iter frame [ "red" ; "green" ; "blue" ; "yellow" ; "pink" ; "grey" ; "white"]
;;

