open JSOO

(* boot *)
let window = eval "window"
let document = eval "document"
let alert msg =
  window >>> call_method "alert" [| string msg |] >>> ignore 
let get_element_by_id id =
  document >>> call_method "getElementById" [| string id |]

(* example *)
let _ =
  let basket = get_element_by_id "basket" in
  let sbx = ref 0 in
  let sby = ref 0 in
  let smx = ref 0 in
  let smy = ref 0 in
  let dragging = ref false in
  let down_handler evt = 
    let bx = basket >>> get "offsetLeft" >>> as_int in
    let by = basket >>> get "offsetTop" >>> as_int in
    let mx = evt >>> get "clientX" >>> as_int in
    let my = evt >>> get "clientY" >>> as_int in
      sbx := bx ;
      sby := by ;
      smx := mx ;
      smy := my ;
      dragging := true
  in
  let up_handler evt = 
    dragging := false
  in
  let move_handler evt = 
    if !dragging then begin
      let mx = evt >>> get "clientX" >>> as_int in
      let my = evt >>> get "clientY" >>> as_int in
      let nx = !sbx + mx - !smx in
      let ny = !sby + my - !smy in
	basket >>> get "style" >>> set "left" (string (string_of_int nx ^ "px")) ;
	basket >>> get "style" >>> set "top" (string (string_of_int ny ^ "px"))
    end
  in
    basket >>> set "innerHTML" (string "<b>et voilà</b>") ;
    basket >>> set "onmousedown" (wrap_event down_handler) ;
    basket >>> set "onmouseup" (wrap_event up_handler) ;
    window >>> set "onmousemove" (wrap_event move_handler) ;
 ;;
