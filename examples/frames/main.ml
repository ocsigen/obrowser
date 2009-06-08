open JSOO
open Boot
open Events
open Style
open Geometry

let size = 80
let id = ref 0
let max_z = ref 0

let frame color =
  let frame = create "div" in
    (frame >>> style) # set_position `FIXED ;
    (frame >>> geometry) # set_bounds (!id * size, 0, size, size) ;
    frame >>> get "style" >>> set "background" (string color) ;
    body >>> append frame ;
    incr id ; incr max_z ;
    let sbx = ref 0 and sby = ref 0 and smx = ref 0 and smy = ref 0 in
    let move_handler (mx, my) = 
      let nx = !sbx + mx - !smx in
      let ny = !sby + my - !smy in
	(frame >>> geometry) # set_x ((nx / size) * size) ;
	(frame >>> geometry) # set_y ((ny / size) * size) ;
    in
      frame >>> Mouse_down.bind
	(fun (mx, my) ->
	   let bx = (frame >>> geometry) # x
	   and by = (frame >>> geometry) # y in
	     sbx := bx ; sby := by ;
	     smx := mx ; smy := my ;
	     window >>> Mouse_move.bind move_handler) ;
      frame >>> Mouse_down.bind
	(fun _ ->
	   incr max_z ;
	   frame >>> get "style" >>> set "zIndex" (int !max_z)
	) ;
      window >>> Mouse_up.bind
	(fun _ -> window >>> Mouse_move.unbind move_handler)
;;

let _ =
  List.iter frame [ "red" ; "green" ; "blue" ; "yellow" ; "pink" ; "grey" ; "cyan"]
;;

