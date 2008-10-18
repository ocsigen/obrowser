open Js
open Graphics

let my_id = Random.self_init () ; string_of_int (Random.int 5000)
  
let rec listen () =
  let lines = http_get ("poll/" ^ my_id) in
  let len = String.length lines in
  let rec up_to c n =
    if n >= len then invalid_arg "up_to" else (if lines.[n] = c then n else up_to c (succ n))
  in
  let rec parse n =
    try
      let n' = up_to '\n' n in
	Scanf.sscanf (String.sub lines n (n' - n)) "%d.%d.%d.%d.%d.%d"
	  (fun color size x0 y0 x1 y1 -> set_color color; set_line_width size ; moveto x0 y0 ; lineto x1 y1) ;
	parse (succ n')
    with Invalid_argument "up_to" -> ()
  in
    parse 0 ; Thread.delay 0.1 ; listen ()

let queue = ref []

let rec speak () =
  if !queue <> [] then (
    ignore (http_post "dispatch/" (List.fold_left (fun r s -> s ^ "\r\n" ^ r) "\r\n" !queue)) ;
    queue := []
  ) ; Thread.delay 0.3 ; speak ()

let _ =
  let color = ref black and size = ref 1 in
    Node.append (get_element_by_id "body") (Html.h1 [Html.string "Multi-user scribble in Caml"]) ;
    Node.append (get_element_by_id "body") (open_graph 200 200) ;
    Node.append (get_element_by_id "body")
      (Html.div
	 (Html.string "Color: "
	  :: (List.fold_right
		(fun (c,n) r -> Html.a ~href:"#" ~onclick:(fun () -> color := c)
		   ~style:("border:1px black solid;text-decoration:none;background-color:" ^ n) [Html.string "　"] :: Html.string " " :: r)
		[red, "red" ; green,"green" ; blue, "blue" ; white, "white"; black, "black" ] [])));
    Node.append (get_element_by_id "body")
      (Html.div
	 (Html.br () :: Html.string "Size: " 
	  :: (List.fold_right
		(fun i r -> Html.a ~href:"#" ~style:((Printf.sprintf "border-left: %dpx black solid; text-decoration: none" i))
		   ~onclick:(fun () -> size := i) [Html.string ""] :: Html.string "　":: r)
		[1;2;4;6;8;10;20] []))) ;
    ignore (http_get ("register/" ^ my_id)) ;
    ignore (Thread.create listen ()) ;
    ignore (Thread.create speak ()) ;
    while true do
      let e = wait_next_event [Button_down ; Key_pressed] in
      let x = e.mouse_x and y = e.mouse_y in
	queue := Printf.sprintf "%d.%d.%d.%d.%d.%d" !color !size x y x y :: !queue ;
	let rec draw x y =
	  let e = wait_next_event [Button_up ; Mouse_motion ] in
	  let x' = e.mouse_x and y' = e.mouse_y in
	    if e.button then (
	      queue := Printf.sprintf "%d.%d.%d.%d.%d.%d" !color !size x y x' y' :: !queue ;
	      set_color !color; set_line_width !size ; moveto x y ; lineto x' y' ;
	      draw x' y'
	    )
	in draw x y
      done
      
