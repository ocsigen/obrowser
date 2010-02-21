Thread.create
  (fun () ->
     while true do
       let x, y = Corner_click.corner_click () in
	 Js.alert (Printf.sprintf "corner clicked at %d %d !" x y)
     done) ()
;;

Thread.create
  (fun () ->
     while true do
       let r = Corner_click.corner_click_callback (+) in
	 Js.alert (Printf.sprintf "callback %d !" r)
     done) ()
;;
