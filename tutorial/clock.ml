open Js ;; open Html ;;

let cdiv = get_element_by_id Sys.argv.(1) ;;

let tdiv = Html.div ~style:"text-align:center; font-size: 300%" [] ;;
let paused = ref false ;;
let t0 = ref (Sys.time ()) ;;
  let pause_time = ref 0. and pause_t0 = ref 0.

let rec display () =
  let dt = Sys.time () -. !t0 -. !pause_time in
  let secs = int_of_float dt in
    Node.replace_all tdiv
      (string (Printf.sprintf "%02d:%02d:%02d" (secs / 3600) ((secs / 60) mod 60) (secs mod 60)))
;;

let rec display_thread () =
  if not !paused then display () ;
  Thread.delay 1. ; display_thread ()
;;

let _ =
  Node.append cdiv tdiv ;
  Node.append cdiv
    (div ~style:"text-align:center"
       [a ~onclick:(fun () ->
		      t0 := Sys.time () ; 
		      pause_time := 0. ;
		      pause_t0 := !t0 ; 
		      display ()) [string "RESET"] ;
	string " :: " ;
	a ~onclick:(fun () ->
		      if !paused then (
			pause_time := !pause_time +. (Sys.time () -. !pause_t0) ;
			paused := false
		      ) else (
			pause_t0 := Sys.time () ;
			paused := true
		      )) [string "RUN/PAUSE"] ]) ;
  ignore (Thread.create display_thread ())
;;
