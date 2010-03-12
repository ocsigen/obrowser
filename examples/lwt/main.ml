open Graphics;;

let _ =  
  Js.Node.append (Js.get_element_by_id "body") (open_graph 1000 1000) ;
  fill_circle 12 12 10

(* let i = get_image 0 0 24 24 *)

let (>>=) = Lwt.bind

(*
let rec f () =
  Js.alert "salut";
  Lwt_obrowser.sleep 2. >>= f

let _ = ignore (f ())
  *)

let rec f x0 y0 r alpha =
  Lwt_obrowser.yield () >>= fun () ->
(*  draw_image i (x0 + truncate (r *. (cos alpha)))
    (y0 + truncate (r *. (sin alpha))); *)
  fill_circle (x0 + truncate (r *. (cos alpha)))
    (y0 + truncate (r *. (sin alpha))) 10;
  f x0 y0 r (alpha +. 0.001)

let _ = ignore (f 500 550 100. 0.)
let _ = ignore (f 500 500 100. 0.)

let _ = Lwt_obrowser.run (fst (Lwt.wait ()))
