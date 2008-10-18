open Js ;;
open Html ;;

let cdiv = get_element_by_id Sys.argv.(1) ;;

let stock = span [] ;;
let on_food_stock =
  let m = Mutex.create () in
    (fun f () ->
       Mutex.lock m ;
       let r = (try f stock with e -> Mutex.unlock m ; raise e) in
	 Mutex.unlock m ; r)
;;

let eater delay =
  let stomach = span [] in
  let rec eater () =
    on_food_stock
      (fun stock ->
	 let avail = Node.children stock in
	   match avail with
	     | [] -> ()
	     | el :: _ -> Node.append stomach el (* automatically deletes from stock (DOM) *)
      ) () ;
    Thread.delay delay ; eater ()
  in ignore (Thread.create eater ()) ; stomach
;;

let resource src =
  a ~onclick:(on_food_stock (fun stock -> Node.append stock (img ~src:src ()))) [img ~src:src ()]
;;

let _ =
  Node.append cdiv (
    div [
      div [string "Feed with " ;
	   resource "food/cherries.png" ; string " " ;
	   resource "food/strawberry.png" ; string " " ;
	   resource "food/lemon.png" ];
      div [string "Stock: " ; stock ] ;
      div [string "Greedy eater: " ; eater 1. ] ;
      div [string "Standard stomach: " ; eater 1.5 ] ;
      div [string "On a diet: " ; eater 2. ] ;
    ]
  )
;;
  
