open Js ;;
open Html ;;
open Printf ;;

let cdiv = get_element_by_id Sys.argv.(1) ;;

let print (r,g,b) = sprintf "#%x%x%x" r g b ;;

let comp proj cb =
  let rec f n =
    if n < 0 then []
    else
      let col = proj (n,n,n) in
	(a
	   ~onclick:(fun () -> cb col) ~style:("background-color:" ^ print col)
	   [string (sprintf "%x" n)])
	:: (f (pred n))
  in f 15
;;

let d = div [string "#fff"] in
let r = ref 0xF and g = ref 0xF and b = ref 0xF in
  Node.replace_all cdiv (
    div (
      d
      :: comp
	(fun (x,_,_) -> (x,0xF,0xF))
	(fun (x,_,_) ->
	   r := x ;
	   Node.set_attribute d "style" ("background-color:" ^ print (!r,!g,!b)) ;
	   Node.replace_all d (string (print (!r,!g,!b))))
      @ br () :: comp
	(fun (_,x,_) -> (0xF,x,0xF))
	(fun (_,x,_) ->
	   g := x ;
	   Node.set_attribute d "style" ("background-color:" ^ print (!r,!g,!b)) ;
	   Node.replace_all d (string (print (!r,!g,!b))))
      @ br () :: comp
	(fun (_,_,x) -> (0xF,0xF,x))
	(fun (_,_,x) ->
	   b := x ;
	   Node.set_attribute d "style" ("background-color:" ^ print (!r,!g,!b)) ;
	   Node.replace_all d (string (print (!r,!g,!b))))
    ))
;;
