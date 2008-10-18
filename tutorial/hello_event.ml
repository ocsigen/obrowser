open Js ;;

(* We retrieve the DOM Node with the id *)
let cdiv = get_element_by_id Sys.argv.(1) ;;

(* We use high level functions from Html encapsulating the imperative DOM processing *)
Node.append cdiv
  (Html.a
     (* the click callback *)         ~onclick:(fun () -> alert "Hello World")
     (* the children of the a Node *) [Html.string "Wakeup, little VM !"])
;;
