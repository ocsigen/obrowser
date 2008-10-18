(* We retrieve the DOM Node with the id *)
let cdiv = Js.get_element_by_id Sys.argv.(1) ;;

(* We append a CDATA node into it *)
Js.Node.append cdiv (Js.Node.text "Hello World") ;;
