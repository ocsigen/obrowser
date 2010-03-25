open Js ;;
open JSOO ;;

let items = Array.to_list (Regexp.split (Regexp.make ~multi_line:true "\n") (http_get "list.txt")) in
let container = get_element_by_id "basket" in
  Node.append
    container
    (Html.ol
       (List.map
	  (fun n -> Html.li [Html.string n])
	  (List.filter ((<>) "") items)))
;;

let parse_html_part s =
  let tmp = eval "document" >>> call_method "createElement" [| string "div" |] in
    tmp >>> set "innerHTML" (string s) ; tmp >>> get "firstChild"
;;

Js.Node.append
  (eval "document" >>> get "body")
  (parse_html_part "<h1>POUET</h1>")
;;

