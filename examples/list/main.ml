open Js ;;

let items = Array.to_list (Regexp.split (Regexp.make ~multi_line:true "\n") (http_get "list.txt")) in
let container = get_element_by_id "basket" in
  Node.append
    container
    (Html.ol
       (List.map
	  (fun n -> Html.li [Html.string n])
	  (List.filter ((<>) "") items)))
;;

failwith "here is how exceptions are shown"
