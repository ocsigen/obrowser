let split s c =
  let z = String.length s in
  let rec split i j =
    if j >= z then
      [String.sub s i (j - i)]
    else
      if s.[j] = c then
	String.sub s i (j - i) :: split (succ j) (succ j)
      else
	split i (succ j)
  in split 0 0
;;

open Js ;;

let items = split (http_get "list.txt") '\n' in
let container = get_element_by_id "basket" in
  Node.append
    container
    (Html.ol
       (List.map
	  (fun n -> Html.li [Html.string n])
	  (List.filter ((<>) "") items)))
;;
