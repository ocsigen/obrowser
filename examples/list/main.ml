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

let prval v =
  let v = Obj.repr v in
  let rec prval v =
      if Obj.is_block v then
	let s = ref "(" in
	  s := !s ^ string_of_int (Obj.tag v) ^ ", " ;
	  s := !s ^ string_of_int (Obj.size v) ^ ", " ;
	  for i = 0 to Obj.size v - 1 do
	    s := !s ^ prval (Obj.field v i) ^ " "
	  done ; !s ^ ")"
      else
	string_of_int (Obj.obj v : int)
  in prval v
;;

alert (prval Array.set) ;;

let items = split (http_get "list.txt") '\n' in
let container = get_element_by_id "basket" in
  Node.append
    container
    (Html.ol
       (List.map
	  (fun n -> Html.li [Html.string n])
	  (List.filter ((<>) "") items)))
;;
