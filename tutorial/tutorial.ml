open Js ;;
open Html ;;
open Syntax_common ;;

let show_example mdiv name desc args =
  let cdiv = div [] in
    Node.set_attribute mdiv "style" "border-left: 2px #888 solid ; padding-left: 1em" ;
    Node.append mdiv (span ~style:"font-weight: bold" [string desc]) ;
    Node.append mdiv (string " ") ;
    Node.append mdiv (a ~onclick:(fun () -> colourise_file "ocaml" cdiv (name ^ ".ml"))
			[string "[show]"]) ;
    Node.append mdiv (string " or ") ;
    Node.append mdiv (a ~href:(name ^ ".ml") [string "[download]"]) ;
    Node.append mdiv (string " source code, ") ;
    Node.append mdiv (a ~onclick:(fun () ->
				    exec
				      (name ^ ".exe.uue")
				      (Array.append [| name ^ "_win" |] args))
			[string "[run]"]) ;
    Node.append mdiv cdiv ;
    Node.append mdiv (div ~attrs:["id", name ^ "_win"] [])
;;

let show_source mdiv name lang =
  let cdiv = div [] in
    Node.set_attribute mdiv "style" "border-left: 2px #888 solid ; padding-left: 1em" ;
    Node.append mdiv (span ~style:"font-weight: bold" [string name]) ;
    Node.append mdiv (string " ") ;
    Node.append mdiv (a ~onclick:(fun () -> colourise_file lang cdiv name) [string "[show]"]) ;
    Node.append mdiv (string " or ") ;
    Node.append mdiv (a ~href:name [string "[download]"]) ;
    Node.append mdiv cdiv
;;

let rec browse node =
  match try Node.get_attribute node "tagName" with _ -> "" with
    | "DIV" | "SPAN" ->
	(match decode_id (Node.get_attribute node "id") with
	   | id :: "example" :: name :: desc :: args ->
	       Node.set_attribute node "id" id ;
	       show_example node name desc (Array.of_list args)
	   | id :: "source" :: name :: lang :: [] ->
	       Node.set_attribute node "id" id ;
	       show_source node name lang
	   | id :: "inline-source" :: lang :: [] ->
	       Node.set_attribute node "id" id ;
	       let code = List.fold_left
		 (fun r n ->
		    if Node.get_attribute n "nodeName" = "#comment" then
		      r ^ Node.get_attribute n "data"
		    else r) "" (Node.children node)  in
		 colourise_source lang node code
	   | _ -> ())
      | _ ->
	  Node.iter browse node
;;

browse Node.document ;;
