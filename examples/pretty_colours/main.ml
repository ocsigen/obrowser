open Js ;;
open Syntax_common ;;


let body =  get_element_by_id "body" in
let d = Html.div [] in
let t = Html.create "textarea" ~attrs:["rows","25";"cols","80"] () in
let langs = Html.select (List.map (fun s -> Html.option [Html.string s]) (available_syntaxes ())) in
  Node.append body
    (Html.div [
       t ;
       Html.br () ;
       langs ;
       Html.string " " ;
       Html.a
	 ~onclick:(fun () ->
		     colourise_source
		       (Node.get_attribute langs "value")
		       d (Node.get_attribute t "value"))
	 [Html.string "COLOURISE !"] ;
       Html.br () ;
       d
     ])
    
