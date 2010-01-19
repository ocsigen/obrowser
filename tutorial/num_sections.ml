open Js ;;
open Html ;;
open Printf ;;

let body = get_element_by_id "body" ;;
let toc_div = get_element_by_id Sys.argv.(1) ;;

let h2 = ref 0 and h3 = ref 0 and h4 = ref 0 ;;
let toc_h2 = ref [] and toc_h3 = ref [] and toc_h4 = ref []

let rec browse node =
  match try Node.get_attribute node "tagName" with _ -> "" with
    | "H2" ->
	incr h2 ; h3 := 0 ; h4 := 0 ;
	toc_h3 := (Node.get_attribute node "textContent", (!h2, !h3), !toc_h4) :: !toc_h3 ;
	toc_h4 := [];
	toc_h2 := (Node.get_attribute node "textContent", !h2, !toc_h3) :: !toc_h2 ;
	toc_h3 := [];
	let content = Node.children node in
	  Node.empty node ;
	  let num = sprintf "%d" !h2 in
	  let new_content =
	    Html.a ~name:("TOC_" ^ num)
	      [string num] :: string " " :: content in
	    List.iter (Node.append node) new_content
    | "H3" ->
	incr h3 ; h4 := 0 ;
	toc_h3 := (Node.get_attribute node "textContent", (!h2, !h3), !toc_h4) :: !toc_h3 ;
	toc_h4 := [];
	let content = Node.children node in
	  Node.empty node ;
	  let num = sprintf "%d.%d" !h2 !h3 in
	  let new_content =
	    Html.a ~name:("TOC_" ^ num)
	      [string num] :: string " " :: content in
	    List.iter (Node.append node) new_content
    | "H4" ->
	incr h4 ;
	toc_h4 := (Node.get_attribute node "textContent", (!h2, !h3, !h4)) :: !toc_h4 ;
	let content = Node.children node in
	  Node.empty node ;
	  let num = sprintf "%d.%d.%d" !h2 !h3 !h4 in
	  let new_content =
	    Html.a ~name:("TOC_" ^ num)
	      [string num] :: string " " :: content in
	    List.iter (Node.append node) new_content
    | _ -> Node.iter browse node
;;

browse body ;;

Node.replace_all toc_div
  (Html.ul
     (List.fold_left
	(fun r (n,s,l) ->
	   Html.li [
	     a ~style:"font-weight: bold; font-size: 100%;" ~href:(sprintf "#TOC_%d" s) [string n] ;
	     Html.ul
	       (List.fold_left
		  (fun r (n,(s,ss),l) ->
		     Html.li [
		       a ~style:"font-size: 100%;" ~href:(sprintf "#TOC_%d.%d" s ss) [string n] ;
		       Html.ul
			 (List.fold_left
			    (fun r (n,(s,ss,sss)) ->
			       Html.li [
				 a ~style:"font-style: italic; font-size: 80%;"
				   ~href:(sprintf "#TOC_%d.%d.%d" s ss sss)
				   [string n]
			       ] :: r) [] l)
		     ] :: r) [] l)	   
	   ] :: r) [] !toc_h2))
;;
