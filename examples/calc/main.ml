open Js
open Html

let calc () =
  let id = fun x -> x in
  let input = input id id "1/((2+2)*3)" 20 true (fun _ -> ()) in
  let list =
    div ~style:"text-align: left; color: black; background-color: white; font-family: courier; " []
  in
    div [
      input.node ;
      a ~onclick:(fun () ->
		    Calc.eval_phrase
		      (fun s -> Node.append list (li ~style:"border-bottom: 1px black dashed;"
						    [string (input.get ()) ; br () ; string " = " ; string s]))
		      (fun s -> Node.append list (li ~style:"border-bottom: 1px black dashed; background-color: #FFCCCC"
						    [string (input.get ()) ; br () ; span ~style:"color:red;" [string s]]))
		      (input.get ())) [string "[Eval !]"] ;
      list
    ]
;;

let body = get_element_by_id "body" in
  Node.append body (calc ())
;;
