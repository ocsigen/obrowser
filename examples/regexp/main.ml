

let lines =
  Array.to_list
    (Regexp.split
       (Regexp.make ~multi_line:true "\n")
       (Js.http_get "text.txt"))

let container = Js.get_element_by_id "basket"

let _ =
  List.iter
    (fun line ->
       let line =
	 Regexp.replace_fun
	   (Regexp.make ~case_insensitive:true "(n)(u)(l)(l)")
	   (fun _ t ->
	      let r = "<" ^ t.(1) ^ "*" ^ t.(2) ^ "*" ^ t.(3) ^ "*" ^ t.(4) ^ ">" in
		Js.alert r ; r)
	   line
       in
       Js.Node.append
	 container
	 (Js.Html.string line) ;
       Js.Node.append
	 container
	 (Js.Html.br ()))
    lines
    
