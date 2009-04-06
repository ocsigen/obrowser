module Table = Map.Make(
struct
type t = string
let compare = compare
end)


let table = ref Table.empty


let _ =
  table := Table.add "__1"
    (fun () ->
       Js.alert "aa";
    )
    !table


let rec browse node =
  (try
     let onclick = Js.Node.get_attribute node "onclick" in
       if onclick <> ""
       then begin
	 Js.alert onclick;
	 let f = Table.find "onclick" !table in
	   Js.Node.register_event node "onclick" f ()
       end
   with Failure "unbound attribute" | Not_found -> ());
  Js.Node.iter browse node
    
    
let _ = browse Js.Node.document



