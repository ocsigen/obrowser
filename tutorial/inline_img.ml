open Js ;;
open Html ;;

let body = get_element_by_id "body" ;;

let h2 = ref 0 and h3 = ref 0 and h4 = ref 0 ;;
let imgs = ref [||]

let display_img idx =
  let nb = Array.length !imgs in
  let current = ref idx in
  let vimg = img ~src:("pictures/" ^ (!imgs).(!current)) () in
  let mask = 
    div ~style:"position: fixed; right: 0px; top: 0px; width: 100%; height: 100%;
              \ background-color: black; opacity: .8;" []
  and pdiv = 
    div ~style:"position: fixed; right: 10px; top: 10px; -moz-border-radius: 5px;
              \ padding: 10px; background-color: white; text-align: center;" [vimg ; br ()]
  in
  let time = int_input ~size:3 ~value:30 () in
  let diapo = span [] in
  let rec start_diapo () =
    Node.replace_all diapo
      (span [a
	       ~onclick:(fun () ->
			   let rec play t =
			     current := (!current + 1) mod nb ;
			     Node.set_attribute vimg "src" ("pictures/" ^ (!imgs).(!current)) ;
			     Thread.delay (float_of_int t) ; play t
			   in
			   let t = Thread.create play (time.get ()) in stop_diapo t)
	       [string "[PLAY]"] ; string " (" ; time.node ; string " secs)"])
  and stop_diapo t =
    Node.replace_all diapo
      (span [a ~onclick:(fun () -> Thread.kill t ; start_diapo () ) [string "[STOP]"]])
  in
    start_diapo () ;
    Node.append pdiv
      (div
	 [a
	    ~onclick:(fun () ->
			Node.remove body mask ;
			Node.remove body pdiv)
	    [string "[CLOSE]"] ;
	  string " - " ;
	  a
	    ~onclick:(fun () ->
			current := (!current + nb - 1) mod nb ;
			Node.set_attribute vimg "src" ("pictures/" ^ (!imgs).(!current)))
	    [string "[< PREV]"] ;
	  string " - " ;
	  a
	    ~onclick:(fun () ->
			current := (!current + 1) mod nb ;
			Node.set_attribute vimg "src" ("pictures/" ^ (!imgs).(!current)))
	    [string "[NEXT >]"] ;
	  string " - " ;
	  diapo]) ;
    Node.append body mask ;
    Node.append body pdiv
;;

let browse node =
  let rec browse (idx : int) (node : Node.t) =
    match try Node.get_attribute node "tagName" with _ -> "" with
      | "A" ->
	  (match decode_id (Node.get_attribute node "id") with
	   | _ :: "viewer" :: picture :: [] ->
	       Node.set_attribute node "id" ("picture_" ^ string_of_int idx) ;
	       Node.set_attribute node "href" "javascript:;" ;
	       Node.register_event node "onclick" display_img idx ;
	       (succ idx, [picture])
	   | _ -> (idx,[]))
      | _ ->
	  Node.fold_left
	    (fun (idx,r) c -> let (idx',r') = browse idx c in (idx', r@r'))
	    (idx, [])
	    node
  in snd (browse 0 node)
;;

imgs := Array.of_list (browse body) ;;
