(* This module unifies JSOO and Js modules *)

open JSOO

let http_get = Js.http_get_with_status
let http_post = Js.http_post

let null = JSOO.null
let string = JSOO.string
let float = JSOO.float
let int = JSOO.int

module Node =
struct

  (* creating nodes *)
  let window = eval "window"
  let document = eval "document"    
  let text content =
    document >>> call_method "createTextNode" [| string content |]
  let element tag =
    document >>> call_method "createElement" [| string tag |]

  (* tampering with attributes *)
  let get_attribute name node =
    try node >>> get name >>> as_string with _ -> failwith "get_attribute"
  let set_attribute name value node =
    node >>> call_method "setAttribute" [| string name ; string value |] >>> ignore
  let remove_attribute name node =
    node >>> set name (inject Nil)

  (* getting nodes *)
  let get_element_by_id id root =
    root >>> call_method "getElementById" [| string id |]
  external children   : obj -> obj list   = "caml_js_node_children"
  external n_children : obj -> int        = "caml_js_node_n_children"
  external child      : obj -> int -> obj = "caml_js_node_child"
  let child n obj = child obj n
  let get_parent obj =
    obj >>> call_method "getParent" [| |]

  (* tampering with events *)
  let register_event name fn arg node =
    node >>> set name (wrap_event (fun _ -> ignore (fn arg)))
  let clear_event name f node =
    node >>> set name (inject Nil)

  (* modifying children *)
  let append child node =
    node >>> call_method "appendChild" [| child |] >>> ignore
  let remove child node =
    node >>> call_method "removeChild" [| child |] >>> ignore
  let insert_before new_child ref_child node =
    node >>> call_method "insertBefore" [| new_child ; ref_child |]
            >>> ignore
  let iter f n =
    for i = 0 to n_children n - 1 do
      f (n >>> child i)
    done
  let rec iter_rec f n =
    for i = 0 to n_children n - 1 do
      f (n >>> child i) ;
      iter_rec f (n >>> child i)
    done
  let fold_left f s n =
    let m = n_children n in
    let rec fold i r = if i >= m then r else fold (i + 1) (f r (child i n)) in
      fold 0 s
  let empty n = List.iter (fun c -> n >>> remove c) (n >>> children)
  let replace_all n c = empty n ; append n c
  let copy_all n1 n2 = n1 >>> iter (fun c -> n2 >>> append c)

end


let blunt_alert = Js.alert
let body = Node.document >>> Node.get_element_by_id "body" (*TODO: get rid of that !*)
let alert =
   let prepare text =
     let mask = Node.element "div" in
       mask >>> Node.set_attribute "style"
         "position: fixed; right: 0px; top: 0px; width: 100%; \
          height: 100%; background-color: grey; opacity: .4;" ;
     let button = Node.element "a" in
       button >>> Node.append (Node.text "Ok") ;
       button >>> Node.set_attribute "style" "background-color: blue" ;
     let msg = Node.element "div" in
       msg >>> Node.append (Node.text text) ;
     let panel = Node.element "div" in
       panel >>> Node.append msg ;
       panel >>> Node.append (Node.element "br") ;
       panel >>> Node.append button ;
       panel >>> Node.set_attribute "style"
         "position: fixed; left: 50%; bottom: 50%; \
         -moz-border-radius: 5px; padding: 10px; \
         background-color: white; text-align: right;" ;
     let close () =
       body >>> Node.remove mask ;
       body >>> Node.remove panel ;
     in
       button >>> Node.register_event "onclick" close () ;
       (mask, panel)
   in
     fun text ->
       let (mask, panel) = prepare text in
       body >>> Node.append mask ;
       body >>> Node.append panel

let debug msg =
  eval "console" >>> call_method "debug" [| string msg |] >>> ignore 

let auto_debug f =
  try f ()
  with exc -> debug (Printexc.to_string exc)

module Misc =
struct

  let disable_selection () =
    JSOO.eval "document.onmousedown = function() {return false;}"

end
