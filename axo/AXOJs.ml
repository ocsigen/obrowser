(* Obrowser
 * http://www.ocsigen.org
 * Copyright (C) 2009
 * Raphaël Proust
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)


(* This module unifies JSOO and Js modules.
 * Every function as the target obj as last argument to allow the use of OO
 * style [argument >>> function]
 *
 * /!\ LOW LEVEL WARNING /!\
 * It is a very low level set of functions not to be used directly. If any of
 * this is needed as such in scripts, a high-level wraper probably needs to be
 * coded in the approriate higher module.
 * *)

open JSOO
(* allow the use of (>>>) ; "call_method" ; "get" ; "set" and "eval" *)
type obj = JSOO.obj

(** Use GET and POST method for server communication.
 * [http_get url] (and [http_post url arguments]) returns [(code,message)] where
 * code is the HTTP code and message is the message part of the server answer.
 * *)
let http_get = Js.http_get_with_status
let http_post = Js.http_post

(** Use these to convert from xml to dom and the other way around. *)
let dom_of_xml        = Js.dom_of_xml
let xml_of_dom        = Js.xml_of_dom
let pretty_xml_of_dom = Js.pretty_xml_of_dom

(** the next values are intended to give arguments to method called with the
 * [JSOO.call_method] function.*)
let null   = JSOO.null                     (* To send null     *)
let string = JSOO.string                   (* To give strings  *)
let float  = JSOO.float                    (* To pass floats   *)
let int    = JSOO.int                      (* To send Integers *)
let bool b = JSOO.int (if b then 1 else 0) (* To use  Booleans *)

module Node =
  (** This module is for manipulating Nodes (with the JSOO.obj type) and
   * their attributes. High level wrapping are all over the modules. As it's a
   * very standard part of Javascript, documentation can be found on Mozilla
   * Develloper Center*)
struct

  (* Standard nodes *)
  let window = eval "window"
  let document = eval "document"    
  let body = document >>> get "body" (**Unspecified behaviour on bodyless documents*)

  (* creating new nodes : HL versions in Html *)
  let text content =
    document >>> call_method "createTextNode" [| string content |]
  let element tag = (*[element "a"] returns the obj equivalent of [<a>] *)
    document >>> call_method "createElement" [| string tag |]

  (* tampering with attributes HL versions in Style *)
  let get_attribute name node =
    try node >>> get name >>> as_string with _ -> failwith "get_attribute"
  let set_attribute name value node =
    node >>> call_method "setAttribute" [| string name ; string value |]
         >>> ignore
  let remove_attribute name node =
    node >>> set name (inject Nil)

  (* getting nodes *)
  let get_element_by_id id root =
    root >>> call_method "getElementById" [| string id |]
  let get_element_by_tag tag root =
    root >>> call_method "getElementByTagName" [| string tag |]
  external children   : obj -> obj list   = "caml_js_node_children"
  external n_children : obj -> int        = "caml_js_node_n_children"
  external child      : obj -> int -> obj = "caml_js_node_child"
  let child n obj = child obj n
  let get_parent obj = (* Go up in the tree *)
    obj >>> call_method "getParent" [| |]
  let copy deep obj =
    obj >>> call_method "cloneNode" [| bool deep |]
  let get_value obj =
    obj >>> get "nodeValue"
    

  (* tampering with events : HL versions in Events *)
  let register_event name fn arg node =
    node >>> set name (wrap_event (fun _ -> ignore (fn arg)))
  let clear_event name f node =
    node >>> set name (inject Nil)

  (* adding/removing children HL versions in Widgets *)
  let append child node =
    node >>> call_method "appendChild" [| child |] >>> ignore
  let remove child node =
    node >>> call_method "removeChild" [| child |] >>> ignore
  let insert_before new_child ref_child node =
    node >>> call_method "insertBefore" [| new_child ; ref_child |]
            >>> ignore

  (* modifying children *)
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

  (* bulk add/remove *)
  let empty n = List.iter (fun c -> n >>> remove c) (n >>> children)
  let replace_all n c = empty n ; append n c

  (* whole (sub)tree operations *)
  let iter_width f n =
    n >>> f ;
    n >>> iter_rec f
  let rec iter_depth f n =
    n >>> f ;
    for i = 0 to n_children n -1 do
      (n >>> child i) >>> iter_depth f
    done

end


module Misc =
  (** Several useful things. May need improvements/completion *)
struct

  let navigator_id () = JSOO.eval "navigator.appName"
  let disable_selection () =
    JSOO.eval "document.onmousedown = function() {return false;}"
  let (new_z_index, current_z_index) = (* use it as a unique zIndex well *)
    let zindex = ref 0 in
      ((fun () -> incr zindex ; !zindex),
       (fun () -> !zindex))

end

(** [blunt_alert s] pop a window with s as a content. As it completely freeze
 * the vm, one should use [alert s] instead. *)
let blunt_alert = Js.alert

(** [alert s] is a nice version for [blunt_alert s] : background threads can
  * keep runing.
  * [rich_alert n] makes a pop up window just like [alert] but with a DOM tree
  * instead of a simple text. It allows the use of images, tables, link... in
  * the alert. *)
let (alert, rich_alert) = (* does not interrupt the Js engine *)
  let q = Queue.create () in
  let mask =
    let mask = Node.element "div" in
    mask >>> Node.set_attribute "style"
      "position: fixed; right: 0px; top: 0px; width: 100%; \
       height: 100%; background-color: grey; opacity: .4; \
       z-index: 2147483646; " ;
    mask
  in
  let show panel =
    Queue.push panel q ;
    Node.body >>> Node.append mask ;
    Node.body >>> Node.append panel;
  in
  let prepare text =
    let button = Node.element "a" in
      button >>> Node.append (Node.text "Ok") ;
      button >>> Node.set_attribute "style" "background-color: cyan" ;
    let msg = Node.element "div" in
      msg >>> Node.append (Node.text text) ;
    let panel = Node.element "div" in
      panel >>> Node.append msg ;
      panel >>> Node.append (Node.element "br") ;
      panel >>> Node.append button ;
      panel >>> Node.set_attribute "style"
        "position: fixed; right: 10%; top:  10%; \
         -moz-border-radius: 5px; padding: 10px; \
         background-color: white; text-align: right; \
         z-index: 2147483647;" ;
    let close () =
      Node.body >>> Node.remove mask ;
      Node.body >>> Node.remove panel ;
      ignore (Queue.pop q) ;
      if Queue.is_empty q
      then ()
      else show (Queue.pop q)
    in
      button >>> Node.register_event "onclick" close () ;
      panel
  in
  let rich_prepare obj =
    let button = Node.element "a" in
      button >>> Node.append (Node.text "Ok") ;
      button >>> Node.set_attribute "style" "background-color: cyan" ;
    let panel = Node.element "div" in
      panel >>> Node.append obj ;
      panel >>> Node.append (Node.element "br") ;
      panel >>> Node.append button ;
      panel >>> Node.set_attribute "style"
        "position: fixed; right: 10%; top: 10%; \
         -moz-border-radius: 5px; padding: 10px; \
         background-color: white; text-align: right; \
         z-index : 2147483647;" ;
    let close () =
      Node.body >>> Node.remove mask ;
      Node.body >>> Node.remove panel ;
      ignore (Queue.pop q) ;
      if Queue.is_empty q
      then ()
      else show (Queue.pop q)
    in
      button >>> Node.register_event "onclick" close () ;
      panel
  in

    ((fun text ->
        let res = prepare text in
        if Queue.is_empty q
        then show res
        else (Queue.push res q)),
     (fun obj ->
        let res = rich_prepare obj in
        if Queue.is_empty q
        then show res
        else (Queue.push res q)))


(** The next two functions are NOT TO BE CALLED WITOUT Firebug ON *)

(** [debug s] write a message in the Firebug console (if aviable) *)
let debug msg =
  eval "console" >>> call_method "debug" [| string msg |] >>> ignore

(** [auto_debug f] catches exceptions raised by [f] and write their string
  * equivalent in the Firebug console. *)
let auto_debug f =
  try f ()
  with exc -> debug (Printexc.to_string exc) ; raise exc

