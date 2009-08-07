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


(*This module is for using POST and GET method and for general communication
 * with a server*)

open AXOLang

(* internal only : encode a string according to percent-encoding system *)
let urlencode_string str =
  AXOJs.Node.window >>> JSOO.call_method "escape" [| JSOO.string str |]
                    >>> JSOO.as_string

(* decode a string according to percent encoding system *)
let urldecode_string str =
  AXOJs.Node.window >>> JSOO.call_method "unescape" [| JSOO.string str |]
                    >>> JSOO.as_string

(* internal only : takes a list of (name,value) and makes it url-friendly *)
let urlencode args =
 String.concat "&"
   (List.map
      (fun (n,v) -> (urlencode_string n) ^ "=" ^ (urlencode_string v))
      args
   )

(* These three functions are HL wrap for POST, GET and mixed HTTP method *)

(** [http_post url args] sends an HTTP POST request to the server with POST
  * arguments [args] nicely encoded and return
  * [(code,message)] where [code] is the HTTP code and [message] the content of
  * the answer. *)
let http_post url args =
  AXOJs.http_post url "application/x-www-form-urlencoded" (urlencode args)

(** [http_get_post url get_args post_args] makes and HTTP POST request with
  * [get_args] encoded and appended to [url] and [post_args] as POST arguments.
  * It's result also is [(code,message)] *)
let http_get_post url get_args post_args =
  AXOJs.http_post
    (url ^ "?" ^ (urlencode get_args))
    "application/x-www-form-urlencode"
    (urlencode post_args)

(** [http_get url args] sends an HTTP GET request with [args] encoded and
  * appended to [url]. Result is identicall to those of [http_post] and
  * [http_get_post]. *)
let http_get url args =
  AXOJs.http_get (url ^ "?" ^ (urlencode args))

(** [alert_on_code (code,message)] makes an alert pop up when [code] is 400 or
* 500. Behaviour for each code type can be set via optional [on_ixx] (where i is
* 1, 2, 3, 4 or 5) arguments. *)
let alert_on_code
     ?(on_1xx = fun _ -> ())
     ?(on_2xx = fun _ -> ())
     ?(on_3xx = fun _ -> ())
     ?(on_4xx = fun (_,m) -> AXOJs.alert m)
     ?(on_5xx = fun (_,m) -> AXOJs.alert m)
     res =
  match (fst res) / 100 with
    | 0 -> AXOJs.alert "Server is offline or couldn't be reached" ;
           failwith (snd res)
    | 1 -> on_1xx res
    | 2 -> on_2xx res
    | 3 -> on_3xx res
    | 4 -> on_4xx res
    | 5 -> on_5xx res
    | _ -> AXOJs.alert ("Server sent " ^ (string_of_int (fst res)))

(** [dynload_post url args parse] make a post request at [url] with [args] and
  * parse the result using [parse] with the result of the http_post request
  * in case of a 200 return code. If not 200 the default (overidable
  * via [on_ixx] (where i is 1, 3, 4 or 5) optional arguments) behaviour is to
  * fail. *)
let dynload_post url args
      ?(on_1xx = (fun (_,m) -> failwith m))
      ?(on_3xx = (fun (_,m) -> failwith m))
      ?(on_4xx = (fun (_,m) -> failwith m))
      ?(on_5xx = (fun (_,m) -> failwith m))
      parse =
  let (code, msg) as res = http_post url args in
    match code / 100 with
      | 0 -> AXOJs.alert "Server is offline or couldn't be reached" ;
             failwith msg
      | 1 -> on_1xx res
      | 2 -> parse msg
      | 3 -> on_3xx res
      | 4 -> on_4xx res
      | 5 -> on_5xx res
      | _ -> AXOJs.alert ("Server sent " ^ (string_of_int code)) ; failwith msg

(** [parse_xml str] makes a DOM tree out of an xml tree using the browser engine *)
let parse_xml = AXOJs.dom_of_xml

(** [print_xml obj] results in a string using the browser engine. *)
let print_xml = AXOJs.xml_of_dom

(** [pretty_print_xml obj] gives a xml string using the browser engine. *)
let pretty_print_xml = AXOJs.pretty_xml_of_dom


(** Firefox doesn't fail nor raise an exception when a parsing error occurs. It
  * just returns a "<parsererror>" XML document. The following function just
  * checks for this result. *)
let check_for_error dom =
  let ddom = dom >>> JSOO.get "documentElement" in
  let h = (ddom >>> JSOO.get "tagName") >>> JSOO.as_string in
    if h = "parsererror"
    then
      failwith (  "parsererror : "
                ^ (ddom >>> JSOO.get "textContent" >>> JSOO.as_string))
    else
      ()



(** Tamper url fragment (see http://ajaxpatterns.org/Unique_URLs ) for reasons
  * to use this 'hack' *)
let write_fragment s =
  AXOJs.Node.window >>> JSOO.get "location" >>> JSOO.set "hash" s
let read_fragment () =
  AXOJs.Node.window >>> JSOO.get "location"
                    >>> JSOO.get "hash"
                    >>> JSOO.as_string

module Url = struct

  (** This three functions are for managing fragment changes and the effect they
    * have. None of those have been tested and there are some obvious problems
    * that need to be corrected before serious use ("aux" should be made thread
    * friendly and "JSOO.wrap_event" is not the function to call here).
    *
    * The first is for initiating the listener loop (it starts an infinite loop)
    * The second is for adding (binding) an event.
    * The third for withdrawing (unbinding) an event.
    * *)
  let init_fragment_polling,
      add_on_fragment_change_event,
      remove_on_fragment_change_event
      =
    let last_hash = ref (read_fragment ()) in
    let closures = ref [] in
    let rec aux _ =
      if !last_hash = read_fragment ()
      then ()
      else (
        List.iter (fun f -> f ()) !closures ;
        last_hash := read_fragment () ;
      ) ;
      AXOJs.Node.window >>> JSOO.call_method "setTimeout"
        [| (JSOO.wrap_event aux) ; JSOO.int 500 |]
    in
    (
     (fun () -> aux (JSOO.inject JSOO.Nil)),
     (fun f -> closures := f :: (List.filter ((!=) f) !closures)),
     (fun f -> closures := List.filter ((!=) f) !closures)
    )


  (* This is just for local uses. *)
  let get_location () = JSOO.eval "window.location"

  (* For the following functions, we give the result you can expect them to give when they are called on "http://www.toto.org:8000/blah/blih?tutu=3#part_two".
     Some functions are named with an ending underscore. It's the mark of low level. *)

  (** Read the current fragment. *)
  let get_fragment_ () =
    urldecode_string ((get_location ()) >>> JSOO.get "hash" >>> JSOO.as_string)
  (* returns "part_two" *)

  (** Read the current host name *)
  let get_host_ () =
    urldecode_string ((get_location ()) >>> JSOO.get "hostname" >>> JSOO.as_string)
  (* returns "www.toto.org" *)

  (** Read the current port *)
  let get_port_ () = (get_location ()) >>> JSOO.get "port" >>> JSOO.as_int
  (* return 8000 *)

  (** Read the protocol *)
  let get_protocol_ () =
    urldecode_string ((get_location ()) >>> JSOO.get "protocol" >>> JSOO.as_string)
  (* return "http:" *)

  (** Read the arguments *)
  let get_arguments_ () =
    urldecode_string ((get_location ()) >>> JSOO.get "search" >>> JSOO.as_string)
  (* return "?tutu" *)

  (** Read the path *)
  let get_path_ () =
    urldecode_string ((get_location ()) >>> JSOO.get "pathname" >>> JSOO.as_string)
  (* return "/blah/blih" *)

  (** Get the value of the specified argument or raise Not_found if there is no argument with the given name. *)
  let get_argument name =
    try
      urldecode_string ( 
        (Regexp.exec
           (Regexp.make ((urlencode_string name) ^ "=([^&]*)"))
           ((get_location ()) >>> JSOO.get "search" >>> JSOO.as_string)
        ).(1)
      )
    with
      | Invalid_argument _ -> raise Not_found

  (** Get the path as a string list *)
  let get_path () =
    List.map urldecode_string
      (Array.to_list
         (Regexp.split
           (Regexp.make "/")
           ((get_location ()) >>> JSOO.get "pathname" >>> JSOO.as_string)
        )
      )

end
