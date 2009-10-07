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


(** This module provides ways to tamper with events. One can use common cases at
 * the end of the module as example. *)

open JSOO
open AXOLang

exception Cannot_destruct of exn

module type PARAMS = sig
  type v (* event valuation *)
  val name : string
  val name_modifier : string option
    (** A custom tag to ensure your bindings can't be unbound by another module.
        It changes the internal representation of bounded handlers. *)
  val destruct : obj -> v
    (** Converts the [obj] describing the event to a caml value.
     * /!\ The [obj] the [destruct] function is called upon is an event object
     * (and not the DOM object the event was fired upon ; to get the target node
     * of the event use [get_target]). 
     *)
  val default_value : v option
    (** The value to send if an exception occurs during the conversion
     *  of the value, if any.
     *  If the default value is [None] 
     *  and the destruction failed with exception [e],
     *  the exception [Cannot_destruct e] is raised.
     *)
end

module Make = functor (Params : PARAMS) ->
struct
  open Params

  let handlers_field = "caml_" ^ name ^ "_handlers"
                     ^ (LOption.unopt ~default:"" name_modifier)

  let bind f obj =
    let handlers =
      try
	Obj.obj (obj >>> get handlers_field >>> as_block)
      with Failure "as_block" ->
	(* first event handler *)
	let handlers = ref [] in
	  obj >>> set handlers_field (inject (Block (Obj.repr handlers))) ;
	  obj >>> set name
	    (wrap_event
	       (fun evt ->
		  let v =
		    try destruct evt with e ->
		      match default_value with
			| Some v -> v
			| None -> raise (Cannot_destruct e)
		  in
		    List.iter (fun f -> f v) !handlers)) ;
	  handlers
    in handlers := f :: (List.filter ((!=) f) !handlers)

  let unbind f obj =
    let handlers =
      try
	Obj.obj (obj >>> get handlers_field >>> as_block)
      with Failure "as_block" ->
	ref []
    in
      handlers := List.filter ((!=) f) !handlers ;
      if !handlers = [] then (
	obj >>> set handlers_field (inject Nil) ;
	obj >>> set name (inject Nil)
      )

    let clear () obj =
      obj >>> set handlers_field (inject Nil) ;
      obj >>> set name (inject Nil)
end

(** [get_target evt] get the DOM node originaly associated to the event. *)
let get_target evt = evt >>> JSOO.get "target"

(** [get_current_target evt] get the DOM node 
    currently associated to the event *)
let get_current_target evt = evt >>> JSOO.get "currentTerget"

(**[stop_propagation evt] prevent the event for going up in the DOM tree. *)
let stop_propagation evt = evt >>> JSOO.call_method "stopPropagation" [| |]


module Onclick =
  Make (
    struct
       type v = unit
       let name = "onclick"
       let name_modifier = None
       let destruct = fun _ -> ()
       let default_value = Some ()
     end)

module Mouse_up =
  Make (
    struct
      type v = int * int
      let name = "onmouseup"
      let name_modifier = None
      let destruct obj =
        (obj >>> get "clientX" >>> as_int,
         obj >>> get "clientY" >>> as_int)
      let default_value = None
    end
)

module Mouse_down =
  Make (
    struct
      type v = int * int
      let name = "onmousedown"
      let name_modifier = None
      let destruct obj =
        (obj >>> get "clientX" >>> as_int,
         obj >>> get "clientY" >>> as_int)
      let default_value = None
    end
)
