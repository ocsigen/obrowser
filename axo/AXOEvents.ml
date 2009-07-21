(*This module provides ways to tamper with events. One can use common cases at
* the end of the module as example.*)

open JSOO

module type PARAMS = sig
  type v (* event valuation *)
  val name : string
  val destruct : obj -> v
    (* /!\ The [obj] the [destruct] function is called upon is an event object
     * (and not the DOM object the event was fired upon ; to get the target node
     * of the event use get_target).*)
  val default_value : v option
    (* an error message is produced if default value is None
     * and the destruction failed *)
end

module Make = functor (Params : PARAMS) ->
struct
  open Params
  exception Cannot_destruct of exn
  let handlers_field = "caml_" ^ name ^ "_handlers"

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

(*[get_target evt] get the DOM node originaly associated to the event. *)
let get_target evt = evt >>> JSOO.get "target"

(*[get_current_target evt] get the DOM node currently associated to the event *)
let get_current_target evt = evt >>> JSOO.get "currentTerget"

(*[stop_propagation evt] prevent the event for going up in the DOM tree. *)
let stop_propagation evt = evt >>> JSOO.call_method "staopPropagation" [| |]


module Onclick =
  Make (
    struct
       type v = unit
       let name = "onclick"
       let destruct = fun _ -> ()
       let default_value = Some ()
     end)

module Mouse_up =
  Make (
    struct
      type v = int * int
      let name = "onmouseup"
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
      let destruct obj =
        (obj >>> get "clientX" >>> as_int,
         obj >>> get "clientY" >>> as_int)
      let default_value = None
    end
)
