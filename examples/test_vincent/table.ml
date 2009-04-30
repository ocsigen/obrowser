external register_closure
  : int -> (unit -> unit) -> unit
  = "caml_register_closure"

external get_closure_arg
  : unit -> 'a
  = "caml_get_closure_arg"

let register_closure id f =
  register_closure id (fun () ->
			 try
			   ignore (f (get_closure_arg ())) ;
			   Thread.exit ()
			 with _ ->
			   Thread.exit ())
