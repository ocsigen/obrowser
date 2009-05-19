(* Java Script Objects Operations *)

type obj

type value =
  | Obj of obj
  | Num of float
  | String of string
  | Nil

(* obtain objects *)
external new_obj : obj -> obj = "jsoo_new"
external eval : string -> obj = "jsoo_eval"
external inject : value -> obj = "jsoo_inject"
external extract : obj -> value = "jsoo_extract"
let null = inject Nil
let string s = inject (String s)
let float f = inject (Num f)
let int i = inject (Num (float_of_int i))

(* set fields *)
external get : string -> obj -> obj = "jsoo_get"
external set : string -> obj -> obj -> unit = "jsoo_set"
let unset field obj = set field obj null

let (>>>) x f = f x

(* call JS functions *)
external call : obj -> obj array -> obj -> obj = "jsoo_call"
let call_method field args dest =
  let meth = get field dest in
    call dest args meth
let call_function args f =
  call null args f

(* build JS event handlers from caml closures *)
external wrap_event : (unit -> unit) -> obj = "jsoo_wrap_event"
external get_event_arg : unit -> obj = "jsoo_get_event_args"
let wrap_event f =
  wrap_event
    (fun () ->
       try
	 f (get_event_arg ()) ;
	 Thread.exit ();
       with e ->
	 eval "console" >>> call_method "debug" [| string (Printexc.to_string e) |] >>> ignore ;
	 Thread.exit ())

let as_string x = match extract x with String s -> s | _ -> failwith "as_string"
let as_obj x = match extract x with Obj o -> o | _ -> failwith "as_obj"
let as_int x = match extract x with Num f -> int_of_float f | _ -> failwith "as_int"
let as_float x = match extract x with Num f -> f | _ -> failwith "as_float"


(*
let alert msg =
  eval "window" >>> get "alert" >>> call [inject (String msg)] >>> ignore
;;

let forms () =
  let nforms = eval "document" >>> get "forms" >>> get "length" >>> as_int in
  let rec get_forms n acc = 
    if n < 0 then
      acc
    else
      get_forms (n - 1) ((eval "document" >>> get "forms" >>> get (string_of_int n)) :: acc)
  in
    get_forms (nforms - 1)
;;

let onclick_handler f =
  fun evt ->
    let x = evt >>> get "x" >>> as_int in
    let y = evt >>> get "y" >>> as_int in
      f x y ;
      inject Nil
;;
*)
