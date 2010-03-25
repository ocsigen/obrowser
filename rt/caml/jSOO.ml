(* JavaScript Objects Operations *)

(** This operator tries to simulate the dot (.) of JavaScript. Its
    left operand being the subject object and its right one the
    message to send. *)
let (>>>) x f = f x

(* types *)

(** The abstract type obj represents a JS object *)
type obj ;;

(** The value type is used as an intermediate container to pass values
    between OCaml and JS *)
type value =
  | Obj of obj       (** identity *)
  | Num of float     (** javascript number / OCaml float *)
  | String of string (** strings *)
  | Block of Obj.t   (** identity, but checks if OCaml value *)
  | Nil              (** JS null *)
;;

(* object creators *)

(** creates An empty object *)
external new_obj : obj -> obj = "jsoo_new" ;;
(** Evaluates a JS code *)
external eval : string -> obj = "jsoo_eval" ;;
(** Transforms an OCaml value into a JS object *)
external inject : value -> obj = "jsoo_inject" ;;
(** Extracts an OCaml value from a JS object *)
external extract : obj -> value = "jsoo_extract" ;;
(** null JS value *)
let null = inject Nil ;;
(** Obtain a JS string from a string. The result is a copy so any
    modification to the original string does not affect the JS
    string. *)
let string s = inject (String s) ;;
(** Obtain a JS number from a float value *)
let float f = inject (Num f) ;;
(** Obtain a JS number from an int value *)
let int i = inject (Num (float_of_int i)) ;;

(* object extractors *)

(** extracts a string from a JS object, raises (Failure "as_string")
    in case of error *)
let as_string x = match extract x with String s -> s | _ -> failwith "as_string" ;;
(** extracts an object from a JS object, raises (Failure "as_obj") in
    case of error *)
let as_obj x = match extract x with Obj o -> o | _ -> failwith "as_obj" ;;
(** extracts a, int from a JS object, raises (Failure "as_int") in
    case of error *)
let as_int x = match extract x with Num f -> int_of_float f | _ -> failwith "as_int" ;;
(** extracts a floatfrom a JS object, raises (Failure "as_float") in
    case of error *)
let as_float x = match extract x with Num f -> f | _ -> failwith "as_float" ;;
(** extracts a block from a JS object, raises (Failure "as_block") in
    case of error *)
let as_block x = match extract x with Block b -> b | _ -> failwith "as_block" ;;

(* field accessors *)

(** Access a property of a JS object, as a JS object. Parameters are
    reversed to be used with the (>>>) combinator defined above. For
    instance (o >>> get "f") is equivalent to o.f (or o["f"]) in JS *)
external get : string -> obj -> obj = "jsoo_get" ;;
(** Modify a property : (o >>> set "f" v) is equivalent to o.f = v in
    JS *)
external set : string -> obj -> obj -> unit = "jsoo_set" ;;
(** removes a property from an object *)
let unset field obj = set field obj null ;;

(* call JS functions *)

(** Generic call mechanism, takes the function object, the subject
    (bound to 'this' in the body and an array of JS objects to use as
    arguments *)
external call : obj -> obj array -> obj -> obj = "jsoo_call" ;;
(** Calls a method from an object *)
let call_method field args dest =
  let meth = get field dest in
    call dest args meth ;;
(** Calls a function object with a null subject *)
let call_function args f =
  call null args f ;;

(* build JS event handlers from caml closures *)

(** internal function *)
external wrap_event : (unit -> unit) -> obj = "jsoo_wrap_event" ;;
(** internal function *)
external get_event_arg : unit -> obj = "jsoo_get_event_args" ;;
(** Wraps an OCaml functional value into a JS object useable as an
event handler *)
let wrap_event f =
  wrap_event
    (fun () ->
       try
	 f (get_event_arg ()) ;
	 Thread.exit ();
       with e ->
	 Thread.thread_uncaught_exception e;
	 Thread.exit ())
;;


(* meta *)

external current_vm : unit -> obj = "current_vm"
