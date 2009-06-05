(* from JSOO *)

type obj

type value =
  | Obj of obj
  | Num of float
  | String of string
  | Nil

(* obtain objects *)
external eval : string -> obj = "jsoo_eval"
external inject : value -> obj = "jsoo_inject"
external extract : obj -> value = "jsoo_extract"
let null = inject Nil
let string s = inject (String s)

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

let ignore _ = ()

(* end *)


let alert msg =
  eval "window" >>> call_method "alert" [| string msg |] >>> ignore
;;

let write =
  let console = ref None in
    fun msg ->
      let console =
	match !console with
	  | None ->
	      let div = eval "document" >>> call_method "createElement" [| string "div" |] in
		div >>> get "style" >>> set "position" (string "fixed") ;
		div >>> get "style" >>> set "right" (string "10px") ;
		div >>> get "style" >>> set "bottom" (string "10px") ;
		div >>> get "style" >>> set "width" (string "40em") ;
		div >>> get "style" >>> set "height" (string "12em") ;
		div >>> get "style" >>> set "white-space" (string "pre") ;
		div >>> get "style" >>> set "background-color" (string "black") ;
		div >>> get "style" >>> set "color" (string "green") ;
		eval "document" >>> get "body" >>> call_method "appendChild" [| div |] >>> ignore ;
		div
	  | Some c -> c
      in
      let line =  eval "document" >>> call_method "createTextNode" [| string msg |] in
	console >>> call_method "appendChild" [| line |] >>> ignore
;;
