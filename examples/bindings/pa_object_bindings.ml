open Camlp4

module Id : Sig.Id = struct
  let name = "O'Browser object bindings"
  let version = "0"
end

module Make (Syntax : Sig.Camlp4Syntax) = struct
  open Sig
  include Syntax

  let js_file = ref None
  let print_js s =
    match !js_file with
	None -> failwith "no JavaScript output specified"
      | Some f ->
	  output_string f (s ^ "\n")

  (* inject a value from ML to JS in ML *)
  let injector _loc = function
    | <:ctyp< int >> -> <:expr< JSOO.int >>
    | <:ctyp< string >> -> <:expr< JSOO.string >>
    | <:ctyp< unit >> -> <:expr< Obj.magic >>
    | _ -> failwith "unhandled type"

  (* extract a value from JS to ML in ML *)
  let  extractor _loc = function
    | <:ctyp< string >> -> <:expr< JSOO.as_string >>
    | <:ctyp< int >> -> <:expr< JSOO.as_int >>
    | <:ctyp< unit >> -> <:expr< ignore >>
    | _ -> failwith "unhandled type"

  (* inject a value from JS to ML in JS *)
  let js_injector _loc = function
    | <:ctyp< int >> -> "val_int"
    | <:ctyp< string >> -> "val_string"
    | <:ctyp< unit >> -> "(function(){return UNIT;})"
    | _ -> failwith "unhandled type"

  (* extract a value from ML to JS in JS *)
  let  js_extractor _loc = function
    | <:ctyp< string >> -> "string_val"
    | <:ctyp< int >> -> "int_val"
    | <:ctyp< unit >> -> "(function(){return UNIT;})"
    | _ -> failwith "unhandled type"



  let make_method n t _loc =
    let argc = ref 0 in
    let rec mk_args t =
      match t with
	  <:ctyp< $t1$ -> $t2$ >> ->
		print_js ("  a[" ^ string_of_int !argc ^ "] = " ^ js_injector _loc t1 ^ "(arguments[" ^ string_of_int !argc ^ "]);");
		let a = incr argc ; Printf.sprintf "a%d" !argc in
		let v = injector _loc t1 in
		let e = mk_args t2 in
		  <:expr< fun $lid:a$ ->
			    let $lid:a$ = $v$ $lid:a$ in
			      $e$ >>
	| _ ->
	    print_js ("  var res = this.vm.callback_method(this.mlo, \"" ^ n ^ "\", a);");
	    print_js ("  return " ^ js_extractor _loc t ^ " (res);");
	    let c = extractor _loc t in
	    let rec l n =
	      if n = 0 then
		<:expr<>>
	      else
		let r = l (n - 1) in
		let n = Printf.sprintf "a%d" n in
		<:expr< $lid:n$ ; $r$ >>
	    in
	    let l = l !argc in
    let nn = n ^ "_" in
	      <:expr< $c$ (JSOO.call_method $str:nn$ [| $l$ |] __jso) >>
    in
    let e = mk_args t in
      <:class_str_item< method $lid:n$ : $t$ = $e$ >>
	
    let make_methods _loc i mi ci m =
      let rec make = function
	| (n, t, _loc) :: l ->
	    print_js (ci ^ ".prototype." ^ n ^ "_ = " ^ i ^ ".prototype." ^ n);
	    print_js (ci ^ ".prototype." ^ n ^ " = function () {");
	    print_js ("  var a = [];");
	    let m = make_method n t _loc in
	      print_js ("}");
	      let l = make l in
		<:class_str_item< $m$ $l$ >>
	| [] ->
	    <:class_str_item< >>
      in make m

  EXTEND Gram
	GLOBAL: str_item;
      str_item: [
	[ "class" ; "external" ; i = a_LIDENT ; ":" ; pm = ctyp ->
	    let mi = "__caml_make_" ^ i in
	    let ci = "__caml_" ^ i in
	      print_js ("function " ^ ci ^ "(mlo, vm, args) {");
	      print_js ("  this.mlo = mlo;");
	      print_js ("  this.vm = vm;");
	      print_js ("  " ^ i ^ ".apply (this, args);") ;
	      print_js ("}") ;
	      print_js ("function " ^ mi ^ "(mlo, vm) {");
	      print_js ("var args = []; for (var i = 2;i < arguments.length;i++) args[i - 2] = arguments[i];");
              print_js ("  return new " ^ ci ^ " (mlo, vm, args);");
              print_js ("}") ;
	  let rec params = function
		<:ctyp< $t1$ -> $t2$ >> -> (t1, _loc) :: params t2
	      | _ -> []
	  in
	  let rec methods = function
		<:ctyp< $t1$ -> $t2$ >> -> methods t2
	      | <:ctyp< < $ms$ > >> ->
		(let rec aux = function
		   | <:ctyp< $lid:i$ : $t$ ; $e$ >> -> (i, t, _loc) :: aux e
		   | <:ctyp< $lid:i$ : $t$ >> -> [(i, t, _loc)]
		   | <:ctyp< >> -> []
		   | _ -> failwith "bad class type"
		 in aux ms)
	      | _ -> failwith "not a class type"
	  in
	  let p = params pm in
	  let m = methods pm in
	  let rec eargs ee n = function
	      _ :: l ->
		let nn = Printf.sprintf "a%d" n in
		let e = eargs ee (n + 1) l in
		  <:expr< $e$ $lid:nn$ >>
	    | [] -> <:expr< $ee$ >>
	  in
	  let targs n p =
	    let rec targs n = function
		v :: l ->
		  let e = targs (n + 1) l in
		    Ast.ExSem (_loc, <:expr< $v$ >>, e)
	      | [] -> <:expr< >>
	    in let ta = targs n p in <:expr< [| $ta$ |] >>
	  in
	  let rec fargs ee n = function
	      _ :: l ->
		let nn = Printf.sprintf "a%d" n in
		let e = fargs ee (n + 1) l in
		  <:expr< fun $lid:nn$ -> $e$ >>
	    | [] -> <:expr< $ee$ >>
	  in
	  let rec pargs ee n = function
	      _ :: l ->
		let nn = Printf.sprintf "a%d" n in
		let e = pargs ee (n + 1) l in
		  <:class_expr< fun $lid:nn$ -> $e$ >>
	    | [] -> <:class_expr< $ee$ >>
	  in
	  let transt l =
	    let rec tt n = function
	      | [] -> []
	      | (e, _loc) :: l -> 
		  let nn = Printf.sprintf "a%d" n in
		    <:expr< $injector _loc e$ $lid:nn$ >> :: tt (n + 1) l
	    in tt 0 l
	  in
	  let m = make_methods _loc i mi ci m in
	      <:str_item< 
	      let $lid:mi$ o = $fargs <:expr<JSOO.call_function  $targs 0 (<:expr<Obj.magic o>> :: <:expr<JSOO.current_vm ()>> :: transt p)$ (JSOO.eval $str:mi$)>> 0 p$
	  ;;
	  class $lid:i$ = $pargs <:class_expr< object (self)
	    val mutable __jso = JSOO.inject JSOO.Nil
	      $cst:m$
	    initializer
	      __jso <- $eargs <:expr<$lid:mi$ (Obj.repr self)>> 0 (List.rev p)$
	  end>> 0 p$
	    >>
	]
      ];
  END
    
  let _ = Camlp4.Options.add "-js" (Arg.String (fun s -> js_file := Some (open_out s))) "set JavaScript code output file name" ;;
end

let module M = Register.OCamlSyntaxExtension (Id) (Make) in ()
