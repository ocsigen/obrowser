module Id = struct
  let name = "External stubs extractor"
  let version = "$Id: $"
end

module Make (Syntax : Camlp4.Sig.Camlp4Syntax) =
struct
  include Syntax

  let print_interf ?input_file ?output_file ast = ()

  let print_implem ?input_file ?output_file ast =
    let input_file = match input_file with Some s -> s | None -> assert false in
    let output_file = match output_file with Some s -> s | None -> assert false in
    let fout = open_out output_file in
      Printf.fprintf fout
	"/**** %s: stubs extracted from %s ****/\n"
	output_file input_file ;
      let rec print ast =
	match ast with
	  | <:str_item<$st1$; $st2$>> -> print st1 ; print st2
	  | <:str_item<external $s$ : $t$ = $sl$>> ->
		(match sl with
		   | Ast.LCons (sym, _) when sym.[0] <> '%' ->
		       Printf.fprintf fout
			 "/* %s */\nvalue %s (value) { return Val_unit; }\n"
			 s sym
		   | _ -> ())
	  | <:str_item< module $_$ ($_$ : $_$) = $me$ >>
	  | <:str_item< module $_$ : $_$ = $me$ >>
	  | <:str_item< module $_$ = $me$ >> -> print_me me	    
	  | _ -> ()
      and print_me ast =
	match ast with
	  | <:module_expr< $me1$ $me2$ >> -> print_me me1 ; print_me me2
	  | <:module_expr< functor ( $_$ : $_$ ) -> $me$ >>
	  | <:module_expr< ( $me$ : $_$ ) >> -> print_me me
	  | <:module_expr< struct $st$ end >> -> print st
	  | _ -> ()
      in
	print ast ;
	close_out fout
end

module Nada = Camlp4.Register.OCamlPrinter(Id)(Make)
