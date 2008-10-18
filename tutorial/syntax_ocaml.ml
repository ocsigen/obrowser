open Js ;;
open Html ;;
open Syntax_common ;;

(* syntax colouring parameters *)
let sty_uppercase = "color:darkgreen"
and sty_lowercase = "color:#00A; font-weight: bold;"
and sty_string =    "color:darkcyan"
and sty_numeric =   "color:darkcyan"
and sty_comments =  "color:#808000; font-style:italic;"
and sty_brackets =  "color:darkviolet"
and sty_infixes =   "color:darkred"
and sty_paren =  [| "background-color:#FF0000; color:white" (* too much ')' *);
		    "color:#008000" ; "color:#C0C000" ; "color:#C05600" ;
		    "color:#C00000" ; "color:#800080" ; "color:#0000C0" ;
		    "color:#008080" |]
and tab_size =      8

type tok = String of string | Id of string | Num of string | Space of int | Com of string | Br of int

let pretty_colours code infos l_fragment flush =
  let kwds =
    let tbl = Hashtbl.create 200 in
      List.iter (fun k -> Hashtbl.add tbl k sty_lowercase)
	["and" ; "as" ; "begin" ; "class" ; "constraint" ; "do" ; "done" ; "downto" ;
	 "else" ; "end" ; "exception" ; "external" ; "false" ; "for" ; "fun" ; "function" ;
	 "functor" ; "if" ; "in" ; "include" ; "inherit" ; "initializer" ; "lazy" ; "let" ;
	 "match" ; "method" ; "mod" ; "module" ; "mutable" ; "new" ; "object" ; "of" ; "open" ;
	 "private" ; "rec" ; "sig" ; "struct" ; "then" ; "to" ; "true" ; "try" ; "type" ; "val" ;
	 "virtual" ; "when" ; "while" ; "with" ; "abstract" ; "->" ; "~" ; "|" ; ";;" ] ;
      List.iter (fun k -> Hashtbl.add tbl k sty_brackets)
	[ ";" ;  "[" ; "]" ; "[|" ; "|]" ; "[<" ; ">]" ; "{" ; "}" ] ;
      List.iter (fun k -> Hashtbl.add tbl k sty_infixes)
	[ "lor" ; "lsl" ; "lsr" ; "asr" ; "or" ; "lxor" ; "land" ; "xor" ; "not" ; ":=" ] ;
      tbl
  in
  let colorize b = span ~style:(Hashtbl.find kwds b) [string b] in
  let par = ref 0 and par_color n = sty_paren.(max 0 (1 + (n mod (Array.length sty_paren - 1)))) in
  let line_ofs = ref 0 in
  let emit = function
    | String s -> Fragment.append l_fragment (span ~style:sty_string [string s])
    | Com s -> Fragment.append l_fragment (span ~style:sty_comments [string s])
    | Id s -> Fragment.append l_fragment (
	try colorize s with Not_found ->
	  match s.[0] with
	    | 'A'..'Z' -> span ~style:sty_uppercase [string s]
	    | '+' | '-' | ':' | '=' | '<' | '>' | '&' | '|' -> span ~style:sty_infixes [string s]
	    | '(' -> let s = span ~style:(par_color !par) [string "("] in incr par ; s
	    | ')' -> decr par ; span ~style:(par_color !par) [string ")"]
	    | _ -> string s)
    | Num s -> Fragment.append l_fragment (span ~style:sty_numeric [string s])
    | Br p -> line_ofs := p ; flush p
    | Space n -> Fragment.append l_fragment (string (String.make n ' '))
  in
  let lex s =
    let sz = String.length s in
    let rec lex_s n i =
      if i >= sz then () (* EOF spaces *)
      else match s.[i] with
	| ' ' -> lex_s (n + 1) (i + 1)
	| '\t' -> lex_s (n + ((i - !line_ofs) / tab_size + 1) * tab_size) (i + 1)
	| '\n' -> emit (Br (i + 1)); lex_s 0 (i + 1) (* EOL spaces *)
	| _ -> emit (Space n) ; lex_k i
    and lex_k i =
      let rec lex_k_s p i =
	if i >= sz then emit (String (String.sub s p (i - p)))
	else match s.[i] with
	  | '\\' -> lex_k_s p (i + 2)
	  | '"' -> emit (String (String.sub s p (i - p + 1))) ; lex_s 0 (i + 1)
	  | '\n' -> emit (String (String.sub s p (i - p))) ; emit (Br (i + 1)) ; lex_k_s (i + 1) (i + 1)
	  | _ -> lex_k_s p (i + 1)
      and lex_k_ch p i =
	if i >= sz then emit (String (String.sub s p (i - p)))
	else match s.[i] with
	  | '\\' -> lex_k_ch_1 p (i + 2)
	  | '\'' -> emit (String (String.sub s p (i - p + 1))) ; lex_s 0 (i + 1)
	  | _ -> lex_k_ch_1 p (i + 1)
      and lex_k_ch_1 p i =
	if i >= sz then (emit (Id "'") ; emit (String (String.sub s (p + 1) (i - p - 1))))
	else match s.[i] with
	  | '\'' -> emit (String (String.sub s p (i - p + 1))) ; lex_s 0 (i + 1)
	  | _ -> emit (Id "'") ; lex_s 0 (p + 1)
      and lex_k_i p i =
	if i >= sz then emit (Id (String.sub s p (i - p)))
	else match s.[i] with
	  | 'a'..'z' | 'A'..'Z' | '-' | '\'' | '0'..'9' | '_' -> lex_k_i p (i + 1)
	  | _ -> emit (Id (String.sub s p (i - p))) ; lex_s 0 i
      and lex_k_sp p i =
	if i >= sz then emit (Id (String.sub s p (i - p)))
	else match s.[i] with
	  | '|' | '<' | '>' | ']' -> emit (Id (String.sub s p (i - p + 1))) ; lex_s 0 (i + 1)
	  | _ -> emit (Id (String.sub s p (i - p))) ; lex_s 0 i
      and lex_k_n p i =
	if i >= sz then emit (Num (String.sub s p (i - p)))
	else match s.[i] with
	  | '0'..'9' | 'e' | '.' | 'x' | 'o' | 'X' -> lex_k_n p (i + 1)
	  | _ -> emit (Num (String.sub s p (i - p))) ; lex_s 0 i
      and lex_k_c p i =
	if i >= sz then emit (Id "(")
	else match s.[i] with
	  | '*' -> lex_k_c' p (i + 1) 0
	  | _ -> emit (Id "(") ; lex_s 0 i
      and lex_k_c' p i l =
	if i + 1 >= sz then emit (Com (String.sub s p (sz - 1 - p)))
	else match s.[i], s.[i+1], l with
	  | '*', ')',0 -> emit (Com (String.sub s p (i + 2 - p))) ; lex_s 0 (i + 2)
	  | '*', ')',_ -> emit (Com (String.sub s p (i + 2 - p))) ; lex_k_c' (i + 2) (i + 2) (l - 1)
	  | '(', '*',_ -> emit (Com (String.sub s p (i + 2 - p))) ; lex_k_c' (i + 2) (i + 2) (l + 1)
	  | '\n',_, _ -> emit (Com (String.sub s p (i - p))) ; emit (Br (i + 1)) ; lex_k_c' (i + 1) (i + 1) l
	  | _,   _, _ -> lex_k_c' p (i + 1) l
      in
	match s.[i] with
	  | '"' -> lex_k_s i (i + 1)
	  | '\'' -> lex_k_ch i (i + 1)
	  | 'a'..'z' | '_' | 'A'..'Z' -> lex_k_i i (i + 1)
	  | '0'..'9' -> lex_k_n i (i + 1)
	  | '[' | ']' | '|' -> lex_k_sp i (i + 1)
	  | '(' -> lex_k_c i (i + 1)
	  | _ -> emit (Id (String.sub s i 1)) ; lex_s 0 (i + 1)
    in lex_s 0 0
  in
    if infos then (
      let t0 = Sys.time () in
	lex code ;
	emit (Br 0);
	emit (Com (Printf.sprintf "(*** coloured in %f seconds ***)" (Sys.time () -. t0))) ;
	flush 0
    ) else (
      lex code ;
      flush 0
    )
;;

register_syntax "ocaml" pretty_colours ;;
register_syntax "ml" pretty_colours ;;

(*** (** (* SELF TEST *) **) ***)   
type 'a self_test = { mutable self : 'a self_test } ;;
let x' = 1 in
let x'x = x' * x' in ignore (((((((((((( x'x )))))))))))) ; "\"  
'\'' " ^ ((((( "'" ))))) ;;
