open Js ;;
open Html ;;
open Syntax_common ;;

(* syntax colouring parameters *)
let sty_comments =  "color:#808000; font-style:italic;"
and sty_string =    "color:darkviolet"
and sty_paren =  [| "background-color:#FF0000; color:white" (* too much '</m>' *);
		    "color:#008000" ; "color:#C0C000" ; "color:#C05600" ;
		    "color:#C00000" ; "color:#800080" ; "color:#0000C0" ;
		    "color:#008080" |]
and sty_odd_line =   "background-color:#fff"
and sty_even_line =  "background-color:#f8f8f8"
and sty_line_numbers = "color:#888"
and sty_js_op =      "color:darkblue;"
and sty_js_const =     "color:darkred;"
and sty_js_kwd =    "color:#008;"
and tab_size =      8

type tok = | Text of string | Space of int
	   | Com of string | Br of int
	   | String of string
	   | Js_op of string | Js_id of string | Js_num of string | Js_string of string

type css_step = Sec | Prop | Val

let pretty_colours code infos l_fragment flush =
  let kwds =
    let tbl = Hashtbl.create 200 in
      List.iter (fun k -> Hashtbl.add tbl k sty_js_const)
	[ "false" ; "this" ; "true" ; "null" ] ;
      List.iter (fun k -> Hashtbl.add tbl k sty_js_kwd)
	[ "break" ; "case" ; "catch" ;
	  "continue" ; "debugger" ; "default" ; "delete" ; "do" ;
	  "else" ; "finally" ; "for" ; "goto" ; "if" ; "import" ; "in" ;
	  "instanceof" ; "new" ; "return" ; "function" ;
	  "switch" ; "throw" ; "var" ; "try" ; "typeof" ; "while" ; "with" ; 
	] ;
      tbl
  in
  let colorize b = span ~style:(Hashtbl.find kwds b) [string b] in
  let jpar = ref 0 and jpar_color n = sty_paren.(max 0 (1 + (n mod (Array.length sty_paren - 1)))) in
  let  line_ofs = ref 0 in
  let rec emit = function
    | Com s -> Fragment.append l_fragment (span ~style:sty_comments [string s])
    | Br p -> line_ofs := p ; flush p
    | String s -> Fragment.append l_fragment (span ~style:sty_string [string s])
    | Text "" ->  ()
    | Text s -> Fragment.append l_fragment (string s)
    | Space n -> Fragment.append l_fragment (string (String.make n ' '))
    | Js_op s -> Fragment.append l_fragment (
 	match s.[0] with
	  | '(' -> let s = span ~style:(jpar_color !jpar) [string "("] in incr jpar ; s
	  | ')' -> decr jpar ; span ~style:(jpar_color !jpar) [string ")"]
	  | _ -> span ~style:sty_js_op [string s])
    | Js_string s -> Fragment.append l_fragment (span ~style:sty_js_const [string s])
    | Js_num s -> Fragment.append l_fragment (span ~style:sty_js_const [string s])
    | Js_id s -> Fragment.append l_fragment (try colorize s with Not_found -> string s)
  in
  let lex s =
    let sz = String.length s in
    let rec lex_js i =
      if i >= sz then () (* EOF *)
      else match s.[i] with
	| ' ' | '\t' -> lex_js_s 0 i
	| '"' -> lex_js_v i (i + 1)
	| '\'' -> lex_js_c i (i + 1)
	| '{' | '}' | ';' | ':' | '(' | ')' | '&' | '~' | '|' | '!' | '?' | '.'
	| '+' | '-' | '=' | '<' | '>' | '[' | ']' -> emit (Js_op (String.make 1 s.[i])) ; lex_js (i + 1) ;
	| 'a'..'z' | 'A'..'Z' | '_' -> lex_js_i i (i + 1)
	| '0'..'9' -> lex_js_n i (i + 1)
	| '/' when i + 1 < sz && s.[i + 1] = '*' -> lex_js_coml i i
	| '/' when i + 1 < sz && s.[i + 1] = '/' -> lex_js_com i i
	| '\n' -> emit (Br i) ; lex_js (i + 1)
	| _ -> emit (Text (String.make 1 s.[i])) ; lex_js (i + 1) ;
    and lex_js_n p i =
      if i >= sz then emit (Js_num (String.sub s p (i - p)))
      else match s.[i] with
	| '0'..'9' | 'e' | '.' | 'x' | 'o' | 'X' -> lex_js_n p (i + 1)
	| _ -> emit (Js_num (String.sub s p (i - p))) ; lex_js i
    and lex_js_s n i =
      if i >= sz then () (* EOF spaces *)
      else match s.[i] with
	| ' ' -> lex_js_s (n + 1) (i + 1)
	| '\t' -> lex_js_s (n + ((i - !line_ofs) / tab_size + 1) * tab_size) (i + 1)
	| '\n' -> emit (Br (i + 1)) ; lex_js_s 0 (i + 1) (* EOL spaces *)
	| _ -> emit (Space n) ; lex_js i
    and lex_js_v p i =
      if i >= sz then emit (Js_string (String.sub s p (i - p + 1)))
      else match s.[i] with
	| '"' -> emit (Js_string (String.sub s p (i - p + 1))) ; lex_js (i + 1)
	| '\\' -> lex_js_v p (i + 2)
	| '\n' -> emit (Js_string (String.sub s p (i - p))) ; emit (Br (i + 1)) ; lex_js_v (i + 1) (i + 1)
	| _ -> lex_js_v p (i + 1)
    and lex_js_c p i =
      if i >= sz then emit (Js_string (String.sub s p (i - p + 1)))
      else match s.[i] with
	| '\'' -> emit (Js_string (String.sub s p (i - p + 1))) ; lex_js (i + 1)
	| '\\' -> lex_js_c p (i + 2)
	| '\n' -> emit (Js_string (String.sub s p (i - p))) ; emit (Br (i + 1)) ; lex_js_c (i + 1) (i + 1)
	| _ -> lex_js_c p (i + 1)
    and lex_js_i p i =
      if i >= sz then emit (Js_id (String.sub s p (i - p)))
      else match s.[i] with 
	| 'a'..'z' | 'A'..'Z' | '_' | '0'..'9' -> lex_js_i p (i + 1)
	| _ -> emit (Js_id (String.sub s p (i - p))) ; lex_js i
    and lex_js_com p i =
      if i >= sz then emit (Com (String.sub s p (i - p)))
      else match s.[i] with
	| '\n' -> emit (Com (String.sub s p (i - p))) ; emit (Br (i + 1)) ; lex_js (i + 1)
	| _ -> lex_js_com p (i + 1)
    and lex_js_coml p i =
      if i + 1 >= sz then emit (Com (String.sub s p (i - p)))
      else match s.[i], s.[i + 1] with
	| '*','/' -> emit (Com (String.sub s p (i - p + 2))) ; lex_js (i + 2)
	| '\n',_ -> emit (Com (String.sub s p (i - p))) ; emit (Br (i + 1)) ; lex_js_coml (i + 1) (i + 1)
	| _, _ -> lex_js_coml p (i + 1)
    in lex_js 0
  in
    if infos then (
      let t0 = Sys.time () in
	lex code ;
	emit (Br 0);
	emit (Com (Printf.sprintf "/*** coloured in %f seconds ***/" (Sys.time () -. t0))) ;
	flush 0
    ) else (
      lex code ;
      flush 0
    )
;;

register_syntax "js" pretty_colours ;;
register_syntax "javascript" pretty_colours ;;
