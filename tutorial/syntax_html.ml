open Js ;;
open Html ;;
open Syntax_common ;;

(* syntax colouring parameters *)
let sty_comments =  "color:#808000; font-style:italic;"
and sty_entity =    "color:darkviolet"
and sty_attribute = "color:darkcyan"
and sty_equal =     "color:darkblue"
and sty_string =    "color:darkviolet"
and sty_paren =  [| "background-color:#FF0000; color:white" (* too much '</m>' *);
		    "color:#008000" ; "color:#C0C000" ; "color:#C05600" ;
		    "color:#C00000" ; "color:#800080" ; "color:#0000C0" ;
		    "color:#008080" |]
and sty_markup =  [| "background-color:#FF0000; color:white" (* too much '</m>' *);
		     "color:#004000" ; "color:#707000" ; "color:#703300" ;
		     "color:#700000" ; "color:#400040" ; "color:#000070" ;
		     "color:#004040" |]
and sty_css_sec =    "color:darkred;"
and sty_css_op =     "color:darkblue;"
and sty_css_prop =   "color:blue;"
and sty_css_value =  "color:black; font-style: italic;"
and sty_js_op =      "color:darkblue;"
and sty_js_const =     "color:darkred;"
and sty_js_kwd =    "color:#008;"
and tab_size =      8

type tok = | OMarkup of string * tok list | CMarkup of string
	   | Markup of (string * string) * string * tok list
	   | Text of string | Space of int
	   | Com of string | Br of int | Entity of string
	   | String of string | Attr of string | Eq
	   | Css_op of string | Css_id of string | Css_string of string
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
  let par = ref 0 and par_color n = sty_paren.(max 0 (1 + (n mod (Array.length sty_paren - 1))))
  and markup_color n = sty_markup.(max 0 (1 + (n mod (Array.length sty_paren - 1))))in
  let line_ofs = ref 0 in
  let css_step = ref Sec in
  let rec emit = function
    | Com s -> Fragment.append l_fragment (span ~style:sty_comments [string s])
    | OMarkup (s, l) ->
	let par = incr par ; !par - 1 in
	Fragment.append l_fragment (span ~style:(par_color par) [string "<"]) ;
	Fragment.append l_fragment (span ~style:(markup_color par) [string s]) ;
	List.iter emit l ;
	Fragment.append l_fragment (span ~style:(par_color par) [string ">"])
    | Markup ((sc,ec), s, l) ->
	Fragment.append l_fragment (span ~style:(par_color !par) [string ("<" ^ sc)]) ;
	Fragment.append l_fragment (span ~style:(markup_color !par) [string s]) ;
	List.iter emit l ;
	Fragment.append l_fragment (span ~style:(par_color !par) [string (ec ^ ">")])
    | CMarkup s ->
	let par = decr par ; !par in
	Fragment.append l_fragment (span ~style:(par_color par) [string "</"]) ;
	Fragment.append l_fragment (span ~style:(markup_color par) [string s]) ;
	Fragment.append l_fragment (span ~style:(par_color par) [string ">"])
    | Entity s -> Fragment.append l_fragment (span ~style:sty_entity [string s])
    | Br p -> line_ofs := p ; flush p
    | Attr s -> Fragment.append l_fragment (span ~style:sty_attribute [string s])
    | String s -> Fragment.append l_fragment (span ~style:sty_string [string s])
    | Eq -> Fragment.append l_fragment (span ~style:sty_equal [string "="])
    | Text "" ->  ()
    | Text s -> Fragment.append l_fragment (string s)
    | Space n -> Fragment.append l_fragment (string (String.make n ' '))
    | Css_op s ->
	(match s with
	   | "{" | ";" -> css_step := Prop | ":" -> css_step := Val | "}" -> css_step := Sec | _ -> ()) ;
	Fragment.append l_fragment (span ~style:sty_css_op [string s])
    | Css_id s | Css_string s ->
	(match !css_step with
	   | Prop -> Fragment.append l_fragment (span ~style:sty_css_prop [string s])
	   | Val -> Fragment.append l_fragment (span ~style:sty_css_value [string s])
	   | Sec -> Fragment.append l_fragment (span ~style:sty_css_sec [string s]))
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
    let rec lex_c p i =
      if i >= sz then emit (Text (String.sub s p (i - p)))
      else match s.[i] with
	| '<' -> emit (Text (String.sub s p (i - p))) ; lex_m i (i + 1)
	| '&' -> emit (Text (String.sub s p (i - p))) ; lex_e i (i + 1)
	| ' ' | '\t' -> emit (Text (String.sub s p (i - p))) ; lex_s 0 i
	| '\n' -> emit (Text (String.sub s p (i - p))) ; emit (Br i) ; lex_c (i + 1) (i + 1)
	| _ -> lex_c p (i + 1)
    and lex_s n i =
      if i >= sz then () (* EOF spaces *)
      else match s.[i] with
	| ' ' -> lex_s (n + 1) (i + 1)
	| '\t' -> lex_s (n + ((i - !line_ofs) / tab_size + 1) * tab_size) (i + 1)
	| '\n' -> emit (Br (i + 1)) ; lex_s 0 (i + 1) (* EOL spaces *)
	| _ -> emit (Space n) ; lex_c i i
    and lex_m p i =
      if i >= sz then emit (Text "<")
      else match s.[i] with
	  | '/' -> lex_m' "" true (i + 1) (i + 1)
	  | '?' -> lex_m' "?" true (i + 1) (i + 1)
	  | '!' -> lex_m_c (i + 1) (i + 1)
	  | _ -> lex_m' "" false i i
    and lex_m_c p i =
      if i + 1 < sz && s.[i] = '-' && s.[i + 1] = '-' then
	lex_m_c' (i - 2) (i - 2)
      else if i + 7 < sz && String.sub s i 7 = "[CDATA[" then
	lex_m_c'' (i - 2) (i - 2)
      else
	lex_m' "!" true i i
    and lex_m_c' p i =
      if i + 3 >= sz then emit (Com (String.sub s p (i - p))) (* trailing comment *)
      else match s.[i], s.[i + 1], s.[i + 2] with
	| '-', '-', '>' -> emit (Com (String.sub s p (i - p + 3))) ; lex_c (i + 3) (i + 3)
	| '\n', _, _ -> emit (Com (String.sub s p (i - p))) ; emit (Br i) ; lex_m_c' (i + 1) (i + 1)
	| _, _, _ -> lex_m_c' p (i + 1)
    and lex_m_c'' p i =
      if i + 3 >= sz then emit (Com (String.sub s p (i - p))) (* trailing comment *)
      else match s.[i], s.[i + 1], s.[i + 2] with
	| ']', ']', '>' -> emit (Com (String.sub s p (i - p + 3))) ; lex_c (i + 3) (i + 3)
	| '\n', _, _ -> emit (Com (String.sub s p (i - p))) ; emit (Br i) ; lex_m_c'' (i + 1) (i + 1)
	| _, _, _ -> lex_m_c'' p (i + 1)
    and lex_m' sc oc p i =
      if i >= sz then emit (Text ((if oc then "</" else "<") ^ String.sub s p (i - p)))
      else match s.[i] with
	| 'a'..'z' | 'A'..'Z' | '-' | '_' | ':' | '0'..'9' -> lex_m' sc oc p (i + 1)
	| _ -> lex_m'' sc oc (String.sub s p (i - p)) [] i i
    and lex_m'' sc oc n acc p i =
      if i >= sz then emit_markup sc oc n (List.rev acc) i
      else match s.[i] with
	| '/' when i + 1 <= sz && s.[i + 1] = '>' -> emit_markup sc oc n (List.rev acc) (i + 1)
	| '?' when i + 1 <= sz && sc = "?" && s.[i + 1] = '>' -> emit_markup sc oc n (List.rev acc) (i + 1)
	| '>' -> emit_markup sc oc n (List.rev acc) i
	| ' ' | '\t' -> lex_m''_s 0 i (lex_m'' sc oc n) acc
	| '"' -> lex_m''_v i (i + 1) (lex_m'' sc oc n) acc
	| 'a'..'z' | 'A'..'Z' | '_' | '0'..'9' -> lex_m''_i i i (lex_m'' sc oc n) acc
	| '\n' -> line_ofs := (i + 1) ; lex_m'' sc oc n (Br i :: acc) (i + 1) (i + 1)
	| '=' -> lex_m'' sc oc n (Eq :: acc) (i + 1) (i + 1)
	| c -> lex_m'' sc oc n (Text (String.make 1 c) :: acc) (i + 1) (i + 1)
    and lex_m''_s n i cont acc =
      if i >= sz then () (* EOF spaces *)
      else match s.[i] with
	| ' ' -> lex_m''_s (n + 1) (i + 1) cont acc
	| '\t' -> lex_m''_s (n + ((i - !line_ofs) / tab_size + 1) * tab_size) (i + 1) cont acc
	| '\n' -> line_ofs := (i + 1) ; lex_m''_s 0 (i + 1) cont (Br (i + 1) :: acc) (* EOL spaces *)
	| _ -> cont (Space n :: acc) i i
    and lex_m''_i p i cont acc =
      if i >= sz then () (* EOF spaces *)
      else match s.[i] with
	| 'a'..'z' | 'A'..'Z' | '_' | '0'..'9' -> lex_m''_i p (i + 1) cont acc
	| _ -> cont (Attr (String.sub s p (i - p)) :: acc) i i
    and lex_m''_v p i cont acc =
      if i >= sz then () (* EOF spaces *)
      else match s.[i] with
	| '"' -> cont (String (String.sub s p (i - p + 1)) :: acc) (i + 1) (i + 1)
	| '\\' -> lex_m''_v p (i + 2) cont acc
	| _ -> lex_m''_v p (i + 1) cont acc
    and emit_markup sc oc name content i =
      emit (if sc <> "" then
	      Markup ((sc, (if sc = "?" then "?" else "")), name, content)
	    else if s.[i - 1] == '/' then
	      Markup (("", "/"), name, content)
	    else if oc then
	      CMarkup (name)
	    else
	      OMarkup (name, content)) ;
      if (not oc) && name = "style" then
	lex_css (i + 1)
      else if (not oc) && name = "script" then
	lex_js (i + 1)
      else
	lex_c (i + 1) (i + 1)
    and lex_e p i =
      if i >= sz then emit (Text ("&" ^ String.sub s p (i - p)))
      else match s.[i] with
	| ';' -> emit (Entity (String.sub s p (i - p + 1))) ; lex_c (i + 1) (i + 1)
	| _ -> lex_e p (i + 1)
    and lex_css i =
      if i >= sz then lex_c i i
      else match s.[i] with
	| '<' when i + 8 <= sz && String.sub s i 8 = "</style>" -> lex_c i i
	| ' ' | '\t' -> lex_css_s 0 i
	| '"' -> lex_css_s i (i + 1)
	| '{' | '}' | ';' | ':' | '(' | ')' -> emit (Css_op (String.make 1 s.[i])) ; lex_css (i + 1) ;
	| 'a'..'z' | 'A'..'Z' | '_' | '0'..'9' | '.' | '#' -> lex_css_i i (i + 1)
	| '/' when i + 1 <= sz && s.[i + 1] = '*' -> lex_css_coml i i
	| '/' when i + 1 <= sz && s.[i + 1] = '/' -> lex_css_com i i
	| '\n' -> emit (Br i) ; lex_css (i + 1)
	| _ -> emit (Css_id (String.make 1 s.[i])) ; lex_css (i + 1) ;
    and lex_css_com p i =
      if i >= sz then emit (Com (String.sub s p (i - p)))
      else match s.[i] with
	| '\n' -> emit (Com (String.sub s p (i - p))) ; emit (Br (i + 1)) ; lex_css (i + 1)
	| _ -> lex_css_com p (i + 1)
    and lex_css_coml p i =
      if i + 1 >= sz then emit (Com (String.sub s p (i - p)))
      else match s.[i], s.[i + 1] with
	| '*','/' -> emit (Com (String.sub s p (i - p + 2))) ; lex_css (i + 2)
	| '\n',_ -> emit (Com (String.sub s p (i - p))) ; emit (Br (i + 1)) ; lex_css_coml (i + 1) (i + 1)
	| _, _ -> lex_css_coml p (i + 1)
    and lex_css_i p i =
      if i >= sz then emit (Css_id (String.sub s p (i - p)))
      else match s.[i] with 
	| 'a'..'z' | 'A'..'Z' | '_' | '0'..'9' | '-' -> lex_css_i p (i + 1)
	| _ -> emit (Css_id (String.sub s p (i - p))) ; lex_css i
    and lex_css_s n i =
      if i >= sz then () (* EOF spaces *)
      else match s.[i] with
	| ' ' -> lex_css_s (n + 1) (i + 1)
	| '\t' -> lex_css_s (n + ((i - !line_ofs) / tab_size + 1) * tab_size) (i + 1)
	| '\n' -> emit (Br (i + 1)) ; lex_css_s 0 (i + 1) (* EOL spaces *)
	| _ -> emit (Space n) ; lex_css i
    and lex_css_v p i =
      if i >= sz then emit (Css_string (String.sub s p (i - p + 1)))
      else match s.[i] with
	| '"' -> emit (Css_string (String.sub s p (i - p + 1))) ; lex_css (i + 1)
	| '\\' -> lex_css_v p (i + 2)
	| '\n' -> emit (Css_string (String.sub s p (i - p))) ; emit (Br (i + 1)) ; lex_css_v (i + 1) (i + 1)
	| _ -> lex_css_v p (i + 1)
    and lex_js i =
      if i >= sz then lex_c i i
      else match s.[i] with
	| '<' when i + 9 <= sz && String.sub s i 9 = "</script>" -> lex_c i i
	| ' ' | '\t' -> lex_js_s 0 i
	| '"' -> lex_js_v i (i + 1)
	| '\'' -> lex_js_c i (i + 1)
	| '{' | '}' | ';' | ':' | '(' | ')' | '&' | '~' | '|' | '!' | '?' | '.'
	| '+' | '-' | '=' | '<' | '>' | '[' | ']' -> emit (Js_op (String.make 1 s.[i])) ; lex_js (i + 1) ;
	| 'a'..'z' | 'A'..'Z' | '_' -> lex_js_i i (i + 1)
	| '0'..'9' -> lex_js_n i (i + 1)
	| '/' when i + 1 <= sz && s.[i + 1] = '*' -> lex_js_coml i i
	| '/' when i + 1 <= sz && s.[i + 1] = '/' -> lex_js_com i i
	| '\n' -> emit (Br i) ; lex_js (i + 1)
	| _ -> emit (Css_id (String.make 1 s.[i])) ; lex_js (i + 1) ;
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
    in lex_c 0 0
  in
    if infos then (
      let t0 = Sys.time () in
	lex code ;
	emit (Br 0);
	emit (Com (Printf.sprintf "<!-- coloured in %f seconds -->" (Sys.time () -. t0))) ;
	flush 0
    ) else (
      lex code ;
      flush 0
    )
;;

register_syntax "html" pretty_colours ;;
