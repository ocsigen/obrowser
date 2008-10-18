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
and sty_odd_line =  "background-color:#fff"
and sty_even_line = "background-color:#f8f8f8"
and sty_line_numbers = "color:#888"
and tab_size =      8

type tok = | OMarkup of string * tok list | CMarkup of string
	   | Markup of (string * string) * string * tok list
	   | Text of string | Space of int
	   | Com of string | Br of int | Entity of string
	   | String of string | Attr of string | Eq

let pretty_colours code infos l_fragment flush =
  let par = ref 0
  and par_color n = sty_paren.(max 0 (1 + (n mod (Array.length sty_paren - 1))))
  and markup_color n = sty_markup.(max 0 (1 + (n mod (Array.length sty_paren - 1))))in
  let line_ofs = ref 0 in
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
      lex_c (i + 1) (i + 1)
    and lex_e p i =
      if i >= sz then emit (Text ("&" ^ String.sub s p (i - p)))
      else match s.[i] with
	| ';' -> emit (Entity (String.sub s p (i - p + 1))) ; lex_c (i + 1) (i + 1)
	| _ -> lex_e p (i + 1)
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

register_syntax "xml" pretty_colours ;;
