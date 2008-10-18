open Js ;;
open Html ;;

let sty_odd_line =     "background-color:#fff"
and sty_even_line =    "background-color:#f8f8f8"
and sty_line_numbers = "color:#888"

let progressbar text =
  let background = div ~style:"position:fixed; width: 100%; left: 0px; bottom:0px;
                            \  height: 1em; background-color: white ; border-top: 1px black solid" []
  and bar = div ~style:"position:fixed; width: 0%; left: 0px; bottom:0px; height: 1em;" []
  and text = div ~style:"position:fixed; left: .5em; bottom:.2em; color: black; font-style: italic;
                       \ font-size: 70%;" [string text]
  and body = get_element_by_id "body" in
    Node.append body background ; Node.append body bar ; Node.append body text ;
    (fun pct ->
       if pct >= 100 then (
	 try Node.remove body background ; Node.remove body bar ; Node.remove body text with _ -> ()
       ) else
	 Node.set_attribute bar "style"
	   (Printf.sprintf "position:fixed; width: %d%%; left: 0px; bottom:0px; height: 1em; background-color: #6df" pct))

let colourisers : (string, string -> bool -> Fragment.t -> (int -> unit) -> unit) Hashtbl.t = Hashtbl.create 100 ;;

let register_syntax name cb =
  Hashtbl.replace colourisers name cb
;;

let available_syntaxes () =
  Hashtbl.fold (fun k _ l -> k :: l) colourisers []
;;

let colourise_source lang cdiv code =
  try
    let pretty_colours = Hashtbl.find colourisers lang in
    let fragment = Fragment.create () in 
    let flush i = if i <> 0 then Fragment.append fragment (br ()) in
      Node.empty cdiv ;
      Node.set_attribute cdiv "style" "white-space: pre; font-family: monospace" ;
      pretty_colours code false fragment flush ;
      Node.empty cdiv ;
      Fragment.flush cdiv fragment
  with Not_found -> Node.append cdiv (string code)
;;

let colourise_file  lang cdiv source_file =
  try
    let code = http_get source_file in
      try
	let pct_cb = progressbar "Syntax colouring in progress..." in
	let pretty_colours = Hashtbl.find colourisers lang in
	let fragment = Fragment.create () and l_fragment = Fragment.create () in 
	let line = ref 1 and text_sz = ref (String.length code) and pct_last = ref 0 in
	let flush line_ofs =
	  let l = div
	    ~style:(if !line mod 2 = 0 then sty_odd_line else sty_even_line)
	    [span ~style:sty_line_numbers [string (Printf.sprintf " %03d: " !line)]]
	  in
	    Fragment.flush l l_fragment ;
	    Fragment.append fragment l ;
	    incr line ;
	    let pct = line_ofs * 100 / !text_sz in
	      if pct - !pct_last > 10 then (
		pct_cb (line_ofs * 100 / !text_sz) ;
		pct_last := pct
	      )
	in
	  Node.empty cdiv ;
	  Node.set_attribute cdiv "style" 
	    "white-space: pre; font-family: monospace; 
           \ overflow: auto; height: 15em; border: 1px black solid; padding: 0;
           \ background: none" ;
	  Node.append cdiv (string "Please wait during syntax coloration...") ; Thread.delay 0.01 (* redraw *) ;
	  pretty_colours code true l_fragment flush ;
	  pct_cb 100 ;
	  Node.empty cdiv ;
	  Fragment.flush cdiv fragment
      with Not_found -> Node.append cdiv (string code)
  with e ->
    Node.empty cdiv ;
    Node.append cdiv
      (string ("Unable to colourise '" ^ source_file ^ "' : " ^ (Printexc.to_string e)))
;;
