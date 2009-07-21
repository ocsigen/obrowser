(*This module is for using POST and GET method and for general communication
 * with a server*)

open AXOLang


let seek_and_destroy seek destroy str =
  Regexp.replace (Regexp.make (string_of_char seek)) destroy str

let percent_assoc = (*/!\ '%' must be first ; ' ' must be after '+' !*)
  [('%', "%25") ; ('!', "%21") ; ('*', "%2A") ; ('"', "%22") ; ('\'', "%27");
   ('(', "%28") ; (')', "%29") ; (';', "%3B") ; (':', "%3A") ; ('@', "%40") ;
   ('&', "%26") ; ('=', "%3D") ; ('+', "%2B") ; ('$', "%24") ; (',', "%2C") ;
   ('/', "%2F") ; ('?', "%3F") ; ('#', "%23") ; ('[', "%5B") ; (']', "%5D") ;
   (' ', "+")   ]

let urlencode_string str = (*TODO: improve performances*)
  (* use [seek_and_destroy] and [percent_assoc] list to encode
   * arguments wrt url-percent-encoding *)
  List.fold_left (fun a (s,d) -> seek_and_destroy s d a)
    str percent_assoc
   
(* takes a list of tuples (name,value) and makes a url-friendly argument *)
let urlencode args =
 String.concat "&"
   (List.map
      (fun (n,v) -> (urlencode_string n) ^ "=" ^ (urlencode_string v))
      args
   )

(* These three functions are high level wrap for POST, GET and mixed HTTP method*)
let http_post url args =
  AXOJs.http_post url "application/x-www-form-urlencoded" (urlencode args)

let http_get_post url get_args post_args =
  AXOJs.http_post
    (url ^ "?" ^ (urlencode get_args))
    "application/x-www-form-urlencode"
    (urlencode post_args)

let http_get url args =
  AXOJs.http_get (url ^ "?" ^ (urlencode args))

(* This function allow automatic HTTP code treatement *)
let alert_on_code
     ?(on_1xx = fun _ -> ())
     ?(on_2xx = fun _ -> ())
     ?(on_3xx = fun _ -> ())
     ?(on_4xx = fun (_,m) -> AXOJs.alert m)
     ?(on_5xx = fun (_,m) -> AXOJs.alert m)
     res =
  match (fst res) / 100 with
    | 1 -> on_1xx res
    | 2 -> on_2xx res
    | 3 -> on_3xx res
    | 4 -> on_4xx res
    | 5 -> on_5xx res
    | _ -> AXOJs.alert ("Server sent " ^ (string_of_int (fst res)))

(* [dynload_post url args] make a post request at [url] with [args] and parse
* the result in case of a 200 return code. If not 200 the default (overidable)
* behaviour is to fail. *)
let dynload_post url args
      ?(on_1xx = (fun (_,m) -> failwith m))
      ?(on_3xx = (fun (_,m) -> failwith m))
      ?(on_4xx = (fun (_,m) -> failwith m))
      ?(on_5xx = (fun (_,m) -> failwith m))
      parse =
  let (code, msg) as res = http_post url args in
    match code / 100 with
      | 1 -> on_1xx res
      | 2 -> parse res
      | 3 -> on_3xx res
      | 4 -> on_4xx res
      | 5 -> on_5xx res
      | _ -> AXOJs.alert ("Server sent " ^ (string_of_int code)) ; failwith msg

(* [parse_xml str] makes a DOM tree out of an xml tree using the browser engine
 * *)
let parse_xml s = Js.dom_of_xml s

