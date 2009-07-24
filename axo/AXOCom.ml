(*This module is for using POST and GET method and for general communication
 * with a server*)

open AXOLang

(* internal only : replace every occurence of a char with a string
* TODO : use regexp with escaping for special characters. Current implementation in O(length)*)
let seek_and_destroy seek destroy str =
  let rec aux str =                    
    try                                
      let i = String.index str seek in 
          (String.sub str 0 i)         
        ^ destroy                      
        ^ (aux (String.sub str (succ i) ((String.length str) - (succ i))))
    with Not_found -> str                                                 
  in aux str  


(* internal only : associate a char and it's percent-encoding *)
let percent_assoc = (*/!\ '%' must be first ; ' ' must be after '+' !*)
  [('%', "%25") ; ('!', "%21") ; ('*', "%2A") ; ('"', "%22") ; ('\'', "%27");
   ('(', "%28") ; (')', "%29") ; (';', "%3B") ; (':', "%3A") ; ('@', "%40") ;
   ('&', "%26") ; ('=', "%3D") ; ('+', "%2B") ; ('$', "%24") ; (',', "%2C") ;
   ('/', "%2F") ; ('?', "%3F") ; ('#', "%23") ; ('[', "%5B") ; (']', "%5D") ;
   (' ', "+")   ]

(* internal only : encode a string according to percent-encoding system *)
let urlencode_string str = (*TODO: improve performances ( O(21*length) ~ 0(length) but still...) *)
  List.fold_left (fun a (s,d) -> seek_and_destroy s d a)
    str percent_assoc
   
(* internal only : takes a list of (name,value) and makes it url-friendly*)
let urlencode args =
 String.concat "&"
   (List.map
      (fun (n,v) -> (urlencode_string n) ^ "=" ^ (urlencode_string v))
      args
   )

(* These three functions are HL wrap for POST, GET and mixed HTTP method *)

(** [http_post url args] sends an HTTP POST request to the server with POST
  * arguments [args] nicely encoded and return
  * [(code,message)] where [code] is the HTTP code and [message] the content of
  * the answer. *)
let http_post url args =
  AXOJs.http_post url "application/x-www-form-urlencoded" (urlencode args)

(** [http_get_post url get_args post_args] makes and HTTP POST request with
  * [get_args] encoded and appended to [url] and [post_args] as POST arguments.
  * It's result also is [(code,message)] *)
let http_get_post url get_args post_args =
  AXOJs.http_post
    (url ^ "?" ^ (urlencode get_args))
    "application/x-www-form-urlencode"
    (urlencode post_args)

(** [http_get url args] sends an HTTP GET request with [args] encoded and
  * appended to [url]. Result is identicall to those of [http_post] and
  * [http_get_post]. *)
let http_get url args =
  AXOJs.http_get (url ^ "?" ^ (urlencode args))

(** [alert_on_code (code,message)] makes an alert pop up when [code] is 400 or
* 500. Behaviour for each code type can be set via optional [on_ixx] (where i is
* 1, 2, 3, 4 or 5) arguments. *)
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

(** [dynload_post url args parse] make a post request at [url] with [args] and
  * parse the result using [parse] with the result of the http_post request
  * in case of a 200 return code. If not 200 the default (overidable
  * via [on_ixx] (where i is 1, 3, 4 or 5) optional arguments) behaviour is to
  * fail. *)
let dynload_post url args
      ?(on_1xx = (fun (_,m) -> failwith m))
      ?(on_3xx = (fun (_,m) -> failwith m))
      ?(on_4xx = (fun (_,m) -> failwith m))
      ?(on_5xx = (fun (_,m) -> failwith m))
      parse =
  let (code, msg) as res = http_post url args in
    match code / 100 with
      | 1 -> on_1xx res
      | 2 -> parse msg
      | 3 -> on_3xx res
      | 4 -> on_4xx res
      | 5 -> on_5xx res
      | _ -> AXOJs.alert ("Server sent " ^ (string_of_int code)) ; failwith msg

(** [parse_xml str] makes a DOM tree out of an xml tree using the browser engine
  * Note that it's only experimental and should be tested before serious use. *)
let parse_xml = Js.dom_of_xml

