(*This module is for using POST and GET method and for general communication
 * with a server*)

let seek_and_destroy seek destroy str =
  let rec aux str =
    try
      let i = String.index str seek in
          (String.sub str 0 i)
        ^ destroy
        ^ (aux (String.sub str (succ i) ((String.length str) - (succ i))))
    with Not_found -> str
  in aux str

let percent_assoc = (*/!\ '%' must be first ; ' ' must be after '+' !*)
  [('%', "%25") ; ('!', "%21") ; ('*', "%2A") ; ('"', "%22") ; ('\'', "%27");
   ('(', "%28") ; (')', "%29") ; (';', "%3B") ; (':', "%3A") ; ('@', "%40") ;
   ('&', "%26") ; ('=', "%3D") ; ('+', "%2B") ; ('$', "%24") ; (',', "%2C") ;
   ('/', "%2F") ; ('?', "%3F") ; ('#', "%23") ; ('[', "%5B") ; (']', "%5D") ;
   (' ', "+")   ]

let urlencode_string str =
  List.fold_left (fun a (s,d) -> seek_and_destroy s d a)
    str percent_assoc
   
let urlencode args =
 String.concat "&"
   (List.map
      (fun (n,v) -> (urlencode_string n) ^ "=" ^ (urlencode_string v))
      args
   )

let http_post url args =
  AXOJs.http_post url "application/x-www-form-urlencoded" (urlencode args)

let http_get url args =
  AXOJs.http_get (url ^ "?" ^ (urlencode args))

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

let dynload url args
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

let parse_xml s = Js.dom_of_xml s

