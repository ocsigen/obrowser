(*This module is for manipulating strings in Obrowser *)



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

