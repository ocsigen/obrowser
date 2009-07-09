(*This module is for using POST and GET method and for general communication
 * with a server*)

let http_post url args =
  AXOJs.http_post url "application/x-www-form-urlencoded"
    (AXOString.urlencode args)

let http_get url args =
  AXOJs.http_get (url ^ "?" ^ (AXOString.urlencode args))

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
    | n -> AXOJs.alert ("Server sent " ^ (string_of_int (fst res)))



