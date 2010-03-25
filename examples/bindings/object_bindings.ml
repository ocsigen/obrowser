open JSOO

external make_c : Obj.t -> JSOO.obj = "make_c_"

class c = object (self)
  val mutable jso = JSOO.inject Nil
  method f (s : string) : unit =
    jso >>> call_method "f_" [| JSOO.string s |] >>> ignore
  method g (s : string) : unit =
    jso >>> call_method "g_" [| JSOO.string s |] >>> ignore
  initializer
    jso <- make_c (Obj.repr self)
end
