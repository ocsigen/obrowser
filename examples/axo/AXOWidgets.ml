(*This module allow easy creation of graphical widgets*)

open JSOO

type position =
  | Absolute
  | Fixed
  | Relative

type dim =
  | Px of int
  | Ex of int
  | Em of int
  | Pct of int

let px x = Px x
let ex x = Ex x
let em x = Em x
let pct x = Pct x

type color = string

class style obj =
object
  val sty = obj >>> get "style"

  (* generic methods *)
  method set_dim (n : string) (v : dim) : unit =
    sty >>> set n
      (match v with
	 | Px v -> string (string_of_int v ^ "px")
	 | Ex v -> string (string_of_int v ^ "px")
	 | Em v -> string (string_of_int v ^ "px")
	 | Pct v -> string (string_of_int v ^ "%")
      )

  (* properties *)
  method position : position =
    match sty >>> get "position" >>> as_string with
      | "fixed" -> Fixed
      | "absolute" -> Absolute
      | "relative" -> Relative
      | _ -> Relative
  method set_position (v : position) : unit =
    match v with
      | Fixed -> sty >>> set "position" (string "fixed")
      | Relative -> sty >>> set "position" (string "relative")
      | Absolute -> sty >>> set "position" (string "absolute")

  method z_index : int =
    sty >>> get "zIndex" >>> as_int
  method set_z_index (z : int) : unit =
    sty >>> set "zIndex" (int z)

  method background_color : color =
    sty >>> get "background" >>> as_string
  method set_background_color (c : color) : unit =
    sty >>> set "background" (string c)

end

let style obj =
  try
    Obj.obj (obj >>> get "caml_style" >>> as_block)
  with Failure "as_block" ->
    let style = new style obj in
      obj >>> set "caml_style" (inject (Block (Obj.repr style))) ;
      style


class geometry obj = object (self)
  method width : int =
    obj >>> get "offsetWidth" >>> as_int
  method set_width (w : int) : unit =
    (obj >>> style) # set_dim "width" (px w)
  method height : int =
    obj >>> get "offsetHeight" >>> as_int
  method set_height (h : int) : unit =
    (obj >>> style) # set_dim "height" (px h)
  method x : int =
    obj >>> get "offsetLeft" >>> as_int
  method set_x (x : int) : unit =
    (obj >>> style) # set_dim "left" (px x)
  method y : int =
    obj >>> get "offsetTop" >>> as_int
  method set_y (y : int) : unit =
    (obj >>> style) # set_dim "top" (px y)
  method bounds : int * int * int * int =
    self # x, self #  y, self # width, self # height
  method set_bounds (x, y, w, h) : unit =
    self # set_x x;
    self # set_y y;
    self # set_width w;
    self # set_height h
  initializer
    match (obj >>> style) # position with
      | Relative -> (obj >>> style) # set_position Absolute
      | Fixed | Absolute -> () (* respect previous mode *) 
end

let geometry obj =
  try
    Obj.obj (obj >>> get "caml_geometry" >>> as_block)
  with Failure "as_block" ->
    let geometry = new geometry obj in
      obj >>> set "caml_geometry" (inject (Block (Obj.repr geometry))) ;
      geometry



(* Simple widgets *)

class widget obj =
object ( self )

  method get_obj = obj

  method get_width  : int = obj >>> get "offsetWidth" >>> as_int
  method get_height : int = obj >>> get "offsetHeight" >>> as_int
  method get_x      : int = obj >>> get "offsetLeft" >>> as_int
  method get_y      : int = obj >>> get "offsetTop" >>> as_int

  method set_width  (w : int) : unit = (obj >>> style) # set_dim "width" (px w)
  method set_height (h : int) : unit = (obj >>> style) # set_dim "height" (px h)
  method set_x      (x : int) : unit = (obj >>> style) # set_dim "left" (px x)
  method set_y      (y : int) : unit = (obj >>> style) # set_dim "top" (px y)

  method move_x     (x : int) : unit = self#set_x (self#get_x + x)
  method move_y     (y : int) : unit = self#set_y (self#get_y + y)

  method set_attribute n v = obj >>> AXOJs.Node.set_attribute n v

end

(* text widgets *)
class span_text_widget content =
object
  inherit widget
    (AXOHtml.Low.span ~children:[ AXOJs.Node.text content ] ())
end
class div_text_widget content =
object
  inherit widget
    (AXOHtml.Low.div ~children:[ AXOJs.Node.text content ] ())
end
class p_text_widget content =
object
  inherit widget
    (AXOHtml.Low.p ~children:[ AXOJs.Node.text content ] ())
end

(* button widgets *)
class virtual button content =
object
  inherit widget content as w
  method virtual click : unit
end
class text_button text =
object
  inherit widget
   (AXOHtml.Low.span ~children:[ AXOJs.Node.text text ] ())
   as w
  method click = ()
end
class image_button src ?alt () =
object
  inherit widget (AXOHtml.High.img ~src ?alt ()) as w
  method click = ()
end
class ['a] cyclic_button box hd tl = (*TODO: use queue*)
  let cc = ref ( hd :: tl ) in
object (self)
  val mutable state : (JSOO.obj * 'a) = hd
  method private cycle =
    state <- List.hd !cc ; cc := (List.tl !cc) @ [ List.hd !cc ]

  inherit widget (box >>> AXOJs.Node.append (fst hd) ; box) as w
  method click = self#cycle ;
                 box >>> AXOJs.Node.empty ;
                 box >>> AXOJs.Node.append (fst state) ;

  method get_state = snd state
                 
end


(* Containers *)

class container =
object (self)

  val mutable contained = ([] : widget list)
  inherit widget (AXOHtml.Low.div ()) as widget

  method get_content = contained
  method set_content = contained
  method add_to_content w = contained <- w :: contained
  method iter_on_content f = List.iter f contained

  method move_x x = self#iter_on_content (fun o -> o#move_x x) ; widget#move_x x
  method move_y y = self#iter_on_content (fun o -> o#move_y y) ; widget#move_y y

end



