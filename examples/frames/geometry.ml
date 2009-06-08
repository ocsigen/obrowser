open JSOO
open Boot
open Style

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
      | `RELATIVE -> (obj >>> style) # set_position `ABSOLUTE
      | `FIXED | `ABSOLUTE -> () (* respect previous mode *) 
end

let geometry obj =
  try
    Obj.obj (obj >>> get "caml_geometry" >>> as_block)
  with Failure "as_block" ->
    let geometry = new geometry obj in
      obj >>> set "caml_geometry" (inject (Block (Obj.repr geometry))) ;
      geometry
