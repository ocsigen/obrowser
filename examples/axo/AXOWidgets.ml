(*This module allow easy creation of graphical widgets*)

open JSOO


(* Simple widgets *)

class widget obj =
object ( self )

  method get_obj = obj

  method get_width  : int = obj >>> get "offsetWidth"  >>> as_int
  method get_height : int = obj >>> get "offsetHeight" >>> as_int
  method get_x      : int = obj >>> get "offsetLeft"   >>> as_int
  method get_y      : int = obj >>> get "offsetTop"    >>> as_int

  method set_width  (w : int) : unit =
    (obj >>> AXOStyle.style) # set_dim "width" (AXOStyle.px w)
  method set_height (h : int) : unit =
    (obj >>> AXOStyle.style) # set_dim "height" (AXOStyle.px h)
  method set_x      (x : int) : unit =
    (obj >>> AXOStyle.style) # set_dim "left" (AXOStyle.px x)
  method set_y      (y : int) : unit =
    (obj >>> AXOStyle.style) # set_dim "top" (AXOStyle.px y)

  method move_x     (x : int) : unit = self#set_x (self#get_x + x)
  method move_y     (y : int) : unit = self#set_y (self#get_y + y)

  method set_attribute n v = obj >>> AXOJs.Node.set_attribute n v

end

(* text widgets *)
class span_text_widget content =
object
  inherit widget (AXOHtml.Low.span ~children:[ AXOJs.Node.text content ] ())
end
class div_text_widget content =
object
  inherit widget (AXOHtml.Low.div ~children:[ AXOJs.Node.text content ] ())
end
class p_text_widget content =
object
  inherit widget (AXOHtml.Low.p ~children:[ AXOJs.Node.text content ] ())
end

(* button widgets *)
module Button_click =
  AXOEvents.Make (
    struct
       type v = unit
       let name = "onclick"
       let destruct = fun _ -> ()
       let default_value = Some ()
     end)

class virtual button ?(activated = true) content =
object ( self )

  val mutable actions = []
  val mutable activated_ = activated

  inherit widget content as w

  method virtual click : unit
  method add_click_action f =
    actions <- f :: actions ; content >>> Button_click.bind f
  method remove_click_action f =
    actions <- List.filter ((!=) f) actions ; content >>> Button_click.unbind f

  method deactivate =
    if activated_
    then content >>> Button_click.clear ()
    else ()
  method reactivate =
    if activated_
    then ()
    else (content >>> Button_click.bind (fun () -> self#click) ;
          List.iter (fun f -> content >>> Button_click.bind f) actions)
  method activate =
    if activated_
    then ()
    else (actions <- [] ; content >>> Button_click.bind (fun () -> self#click))


  initializer content >>> Button_click.bind (fun () -> self#click)
end

(* prepared buttons *)
class text_button ?activated text =
object
  inherit button
    ?activated
    (AXOHtml.Low.span ~children:[ AXOJs.Node.text text ] ())
    as w
  method click = ()
end

class image_button ?activated src ?alt () =
object
  inherit button ?activated (AXOHtml.High.img ~src ?alt ()) as w
  method click = ()
end

class ['a] cyclic_button ?activated box hd tl =
  let q = Queue.create () in
object (self)
  val mutable state : (JSOO.obj * 'a) = hd
  method private cycle =
    state <- Queue.pop q ; q >>> Queue.add state

  inherit button ?activated (box >>> AXOJs.Node.append (fst hd) ; box) as w
  method click = self#cycle ;
                 box >>> AXOJs.Node.empty ;
                 box >>> AXOJs.Node.append (fst state) ;

  method get_state = snd state

  initializer 
      q >>> Queue.push hd ;
      List.iter (fun e -> q >>> Queue.push e) tl ;
                 
end

(* shadow widget *)
module Mouse_move =
  AXOEvents.Make
    (struct
       type v = int * int
       let name = "onmousemove"
       let destruct obj =
         (obj >>> get "clientX" >>> as_int,
          obj >>> get "clientY" >>> as_int)
       let default_value = None
     end)

class shadow container obj = (*TODO : make exception proof !*)
  let move_handler (mx,my) =
    (obj >>> AXOStyle.style) # set_dim "left" (AXOStyle.px (mx + 2)) ;
    (obj >>> AXOStyle.style) # set_dim "top" (AXOStyle.px (my + 2)) ;
  in
object (self)
  inherit widget obj as w
  method private move (x,y) =
    w#set_x (x + 2) ; w#set_y (y + 2)
  method activate =
    container >>> Mouse_move.bind move_handler ;
    container >>> AXOJs.Node.append obj
  method deactivate =
    container >>> AXOJs.Node.remove obj ;
    container >>> Mouse_move.clear ()

  initializer
    (obj >>> AXOStyle.style) # set_position AXOStyle.Fixed ;

end


(* Containers *)

class container (box : JSOO.obj) =
object (self)

  inherit widget box as w

  method get_content   = w#get_obj >>> AXOJs.Node.children
  method wipe_content  = w#get_obj >>> AXOJs.Node.empty
  method add_to ?before o = match before with
      | None ->   w#get_obj >>> AXOJs.Node.append o ;
      | Some b -> w#get_obj >>> AXOJs.Node.insert_before o b ;
  method remove_from o = w#get_obj >>> AXOJs.Node.remove o

  method iter (f : JSOO.obj -> unit)     = w#get_obj >>> AXOJs.Node.iter f
  method iter_rec (f : JSOO.obj -> unit) = w#get_obj >>> AXOJs.Node.iter_rec f


end
class div_container = container (AXOHtml.Low.div ())
class span_container = container (AXOHtml.Low.span ())
class p_container = container (AXOHtml.Low.p ())



