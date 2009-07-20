(*This module allow easy creation of graphical widgets*)
(*each widget kind is organised with :
 * a virtual "interface" ( called generic_* )
 * a half virtual plugin ( called *_plugin )
 * some usual implementations
 *
 * to create a new widget implementation just provide a obj method
 * (as described in common) and plug whatever plugin type you want
 *)

open JSOO
open AXOLang

(* Common denominator to every class coded here *)
class virtual common =
object
  method virtual obj : JSOO.obj
end


(**********************)
(*** Simple widgets ***)
(**********************)
class virtual generic_widget = (* interface *)
object

  inherit common

  method virtual get_width  : int
  method virtual get_height : int
  method virtual get_x      : int
  method virtual get_y      : int

  method virtual set_width  : int -> unit
  method virtual set_height : int -> unit
  method virtual set_x      : int -> unit
  method virtual set_y      : int -> unit

  method virtual move_x     : int -> unit
  method virtual move_y     : int -> unit

  method virtual set_attribute : string -> string -> unit
  method virtual get_attribute : string -> string

  method virtual set_position : AXOStyle.position -> unit

end

class virtual widget_plugin = (* plugin *)
object (self)

  inherit common
  inherit generic_widget
  
  method get_width  : int = self#obj >>> get "offsetWidth"  >>> as_int
  method get_height : int = self#obj >>> get "offsetHeight" >>> as_int
  method get_x      : int = self#obj >>> get "offsetLeft"   >>> as_int
  method get_y      : int = self#obj >>> get "offsetTop"    >>> as_int

  method set_width  (w : int) : unit =
    (self#obj >>> AXOStyle.style) # set_dim "width" (AXOStyle.px w)
  method set_height (h : int) : unit =
    (self#obj >>> AXOStyle.style) # set_dim "height" (AXOStyle.px h)
  method set_x      (x : int) : unit =
    (self#obj >>> AXOStyle.style) # set_dim "left" (AXOStyle.px x)
  method set_y      (y : int) : unit =
    (self#obj >>> AXOStyle.style) # set_dim "top" (AXOStyle.px y)

  method move_x     (x : int) : unit = self#set_x (self#get_x + x)
  method move_y     (y : int) : unit = self#set_y (self#get_y + y)

  method set_attribute n v = (self#obj) >>> AXOJs.Node.set_attribute n v
  method get_attribute n   = (self#obj) >>> AXOJs.Node.get_attribute n

  method set_position p = ((self#obj) >>> AXOStyle.style) # set_position p

end

class block_widget = (* block implementation *)
object ( self )

  inherit common
  val obj = AXOHtml.Low.div ()
  method obj = obj

  inherit widget_plugin

end
class inline_widget = (* inline implementation *)
object ( self )

  inherit common
  val obj = AXOHtml.Low.span ()
  method obj = obj

  inherit widget_plugin

end
class widget_wrap obj_ = (* for wraping a JSOO.obj into a widget *)
object

  inherit common
  val obj = obj_
  method obj = obj

  inherit widget_plugin

end



(********************)
(*** text widgets ***)
(********************)
class virtual generic_text = (* interface *)
object
  inherit common
  method virtual get_text : string
  method virtual set_text : string -> unit
end
class virtual text_plugin ?(txt = "") () = (* plugin *)
object (self)

  inherit common
  inherit generic_text
  val mutable text = txt
  val mutable text_obj = AXOJs.Node.text txt

  method get_text   = text
  method set_text t =
    text <- t ;
    text_obj <- AXOJs.Node.text t ;
    self#obj >>> AXOJs.Node.remove text_obj ;
    self#obj >>> AXOJs.Node.append text_obj ;

  initializer self#obj >>> AXOJs.Node.append (AXOJs.Node.text txt)

end

class inline_text_widget txt =
object
  inherit common
  val obj = AXOHtml.Low.span ()
  method obj = obj

  inherit widget_plugin
  inherit text_plugin ~txt ()
end
class block_text_widget txt =
object
  inherit common
  val obj = AXOHtml.Low.div ()
  method obj = obj

  inherit widget_plugin
  inherit text_plugin ~txt ()
end


(******************)
(*** Containers ***)
(******************)
class virtual generic_container =
object

  inherit common

  method virtual get_content  : generic_widget list
  method virtual wipe_content : unit
  method virtual add_widget   : ?before:generic_widget -> generic_widget -> unit
  method virtual remove_widget: generic_widget -> unit

end
class virtual container_plugin =
object (self)

  inherit common
  inherit generic_container

  val mutable content = []
  method get_content   = content
  method wipe_content  = content <- [] ;
                         self#obj >>> AXOJs.Node.empty ;
  method add_widget ?before wi = match before with
      | None -> content <- wi :: (List.filter ((!=) wi) content) ;
                self#obj >>> AXOJs.Node.append wi#obj ;
      | Some wii -> content <- LList.insert_after content wi wii ;
                    self#obj >>> AXOJs.Node.insert_before wi#obj wii#obj ;
  method remove_widget wi = content <- List.filter ((!=) wi) content ;
                            self#obj >>> AXOJs.Node.remove wi#obj ;
end

let body =
object (self)

  inherit common
  val obj = AXOJs.Node.body
  method obj = obj

  inherit widget_plugin
  inherit container_plugin
end

class inline_container =
object (self)

  inherit common
  val obj = AXOHtml.Low.span ()
  method obj = obj

  inherit widget_plugin
  inherit container_plugin


end
class block_container =
object (self)

  inherit common
  val obj = AXOHtml.Low.div ()
  method obj = obj

  inherit widget_plugin
  inherit container_plugin

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

class virtual generic_button =
object

  inherit common

  method virtual add_click_action    : (unit -> unit) -> unit
  method virtual remove_click_action : (unit -> unit) -> unit
  method virtual clear_click_action  : unit

  method virtual deactivate : unit
  method virtual activate   : unit

end
class virtual button_plugin ?(activated = true) () =
object (self)

  inherit common

  val mutable actions = []
  val mutable activated_ = activated

  method add_click_action f =
    actions <- f :: (List.filter ((!=) f) actions) ;
    if activated_ then self#obj >>> Button_click.bind f
  method remove_click_action f =
    actions <- List.filter ((!=) f) actions ;
    if activated_ then self#obj >>> Button_click.unbind f
  method clear_click_action =
    actions <- [] ;
    if activated_ then self#obj >>> Button_click.clear ()

  method deactivate =
    if activated_ then self#obj >>> Button_click.clear () else ()
  method activate =
    if activated_
    then ()
    else List.iter (fun f -> self#obj >>> Button_click.bind f) actions

end
class inline_button ?(activated = true) () =
object

  inherit common
  val obj = AXOHtml.Low.span ()
  method obj = obj

  inherit widget_plugin
  inherit button_plugin ~activated ()

end
class block_button ?(activated = true) () =
object

  inherit common
  val obj = AXOHtml.Low.span ()
  method obj = obj

  inherit widget_plugin
  inherit button_plugin ~activated ()

end
class inline_text_button ?(activated = true) txt =
object

  inherit common
  val obj = AXOHtml.Low.span ()
  method obj = obj

  inherit widget_plugin
  inherit button_plugin ~activated ()
  inherit text_plugin ~txt ()

end
class block_text_button ?(activated = true) txt =
object

  inherit common
  val obj = AXOHtml.Low.div ()
  method obj = obj

  inherit widget_plugin
  inherit button_plugin ~activated ()
  inherit text_plugin ~txt ()

end
