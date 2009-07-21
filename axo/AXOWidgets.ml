(*This module allow easy creation of graphical widgets*)

(*each widget kind is organised with :
 *
 * a virtual "interface" ( called generic_* ) with detailed comments on expected methods behaviour
 *
 * a half virtual plugin ( called *_plugin )
 *
 * a wrapper to be used to avoid code duplication in implementation *)







(* To have implementation of a widget/button/... one can use

 -the default wrappers :
     class block_widget = object inherit widget_wrap (AXOHtml.Low.div ()) end
     let w = new block_widget

 -the plugins :
     class block_text_button_widget activated txt =
       object
         inherit common_wrap (AXOHtml.Low.div ())
         inherit button_plugin activated ()
         inherit text_plugin txt
         inherit widget_plugin
       end
     let tbw = new block_text_button_widget false "btw"

 -custom wrappers :
     class text_button_widget_wrap activated txt obj_ =
       object
         inherit common_wrap obj_
         inherit button_plugin activated ()
         inherit text_plugin txt
         inherit widget_plugin
       end
     let btw = new text_button_widget_wrap true "btw" (AXOHtml.Low.span ())

 *)

open JSOO
open AXOLang

(* Common denominator to every class coded here.
*  TODO: use the friend-methods hack showed in the Manual to hide this method *)
class virtual common =
object
  method virtual obj : JSOO.obj
    (* This method should return the DOM object the widget is built upon. It
    * should always return the very same object. *)
end

(*This class allow the wraping of an obj into a common-compatible ocamlobject*)
class common_wrap obj_ =
object
  inherit common
  method obj = obj_
end


(**********************)
(*** Simple widgets ***)
(**********************)
class virtual generic_widget = (* interface *)
object

  inherit common

  method virtual get_width  : int (* the width of the widget *)
  method virtual get_height : int (* the heigth of the widget *)
  method virtual get_x      : int (* the x position of the widget *)
  method virtual get_y      : int (* the y position of the widget (starting at the top of the container the obj is in)*)

  method virtual set_width  : int -> unit (* change width *)
  method virtual set_height : int -> unit (* change height *)
  method virtual set_x      : int -> unit (* change x *)
  method virtual set_y      : int -> unit (* change y *)

  method virtual move_x     : int -> unit (* add to x *)
  method virtual move_y     : int -> unit (* add to y *)

  method virtual set_attribute : string -> string -> unit (* set an attribute *)
  method virtual get_attribute : string -> string         (* get an attribute *)
  method virtual remove_attribute : string -> unit     (* remove an attribute *)

  method virtual set_position : AXOStyle.position -> unit (* set the position *)

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

  method set_attribute n v  = self#obj >>> AXOJs.Node.set_attribute n v
  method get_attribute n    = self#obj >>> AXOJs.Node.get_attribute n
  method remove_attribute n = self#obj >>> AXOJs.Node.remove_attribute n

  method set_position p = ((self#obj) >>> AXOStyle.style) # set_position p

end

class widget_wrap obj_ = (* for wraping a JSOO.obj into a widget *)
object
  inherit common_wrap obj_
  inherit widget_plugin
end



(********************)
(*** text widgets ***)
(********************)
class virtual generic_text = (* interface *)
object
  inherit common
  method virtual get_text : string         (* get the text content *)
  method virtual set_text : string -> unit (* set the content to a new one *)
end
class virtual text_plugin txt = (* plugin *)
object (self)

  inherit common
  inherit generic_text
  val mutable text = txt

  method get_text   = text
  method set_text t =
    text <- t ;
    self#obj >>> AXOJs.Node.empty ;
    self#obj >>> AXOJs.Node.append ( AXOJs.Node.text t ) ;

  initializer self#obj >>> AXOJs.Node.append (AXOJs.Node.text txt)

end

class text_wrap txt obj_ =
object
  inherit common_wrap obj_
  inherit widget_plugin
  inherit text_plugin txt
end


(******************)
(*** Containers ***)
(******************)
class virtual generic_container =
object

  inherit common

  method virtual get_content  : common list (* get the common's put in the container *)
  method virtual wipe_content : unit (* remove everything from the container *)
  method virtual add_common   : ?before:common -> common -> unit (* add a common*)
  method virtual remove_common: common -> unit (* remove a common *)

end
class virtual container_plugin =
object (self)

  inherit common
  inherit generic_container

  val mutable content = []
  method get_content   = content
  method wipe_content  = content <- [] ;
                         self#obj >>> AXOJs.Node.empty ;
  method add_common ?before wi = match before with
      | None -> content <- wi :: (List.filter ((!=) wi) content) ;
                self#obj >>> AXOJs.Node.append wi#obj ;
      | Some wii -> content <- LList.insert_after content wi wii ;
                    self#obj >>> AXOJs.Node.insert_before wi#obj wii#obj ;
  method remove_common wi = content <- List.filter ((!=) wi) content ;
                            self#obj >>> AXOJs.Node.remove wi#obj ;
end

class container_wrap obj_ =
object (self)
  inherit common_wrap obj_
  inherit container_plugin
end
let body = new container_wrap AXOJs.Node.body

(*** mixed container and widgets ***)
class widget_container_wrap obj_ =
object
  inherit common_wrap obj_
  inherit container_plugin
  inherit widget_plugin
end


(**************)
(*** button ***)
(**************)
module Button_click = (*TODO: hide this (via .mli) to prevent low level access to button events*)
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

  method virtual add_click_action    :(unit -> unit) -> unit  (*bind an event *)
  method virtual remove_click_action :(unit -> unit) -> unit (*unbind an event*)
  method virtual clear_click_actions : unit              (* unbind all events *)

  method virtual deactivate : unit (* temporarily deactivate events *)
  method virtual activate   : unit (* reset deactivated events *)

end
class virtual button_plugin ?(activated = true) () =
object (self)

  inherit common

  inherit generic_button (* for method type checking *)

  val mutable actions = []
  val mutable activated_ = activated

  method add_click_action f =
    actions <- f :: (List.filter ((!=) f) actions) ;
    if activated_ then self#obj >>> Button_click.bind f
  method remove_click_action f =
    actions <- List.filter ((!=) f) actions ;
    if activated_ then self#obj >>> Button_click.unbind f
  method clear_click_actions =
    actions <- [] ;
    self#obj >>> Button_click.clear ()

  method deactivate =
    if activated_ then self#obj >>> Button_click.clear () else ()
  method activate =
    if activated_
    then ()
    else List.iter (fun f -> self#obj >>> Button_click.bind f) actions

end

class button_wrap ?(activated = true) obj_ =
object
  inherit common_wrap obj_
  inherit button_plugin ~activated ()
end




