(**This module allow easy creation of "graphical" widgets*)

(** Each widget kind is organised with :

 a virtual "interface" ( called generic_* ) with detailed comments on expected methods behaviour
 a half virtual plugin ( called *_plugin )
 a wrapper to be used to avoid code duplication in implementation

*)


(* To have an implementation of a widget/button/... one can use

 -the default wrappers :
     [class block_widget = object inherit widget_wrap (AXOHtml.Low.div ()) end
      let w = new block_widget]
  or directly :
     [let w = new widget_wrap (AXOHtml.Low.div ())]

 -the plugins : (using several plugins allow one to get a powerful class/object)
     [class block_text_button_widget activated txt =
        object
          inherit common_wrap (AXOHtml.Low.div ())
          inherit button_plugin activated ()
          inherit text_plugin txt
          inherit widget_plugin
        end
      let tbw = new block_text_button_widget false "tbw"]

 -custom wrappers : (create a wrapper from plugins)
     [class text_button_widget_wrap activated txt obj_ =
        object
          inherit common_wrap obj_
          inherit button_plugin activated ()
          inherit text_plugin txt
          inherit widget_plugin
        end
      let btw = new text_button_widget_wrap true "btw" (AXOHtml.Low.span ())]

 *)

(* /!\       all plugins are compatible with each other       /!\ *)
(* /!\ no plugin should be added if not completely compatible /!\ *)

open JSOO
open AXOLang

(* Common denominator to every class coded here.
*  TODO: use the friend-methods hack showed in the Manual to hide this method *)
class virtual common =
object
  method virtual obj : JSOO.obj
    (** This method should return the DOM object the widget is built upon. It
        should always return the very same object. *)
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
(** The class [widget] provides some attribute manipulation methods. *)
object

  inherit common

  method virtual get_width  : int
  (** the width of the widget (in px) *)

  method virtual get_height : int
  (** the heigth of the widget (in px) *)

  method virtual get_x      : int
  (** the x position of the widget (from the left border) *)

  method virtual get_y      : int
  (** the y position of the widget (from the top border ) *)

  method virtual set_width  : int -> unit
  (** change width *)

  method virtual set_height : int -> unit
  (** change height *)

  method virtual set_x      : int -> unit
  (** change x with 0 being the left most position *)

  method virtual set_anti_x : int -> unit
  (** change x with 0 being the right most position *)

  method virtual set_y      : int -> unit
  (** change y with 0 being the top position *)

  method virtual set_anti_y : int -> unit
 (** change y with 0 being the bottom position *)

  method virtual move_x     : int -> unit
  (** add to x *)

  method virtual move_y     : int -> unit
  (** add to y *)

  method virtual set_attribute : string -> string -> unit
  (** set an attribute *)

  method virtual get_attribute : string -> string
  (** get an attribute *)

  method virtual remove_attribute : string -> unit
  (** remove an attribute *)

  method virtual set_position : AXOStyle.position -> unit
  (** set the position *)

  method virtual set_z_index : int -> unit
  (** Set the zIndex attribute *)

  method virtual get_z_index : int
  (** Get the zIndex attibute *)

  method virtual auto_set_z_index : int
  (** Set the zIndex according to [AXOJs.Misc.new_z_index] and returns it *)

  method virtual set_background : AXOStyle.color -> unit
  (** Set the background color for the widget *)

  method virtual get_background : AXOStyle.color
  (** Get the background color the widget currently has *)

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
  method set_anti_x (x : int) : unit =
    (self#obj >>> AXOStyle.style) # set_dim "right" (AXOStyle.px x)
  method set_y      (y : int) : unit =
    (self#obj >>> AXOStyle.style) # set_dim "top" (AXOStyle.px y)
  method set_anti_y (y : int) : unit =
    (self#obj >>> AXOStyle.style) # set_dim "bottom" (AXOStyle.px y)

  method move_x     (x : int) : unit = self#set_x (self#get_x + x)
  method move_y     (y : int) : unit = self#set_y (self#get_y + y)

  method set_attribute n v  = self#obj >>> AXOJs.Node.set_attribute n v
  method get_attribute n    = self#obj >>> AXOJs.Node.get_attribute n
  method remove_attribute n = self#obj >>> AXOJs.Node.remove_attribute n

  method set_position p   = ( self#obj >>> AXOStyle.style ) # set_position p

  method set_z_index    z = ( self#obj >>> AXOStyle.style ) # set_z_index z
  method get_z_index      = ( self#obj >>> AXOStyle.style ) # z_index
  method auto_set_z_index = ( self#obj >>> AXOStyle.style ) # auto_set_z_index

  method set_background c = (self#obj >>> AXOStyle.style)#set_background_color c
  method get_background   = (self#obj >>> AXOStyle.style)#background_color


end

class widget_wrap obj_ = (* for wraping a JSOO.obj into a widget *)
object
  inherit common_wrap obj_
  inherit widget_plugin
end




(******************)
(*** Containers ***)
(******************)
class virtual generic_container =
(** A [container] is a node where you can put widgets into *)
object

  inherit common

  method virtual get_content  : common list
  (** get the commons previously put in the container *)

  method virtual wipe_content : unit
  (** remove everything from the container *)

  method virtual add_common   : ?before:common -> common -> unit
  (** add a common (or a coerced widget) to the container *)

  method virtual remove_common: common -> unit
  (** remove a common from the container *)

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
(** A [button] is a widget with click capabilities *)
object

  inherit common

  method virtual add_click_action    :(unit -> unit) -> unit
  (** bind an event that will be called when the button is clicked.
      Call order is unspecified. *)

  method virtual remove_click_action :(unit -> unit) -> unit
  (** unbind an event so it won't be called again *)

  method virtual clear_click_actions : unit
  (** unbind all events *)

  method virtual deactivate_button : unit
  (** temporarily deactivate events *)

  method virtual activate_button   : unit
  (** reset deactivated events *)

  method virtual click      : unit
  (** immediatly perform all activated click actions *)

end
class virtual button_plugin activated_ =
object (self)

  inherit common

  inherit generic_button (* for method type checking *)

  val mutable actions = []
  val mutable activated = activated_

  method add_click_action f =
    actions <- f :: (List.filter ((!=) f) actions) ;
    if activated then self#obj >>> Button_click.bind f
  method remove_click_action f =
    actions <- List.filter ((!=) f) actions ;
    if activated then self#obj >>> Button_click.unbind f
  method clear_click_actions =
    actions <- [] ;
    self#obj >>> Button_click.clear ()

  method deactivate_button =
    if activated then self#obj >>> Button_click.clear () else ()
  method activate_button =
    if activated
    then ()
    else List.iter (fun f -> self#obj >>> Button_click.bind f) actions

  method click = List.iter (fun a -> a ()) actions

  initializer
    ( self#obj >>> AXOStyle.style )#set_cursor "pointer"

end

class button_wrap ?(activated = true) obj_ =
object
  inherit common_wrap obj_
  inherit button_plugin activated
end




(*********************)
(*** Drag and drop ***)
(*********************)

module Dragg_n_drop_move =
  AXOEvents.Make
    (struct
       type v = int * int
       let name = "onmousemove"
       let destruct obj =
         (obj >>> get "clientX" >>> as_int,
          obj >>> get "clientY" >>> as_int)
       let default_value = None
     end)
module Dragg_n_drop_down =
  AXOEvents.Make
    (struct
       type v = unit
       let name = "onmousedown"
       let destruct _ = ()
       let default_value = None
     end)
module Dragg_n_drop_up =
  AXOEvents.Make
    (struct
       type v = common
       let name = "onmouseup"
       let destruct obj = new common_wrap (obj >>> AXOEvents.get_target)
       let default_value = None
     end)

(* shadow : to show a phantom copy of your dragged nodes *)
class shadow
    ?(style = "background-color: black; opacity: .3")
    obj = (* a shadow takes the imitated obj as argument *)
  let w = new widget_wrap (AXOHtml.Low.div ()) in
object (self)


  val mutable activated = false

  method private move (x,y) = w#set_x (x + 2) ; w#set_y (y + 2)
  method activate : unit =
    w#set_width  ( obj >>> get "offsetWidth"  >>> as_int ) ;
    w#set_height ( obj >>> get "offsetHeight" >>> as_int ) ;
    if not activated
    then (
      body#obj >>> Dragg_n_drop_move.bind
                                 (fun (x,y) -> self#move (x,y)) ;
      body#add_common (w :> common) ;
    )
  method deactivate : unit =
    if activated
    then (
      body#remove_common (w :> common) ;
      body#obj >>> Dragg_n_drop_move.clear ()
    )

  initializer
    w#set_position AXOStyle.Fixed ;
    w#set_attribute "style" style

end


class virtual generic_dragg =
(** A [dragg] is a widget you can assign droppable widgets to.
    Once a widget is assigned you can dragg and drop the [dragg] to it. *)
object

  inherit common

  method virtual add_drop :
         common -> ( common -> common -> unit ) -> unit
  (** Add a common where it is possible to drop [self] to. The second argument
      is the action to be taken when such a case happen. This callback uses the
      dragged common as first argument and the dropped common as the second
      argument. There can be several actions on the same common.
      Note that actions added while a dragging is in progress won't be triggered
      this one time. The opposite is true for removed actions. *)

  method virtual remove_drop_action :
         common -> ( common -> common -> unit ) -> unit
  (** Removes an action for a particular droppable common. *)

  method virtual remove_drop     : common -> unit
  (** Removes entirely an droppable common and all of his actions. *)

  method virtual deactivate_dragg : unit
  (** Temporarely deactivate dragg. Can be reactivated. *)

  method virtual activate_dragg   : unit
  (** Activate/Reactivate dragg. Note that draggs are not initialized with
      activation, they need to be manually activated. *)

end
class virtual dragg_plugin shadow =
    let rec dragg_begin dragg drop_list shadow () =
      List.iter
        (fun (d,f) ->
           d#obj >>> Dragg_n_drop_up.bind (dragg_end (f dragg) drop_list shadow)
        )
        !drop_list ;
      body#obj >>> Dragg_n_drop_up.bind
        (fun _ ->
           shadow#deactivate ;
           List.iter
             (fun (d,_) -> d#obj >>> Dragg_n_drop_up.clear ())
             !drop_list
        ) ;
      shadow#activate ;
    and dragg_end f drop_list shadow target =
      List.iter ( fun (d,_) -> if d = target then f d else () ) !drop_list ;
    in
object (self)

  inherit common
  inherit generic_dragg

  val drop_list = ref []

  method add_drop d f  =
    drop_list := (d,f) :: !drop_list ;
  method remove_drop d =
    drop_list := List.filter (fun (dd,_) -> dd <> d) !drop_list

  method remove_drop_action d f =
    drop_list := List.filter (fun (dd,ff) -> (dd <> d) && (ff != f)) !drop_list

  method deactivate_dragg =
    self#obj >>> Dragg_n_drop_down.clear ()

  method activate_dragg   =
    self#obj >>> Dragg_n_drop_down.bind
      (dragg_begin (self :> common) drop_list shadow)

  initializer
    (self#obj >>> AXOStyle.style)#set_cursor "move"

end
class dragg_wrap ?shadow_style obj_ =
object
  inherit common_wrap obj_
  inherit dragg_plugin (new shadow ?style:shadow_style obj_)
end

