(* Obrowser
 * http://www.ocsigen.org
 * Copyright (C) 2009
 * Raphaël Proust
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)


(** This module allows easy creation of "graphical" widgets *)

(** Each widget kind is organised with :

 a virtual "interface" ( called generic_* ) 
    with detailed comments on expected methods behaviour
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

 This organisation is heavily based on multiple inheritance.

 *)

(* /!\       all plugins are compatible with each other       /!\ *)
(* /!\ no plugin should be added if not completely compatible /!\ *)

open JSOO
open AXOLang

(* Common denominator to every class coded in this module.
 * TODO: use the friend-methods hack showed in the Manual to hide this method *)
class virtual common =
object
  method virtual obj : JSOO.obj
    (** This method should return the DOM object the widget is built upon. It
        should always return the very same object. *)
end

(* This class allow the wraping of an obj into a common-compatible ocamlobject *)
class common_wrap obj_ =
object
  inherit common
  method obj = obj_
end


(** {2 Simple widgets} *)
(** Inheriting from [widget] provides a complete set of methods for attributes
  * and style manipulation. As it is based on Dom/XHTML/CSS properties, one
  * should be familiar with concepts of {em position}, {em Dom tree} and the
  * such. *)

(** {4 types for attribute manipulation } *)

(** Position attribute enumeration *)
type position =
  | Absolute
  | Fixed
  | Relative
let string_of_position = function
  | Absolute -> "absolute"
  | Fixed    -> "fixed"
  | Relative -> "relative"
let position_of_string = function
  | "absolute" -> Absolute
  | "fixed"    -> Fixed
  | _          -> Relative

(** As there are many ways of giving a color proerty in css, it is very hardto
  * give a better type for color. A private type with a Regexp-check constructor
  * could be used... *)
type color = string

let rgb r g b = ((  "rgb("
                  ^ string_of_int r ^ ","
                  ^ string_of_int g ^ ","
                  ^ string_of_int b ^ ")"
                 ) : color)
let hex r g b = (( "#" ^ Printf.sprintf "%X%X%X" r g b) : color )

let px_string i = string_of_int i ^ "px"
let pct_string i = string_of_int i ^ "%"

class virtual generic_widget = (* interface *)
object

  inherit common

(* These methods <s>are</s> were commented to improve speed. It <s>makes</s>
 * made the loading of AXO much faster, but <s>deacreases</s> decreased safety
 * (because there are less wraping for automated type conversion)

 * They now are enabled to stress test the camlinternalOO implementation.

*)

  method virtual get_width  : int
  (** the width of the widget (in px) *)

  method virtual set_width  : int -> unit
  (** change width *)

  method virtual get_height : int
  (** the heigth of the widget (in px) *)

  method virtual set_height : int -> unit
  (** change height *)

  method virtual get_x      : int
  (** the distance between the widget and the left border of the first
  * positioned parent. *)

  method virtual set_x      : int -> unit
  (** set the distance between the widget and the left border of the first
  * positioned parent to the given value. *)

  method virtual set_anti_x : int -> unit
  (** set the distance between the widget and the right border of the first
  * positioned parent to the given value. *)

  method virtual get_y      : int
  (** the distance between the widget and the top border of the first
    * positioned parent *)

  method virtual set_y      : int -> unit
  (** set the distance between the widget and the top border of the first
  * positioned parent to the given value. *)

  method virtual set_anti_y : int -> unit
  (** set the distance between the widget and the bottom border of the first
  * positioned parent to the given value. *)

  method virtual move_x     : int -> unit
  (** increase the distance between the widget and the left border of the first
  * positioned parent by a given amount. It automatically decreases the distance
  * between the widget and the right border of the first positioned parent
  * accordingly. Argument can be less than zero. *)

  method virtual move_y     : int -> unit
  (** increase the distance between the widget and the top border of the first
  * positioned parent by a given amount. It automatically decreases the distance
  * between the widget and the bottom border of the first positioned parent
  * accordingly. Argument can be less than zero. *)

  method virtual set_attribute : string -> string -> unit
  (** [set_attribute name value] sets the attribute [name] to [value]. It is
  * mid-level and should be used carefully. *)

  method virtual get_attribute : string -> string
  (** [get_attribute name] returns the value held by the node's [name]
  * attribute. *)

  method virtual set_position : position -> unit
  (** change the {em position} attribute to the given value. *)

  method virtual get_position : position
  (** get the current value of the {em position} attribute. *)

  method virtual set_z_index : int -> unit
  (** Set the zIndex attribute. Use it to place overlaping widgets above or
  * bellow one another. *)

  method virtual get_z_index : int
  (** Get the zIndex attibute *)

  method virtual auto_set_z_index : int
  (** Set the zIndex according to [AXOJs.Misc.new_z_index] and returns it. If
    * the only method used to set zIndex's is this one, it places the widget on
    * top of the others. *)

  method virtual set_background : color -> unit
  (** Set the background color for the widget *)

  method virtual get_background : color
  (** Get the background color the widget currently has *)

  method virtual set_margin_left : int -> unit
  (** Set the left margin for the widget *)

  method virtual set_margin_right : int -> unit
  (** Set the right margin for the widget *)

  method virtual set_margin_top : int -> unit
  (** Set the top margin for the widget *)

  method virtual set_margin_bottom : int -> unit
  (** Set the bottom margin for the widget *)

  method virtual remove_attribute : string -> unit
  (** [remove_attribute name] wipes the value help by the node's [name]
  * attribute out. *)

  method virtual set_attribute : string -> string -> unit
  (** [set_attribute name value] sets the attribute [name] to [value]. It is
  * mid-level and should be used carefully. *)

  method virtual get_attribute : string -> string
  (** [get_attribute name] returns the value held by the node's [name]
  * attribute. *)

  method virtual set_style_property : string -> string -> unit
  (** [set_style_property name value] set the style property [name] to [value].
    * It's equivalent to the [node.style.name = value] Javascript statement. *)

  method virtual get_style_property : string -> string
  (** [get_style_property name] returns the value of the property whose name
    * matches [name]. *)

end

class virtual widget_plugin = (* plugin *)
  let string_of_px i = string_of_int i ^ "px" in
object (self)

  inherit generic_widget
  method get_width  : int = self#obj >>> get "offsetWidth"  >>> as_int
  method get_height : int = self#obj >>> get "offsetHeight" >>> as_int
  method get_x      : int = self#obj >>> get "offsetLeft"   >>> as_int
  method get_y      : int = self#obj >>> get "offsetTop"    >>> as_int

  method set_width  (w : int) : unit =
    self#set_style_property "width" (string_of_px w)
  method set_height (h : int) : unit =
    self#set_style_property "height" (string_of_px h)
  method set_x      (x : int) : unit =
    self#set_style_property "left" (string_of_px x)
  method set_anti_x (x : int) : unit =
    self#set_style_property "right" (string_of_px x)
  method set_y      (y : int) : unit =
    self#set_style_property "top" (string_of_px y)
  method set_anti_y (y : int) : unit =
    self#set_style_property "bottom" (string_of_px y)

  method move_x     (x : int) : unit = self#set_x (self#get_x + x)
  method move_y     (y : int) : unit = self#set_y (self#get_y + y)

  method set_position p =
    self#set_style_property "position" (string_of_position p)
  method get_position   =
    (self#get_style_property "position") >>> position_of_string

  method set_z_index    z = self#set_style_property "zIndex" (string_of_int z)
  method get_z_index      = self#get_style_property "zIndex" >>> int_of_string
  method auto_set_z_index =
    let z = AXOJs.Misc.new_z_index () in self#set_z_index z ; z

  method set_background c =
    self#set_style_property "background" c
  method get_background   =
    self#get_style_property "background"

  method set_margin_left (m : int) : unit =
    self#set_style_property "marginLeft" (string_of_px m)
  method set_margin_right (m : int) : unit =
    self#set_style_property "marginRight" (string_of_px m)
  method set_margin_top (m : int) : unit =
    self#set_style_property "marginRight" (string_of_px m)
  method set_margin_bottom (m : int) : unit =
    self#set_style_property "marginRight" (string_of_px m)

  method set_attribute n v  = self#obj >>> AXOJs.Node.set_attribute n v
  method get_attribute n    = self#obj >>> AXOJs.Node.get_attribute n
  method remove_attribute n = self#obj >>> AXOJs.Node.remove_attribute n

  method set_style_property name value =
    self#obj >>> get "style" >>> set name (string value)
  method get_style_property name       =
    self#obj >>> get "style" >>> get name >>> as_string

end

(*
class widget_wrap obj_ = (* for wraping a JSOO.obj into a widget *)
object
  inherit common_wrap obj_
  inherit widget_plugin
end
 *)



(******************)
(*** Containers ***)
(******************)
class virtual generic_container =
(** A [container] is a node you can put common's into. It provides *)
object

  inherit common

  method virtual get_content   : common list
  (** get the commons previously put in the container *)

  method virtual wipe_content  : unit
  (** remove everything from the container *)

  method virtual add_common    : ?before:common -> common -> unit
  (** add a common (or a coerced widget) to the container.
      By default it is at the end.
      Use the optional argument [?before] if want to put it somewhere else.
  *)

  method virtual remove_common : common -> unit
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

let body =
  object
    inherit common_wrap AXOJs.Node.body
    inherit container_plugin
  end
(*
class container_wrap obj_ =
object (self)
  inherit common_wrap obj_
  inherit container_plugin
end

(*** mixed container and widgets ***)
class widget_container_wrap obj_ =
object
  inherit common_wrap obj_
  inherit container_plugin
  inherit widget_plugin
end
 *)

(**************)
(*** button ***)
(**************)
module Button_click = (*TODO: hide this (via .mli) to prevent low level access to button events*)
  AXOEvents.Make (
    struct
       type v = unit
       let name = "onclick"
       let name_modifier = Some "_for_buttons_"
       let destruct = fun _ -> ()
       let default_value = Some ()
     end)

class virtual generic_button =
(** A [button] is a widget with click capabilities *)
object

  inherit common

  method virtual add_click_action    : (unit -> unit) -> unit
  (** bind an event that will be called when the button is clicked.
      Call order is unspecified. *)

  method virtual remove_click_action : (unit -> unit) -> unit
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
    self#obj >>> get "style" >>> set "cursor" (JSOO.string "pointer")

end
(*
class button_wrap ?(activated = true) obj_ =
object
  inherit common_wrap obj_
  inherit button_plugin activated
end
 *)



(*********************)
(*** Drag and drop ***)
(*********************)

module Dragg_n_drop_move =
  AXOEvents.Make
    (struct
       type v = int * int
       let name = "onmousemove"
       let name_modifier = Some "_for_dragg_n_drop_"
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
       let name_modifier = Some "_for_dragg_n_drop_"
       let destruct _ = ()
       let default_value = None
     end)

module Dragg_n_drop_up =
  AXOEvents.Make
    (struct
       type v = common
       let name = "onmouseup"
       let name_modifier = Some "_for_dragg_n_drop_"
       let destruct obj = new common_wrap (obj >>> AXOEvents.get_target)
       let default_value = None
     end)

(* shadow : to show a phantom copy of your dragged nodes *)
class shadow
    ?(style = "background-color: black; opacity: .3")
    obj = (* a shadow takes the imitated obj as argument *)
  let w = object
            inherit common_wrap (AXOHtml.Low.div ())
            inherit widget_plugin
          end
  in
object (self)


  val mutable activated = false

  method private move (x,y) =
    w#set_style_property "left" (string_of_int (x + 2) ^ "px") ;
    w#set_style_property "top"  (string_of_int (y + 2) ^ "px") ;

  method activate : unit =
    w#set_style_property "width"
      ( obj >>> get "offsetWidth"  >>> as_int >>> px_string ) ;
    w#set_style_property "height"
      ( obj >>> get "offsetHeight" >>> as_int >>> px_string ) ;
    if not activated
    then (
      body#obj >>> Dragg_n_drop_move.bind (fun (x,y) -> self#move (x,y)) ;
      body#add_common (w :> common) ;
    )

  method deactivate : unit =
    if activated
    then (
      body#remove_common (w :> common) ;
      body#obj >>> Dragg_n_drop_move.clear ()
    )

  initializer
    w#set_attribute "style" style ;
    w#set_style_property "position" (string_of_position Fixed) ;

end


class virtual generic_dragg =
(** A [dragg] is a draggable widget.
    You can assign any widgets as targets to drop the [dragg] into.
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
    self#obj >>> get "style" >>> set "cursor" (JSOO.string "move")

end
(*
class dragg_wrap ?shadow_style obj_ =
object
  inherit common_wrap obj_
  inherit dragg_plugin (new shadow ?style:shadow_style obj_)
end
 *)
