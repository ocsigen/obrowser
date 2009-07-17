(* This module agregates graphical standard constructions *)

open JSOO
open AXOLang

let foldable ?(folded = true) ~button content =
  let container = new AXOWidgets.block_container in
    container#add_widget
      (button :> AXOWidgets.generic_widget) ;
    if folded then () else container#add_widget content ;
    button#add_click_action
      (let folded = ref folded in
         fun () ->
           (if !folded
            then container#add_widget content
            else container#remove_widget content ) ;
           folded := not !folded ;
      ) ;
    container

(* shadow widget *)
module Shadow_move =
  AXOEvents.Make_for_widget
    (struct
       type v = int * int
       let name = "onmousemove"
       let destruct obj =
         (obj >>> get "clientX" >>> as_int,
          obj >>> get "clientY" >>> as_int)
       let default_value = None
     end)
class shadow widget = (* a shadow takes the imitated widget as argument *)
  let w = new AXOWidgets.block_widget in
object (self)


  val mutable activated = false

  method private move (x,y) = w#set_x (x + 2) ; w#set_y (y + 2)
  method activate : unit =
    w#set_width widget#get_width ; w#set_height widget#get_height ;
    if not activated
    then (
      AXOWidgets.body >>> Shadow_move.bind (fun (x,y) -> self#move (x,y)) ;
      AXOWidgets.body # add_widget (w :> AXOWidgets.generic_widget) ;
    )
  method deactivate : unit =
    if activated
    then (
      AXOWidgets.body # remove_widget (w :> AXOWidgets.generic_widget) ;
      AXOWidgets.body >>> Shadow_move.clear ()
    )

  initializer
    w#set_position AXOStyle.Fixed ;
    w#set_attribute "style" "background-color: black; opacity: .3"

end
