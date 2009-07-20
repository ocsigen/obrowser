(* This module agregates usual constructions *)

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
  AXOEvents.Make
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
      AXOWidgets.body#obj >>> Shadow_move.bind (fun (x,y) -> self#move (x,y)) ;
      AXOWidgets.body # add_widget (w :> AXOWidgets.generic_widget) ;
    )
  method deactivate : unit =
    if activated
    then (
      AXOWidgets.body # remove_widget (w :> AXOWidgets.generic_widget) ;
      AXOWidgets.body#obj >>> Shadow_move.clear ()
    )

  initializer
    w#set_position AXOStyle.Fixed ;
    w#set_attribute "style" "background-color: black; opacity: .3"

end


(* user_input *)
module On_input_change =
  AXOEvents.Make
    ( struct
        type v = string
        let name = "onchange"
        let destruct obj =
          obj >>> JSOO.get "target" >>> AXOJs.Node.get_attribute "value"
        let default_value = None
      end )

(* select *)
class select =
object

  inherit AXOWidgets.common
  val obj = AXOHtml.Low.select ()
  method obj = obj

  inherit AXOWidgets.widget_plugin as w

  val mutable option_list = []
  method add_option ?value ?label ?disabled ?selected txt =
    let child = AXOHtml.High.option ?value ?label ?disabled ?selected txt in
      option_list <- ( child, txt ) :: option_list ;
      obj >>> AXOJs.Node.append child ;
  method remove_option txt =
    let ((child,_), new_option_list) =
      LList.find_remove (fun (_,t) -> t = txt) option_list
    in
      obj >>> AXOJs.Node.remove child ;
      option_list <- new_option_list ;

  method set_editable b =
    if b
    then w#set_attribute "diasbled" "disabled"
    else w#set_attribute "disabled" ""

end

(* input *)
class [ 'a ] typed_text_input string_of_t t_of_string (value : 'a) =
object (self)

  inherit AXOWidgets.common
  val obj = AXOHtml.Low.input
              ~attrs:[("type", "text") ; ("value", string_of_t value)]
              ()
  method obj = obj

  inherit AXOWidgets.widget_plugin as w

  val mutable v = value
  
  method get_value    = v
  method set_value vv = v <- vv ; w # set_attribute "value" (string_of_t vv)

  initializer
    obj >>> On_input_change.bind
      (let sty = ref (try w # get_attribute "style" with _ -> "") in
       fun sv ->
         try
           v <- t_of_string sv ;
           w#set_attribute "style" !sty ;
         with _ ->
           self # set_value v ;
           w # set_attribute "style" "background-color: red" ;
      )

end

