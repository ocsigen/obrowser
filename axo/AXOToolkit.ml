(* This module agregates usual constructions. If code can be factored, the
* common part should be place in the Widgets module *)

open JSOO
open AXOLang

(* Some improved/new widgets *)

(***********************)
(*** Classic widgets ***)
(***********************)
class block_widget =
object
  inherit AXOWidgets.widget_wrap (AXOHtml.Low.div ())
end
class inline_widget =
object
  inherit AXOWidgets.widget_wrap (AXOHtml.Low.span ())
end

(**************************)
(*** Classic containers ***)
(**************************)
class block_container =
object
  inherit AXOWidgets.container_wrap (AXOHtml.Low.div ())
end
class inline_container =
object
  inherit AXOWidgets.container_wrap (AXOHtml.Low.span ())
end

(*************************)
(*** Some more buttons ***)
(*************************)
class cyclic_block_text_button ?(activated = true) hd_txt tl_txt =
  let q =
    let q = Queue.create () in
      List.iter (fun t -> Queue.push t q) ( hd_txt :: tl_txt ) ;
      q
  in
  let cycle () =
    let res = Queue.pop q in
      Queue.push res q ;
      res
  in
object (* OBOB proof *)

  inherit AXOWidgets.common_wrap ( AXOHtml.Low.div () )
  inherit AXOWidgets.button_plugin ~activated () as b
  inherit AXOWidgets.text_plugin (cycle ()) as t

  method clear_click_actions =
    b # clear_click_actions ;
    b # add_click_action
      (fun () -> t # set_text (cycle ()))

  initializer 
    b # add_click_action (fun () -> t # set_text (cycle ()))

end



(******************)
(*** user_input ***)
(******************)
module On_input_change =
  AXOEvents.Make
    ( struct
        type v = string
        let name = "onchange"
        let destruct obj =
          obj >>> AXOEvents.get_target >>> AXOJs.Node.get_attribute "value"
        let default_value = None
      end )

(* select *)
class select =
object

  inherit AXOWidgets.widget_container_wrap (AXOHtml.Low.select ()) as wc

  val mutable option_list = []
  method add_option ?value ?label ?disabled ?selected txt =
    let child =
      new AXOWidgets.common_wrap
        (AXOHtml.High.option ?value ?label ?disabled ?selected txt)
    in
      option_list <- ( child, txt ) :: option_list ;
      wc # add_common child ;
  method remove_option txt =
    let ((child,_), new_option_list) =
      LList.find_remove (fun (_,t) -> t = txt) option_list
    in
      wc # remove_common child ;
      option_list <- new_option_list ;

  method set_editable b =
    if b
    then wc#set_attribute "diasbled" "disabled"
    else wc#remove_attribute "disabled"

end

(* input *)
class [ 'a ] typed_text_input
  ?parse_error_style
  ?parse_error_message
  string_of_t t_of_string (value : 'a) =
object (self)

  inherit AXOWidgets.widget_wrap
         ( AXOHtml.Low.input
             ~attrs:[("type", "text") ; ("value", string_of_t value)]
             ()
         ) as w

  val mutable v = value
  
  method get_value    = v
  method set_value vv = v <- vv ; w # set_attribute "value" (string_of_t vv)

  initializer
    w#obj >>> On_input_change.bind
      (let sty = ref (try w # get_attribute "style" with _ -> "") in
       fun sv ->
         try
           self # set_value (t_of_string sv) ;
           w#set_attribute "style" !sty ;
         with _ ->
           self # set_value v ;
           sty := (try w # get_attribute "style" with _ -> "") ;
           LOption.cb_on_opted (w # set_attribute "style") parse_error_style ;
           LOption.cb_on_opted AXOJs.alert parse_error_message ;
      )

end



(***************)
(*** Folding ***)
(***************)
class block_foldable
  ?(folded = true)
  (button     : AXOWidgets.generic_button   )
  (persistent : AXOWidgets.generic_container)
  (foldable   : AXOWidgets.generic_container)
  =
  let b = new block_container in
object (self)

  inherit AXOWidgets.common
  method obj = b # obj

  val mutable folded_ = folded

  method get_persistent = persistent
  method get_foldable   = foldable

  initializer
    b # add_common (button     :> AXOWidgets.common) ;
    b # add_common (persistent :> AXOWidgets.common) ;
    if folded
    then ()
    else b # add_common (foldable :> AXOWidgets.common) ;
    button # add_click_action
      (fun () ->
         (if folded_
          then ( b # add_common    (foldable :> AXOWidgets.common) ; )
          else ( b # remove_common (foldable :> AXOWidgets.common) ; )
         ) ;
         folded_ <- not folded_ ;
      ) ;

end



(* shadow : use it to show a phantom node of your dragg actions *)
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
class shadow
    ?(style = "background-color: black; opacity: .3")
    widget = (* a shadow takes the imitated widget as argument *)
  let w = new block_widget in
object (self)


  val mutable activated = false

  method private move (x,y) = w#set_x (x + 2) ; w#set_y (y + 2)
  method activate : unit =
    w#set_width widget#get_width ; w#set_height widget#get_height ;
    if not activated
    then (
      AXOWidgets.body#obj >>> Shadow_move.bind (fun (x,y) -> self#move (x,y)) ;
      AXOWidgets.body # add_common (w :> AXOWidgets.common) ;
    )
  method deactivate : unit =
    if activated
    then (
      AXOWidgets.body # remove_common (w :> AXOWidgets.common) ;
      AXOWidgets.body#obj >>> Shadow_move.clear ()
    )

  initializer
    w#set_position AXOStyle.Fixed ;
    w#set_attribute "style" style

end




(* trees *)
let foldable_tree ?(depth = (-1)) tree elements container =
  let before = None in
  let rec aux t l container d =
    let (but,kid,dom) = elements t l in
    let fol = ref (d > depth) in
    let nl =
      List.map
        (fun { LTree.content = t ; LTree.children = l } -> aux t l kid (succ d))
        l
    in
      if !fol then () else container#add_common ?before (dom :> AXOWidgets.common) ;
      but#add_click_action
        (fun _ ->
           if !fol
           then (
             List.iter
               (fun { LTree.content = (_,dom) } ->
                  kid#add_common ?before (dom :> AXOWidgets.common))
               nl ;
             fol := not !fol
           )
           else (
             kid#wipe_content ;
             fol := not !fol
           )
        ) ;
      LTree.node (t,dom) nl
  in aux (LTree.get_content tree) (LTree.get_children tree) container 0

       
