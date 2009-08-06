(* This module agregates usual constructions. If anything come out to be
* compatible with all of AXOWidgets' widgets, it can (and should) be moved there. *)

open JSOO
open AXOLang


(*********************)
(*** Empty widgets ***)
(*********************)
class block_widget =
object
  inherit AXOWidgets.widget_wrap (AXOHtml.Low.div ())
end
class inline_widget =
object
  inherit AXOWidgets.widget_wrap (AXOHtml.Low.span ())
end


(******************************)
(*** BR & other "constants" ***)
(******************************)
class br =
object inherit AXOWidgets.common_wrap (AXOHtml.Low.br ()) end
class nbsp =
object inherit AXOWidgets.common_wrap (AXOJs.Node.element "&nbsp;") end



(************)
(*** text ***)
(************)
(* simple static text 'common' *)
let text t = new AXOWidgets.common_wrap (AXOJs.Node.text t)

class text_widget_wrap txt obj_ =
object (self)
  inherit AXOWidgets.common_wrap obj_
  inherit AXOWidgets.widget_plugin
  val mutable text = txt

  method get_text   = text
  method set_text t =
    text <- t ;
    self#obj >>> AXOJs.Node.empty ;
    self#obj >>> AXOJs.Node.append ( AXOJs.Node.text t ) ;

  initializer self#obj >>> AXOJs.Node.append (AXOJs.Node.text txt)

end

class inline_widget_text txt =
object inherit text_widget_wrap txt (AXOHtml.Low.span ()) end
class block_widget_text txt =
object inherit text_widget_wrap txt (AXOHtml.Low.div ()) end

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
class vbox =
object ( self )

  inherit AXOWidgets.common_wrap (AXOHtml.Low.div ())
  inherit AXOWidgets.generic_container
  val mutable content = []
  method get_content   = fst (List.split content)
  method wipe_content  = content <- [] ;
                         self#obj >>> AXOJs.Node.empty ;
  method add_common ?before wi =
    let br = new br in
      match before with
        | None ->
            content <-    (wi, br)
                       :: (List.filter (fun (wii,_) -> wii = wi) content);
            self#obj >>> AXOJs.Node.append wi#obj ;
            self#obj >>> AXOJs.Node.append br#obj ;
        | Some wii ->
            content <- LList.insert_after_ content (wi,br)
                         (fun (wiii,_) -> wii = wiii)  ;
            self#obj >>> AXOJs.Node.insert_before wi#obj wii#obj ;
            self#obj >>> AXOJs.Node.insert_before wi#obj br#obj ;
  method remove_common wi =
    let ((wi,br),c) = LList.find_remove (fun (wii,_) -> wi = wii) content in
    content <- c ;
    self#obj >>> AXOJs.Node.remove wi#obj ;
    self#obj >>> AXOJs.Node.remove br#obj ;
end

(***************************************)
(*** Container with widget abilities ***)
(***************************************)
class widget_container_wrap obj_ =
object
  inherit AXOWidgets.common_wrap obj_
  inherit AXOWidgets.container_plugin
  inherit AXOWidgets.widget_plugin
end
class block_widget_container =
object
  inherit widget_container_wrap (AXOHtml.Low.div ())
end
class inline_widget_container =
object
  inherit widget_container_wrap (AXOHtml.Low.div ())
end
class widget_vbox =
object
  inherit vbox
  inherit AXOWidgets.widget_plugin
end

(*************************)
(*** Some more buttons ***)
(*************************)
class inline_text_button ?(activated = true) txt =
object
  inherit text_widget_wrap txt (AXOHtml.Low.span ())
  inherit AXOWidgets.button_plugin activated
end
class inline_text_widget_button ?(activated = true) txt =
object
  inherit text_widget_wrap txt (AXOHtml.Low.span ())
  inherit AXOWidgets.widget_plugin
  inherit AXOWidgets.button_plugin activated
end
class virtual cyclic_text_button_wrap activated hd_txt tl_txt obj_ =
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

  inherit text_widget_wrap (cycle ()) obj_ as t
  inherit AXOWidgets.button_plugin activated as b

  initializer 
    b # add_click_action (fun () -> t # set_text (cycle ()))

end
class cyclic_inline_text_button ?(activated = true) hd_txt tl_txt =
object

  inherit cyclic_text_button_wrap
    activated hd_txt tl_txt (AXOHtml.Low.span ())

end
class cyclic_img_button ?(activated = true) alt hd_srcs tl_srcs =
  let q =
    let q = Queue.create () in
      List.iter (fun t -> Queue.push t q) ( hd_srcs :: tl_srcs ) ;
      q
  in
  let cycle () =
    let res = Queue.pop q in
      Queue.push res q ;
      res
  in
object (self)

  inherit AXOWidgets.common_wrap (AXOHtml.High.img ~src:(cycle ()) ~alt ())
  inherit AXOWidgets.button_plugin activated as b
  inherit AXOWidgets.widget_plugin

  initializer 
    b # add_click_action (fun () -> self#set_attribute "src" (cycle ()))

end
class img_button ?(activated = true) ?(alt = "") src =
object
  inherit AXOWidgets.common_wrap (AXOHtml.High.img ~src ~alt ())
  inherit AXOWidgets.button_plugin activated
  inherit AXOWidgets.widget_plugin
end


(******************)
(*** user_input ***)
(******************)
module On_input_change =
  AXOEvents.Make
    ( struct
        type v = string
        let name = "onchange"
        let name_modifier = Some "_for_input_"
        let destruct obj =
          obj >>> AXOEvents.get_target >>> get "value" >>> as_string
        let default_value = None
      end )

(* select *)
class [ 'a ] select string_of_t t_of_string (value : 'a) alts =
object (self)

  inherit AXOWidgets.widget_container_wrap (AXOHtml.Low.select ()) as wc

  val mutable option_list = []
  val mutable current = value

  method get_value   = current
  method set_value v = wc#set_attribute "value" (string_of_t v) ; current <- v
  method private set_value_ s =
    wc#set_attribute "value" s ; current <- (t_of_string s)

  method add_option value =
    let child = new AXOWidgets.common_wrap
                  (AXOHtml.High.option (string_of_t value) )
    in
      option_list <- ( child, value ) :: option_list ;
      wc # add_common child ;

  method private remove_by f =
    let ( (child,_),new_option_list ) = LList.find_remove f option_list in
      wc # remove_common child ;
      option_list <- new_option_list ;
  method remove_option value =
      self#remove_by (fun (_,v) -> v = value)

  method set_editable b =
    if b
    then try wc#set_attribute "diasbled" "disabled" with _ -> ()
    else try wc#remove_attribute "disabled" with _ -> ()

  initializer
    List.iter (fun v -> self#add_option v) ( value :: alts ) ;
    wc#set_attribute "value" (string_of_t value) ;
    wc#obj >>> On_input_change.bind (fun s -> self#set_value_ s)

end
class [ 'a ] auto_update_select string_of_t t_of_string (value : 'a) alts
  name url args =
object (self)
  inherit [ 'a ] select string_of_t t_of_string value alts as s
  initializer
    s#obj >>> On_input_change.bind
         (fun v ->
            AXOCom.alert_on_code
              ( AXOCom.http_post url
                  ( (name, string_of_t (t_of_string v)) :: args) )
         )

end


(* input *)
class text_input value =
object (self)

  inherit AXOWidgets.widget_wrap
         ( AXOHtml.Low.input
             ~attrs:[("type", "text") ; ("value", value)]
             ()
         )

  val mutable v = value
  method get_value    = v
  method set_value vv = self#set_attribute "value" vv ; v <- vv

  initializer
    self#obj >>> On_input_change.bind (fun sv -> self#set_value sv )

end
class [ 'a ] typed_text_input
  ?parse_error_color
  ?parse_error_message
  ?size
  string_of_t t_of_string (value : 'a) =
object (self)

  inherit AXOWidgets.widget_wrap
         ( AXOHtml.Low.input
             ~attrs:(
               LOption.optionnaly_add_to_list
                 [("type", "text") ; ("value", string_of_t value)]
                 (LOption.apply_on_opted
                    (fun s -> ("size",string_of_int s))
                    size
                 )
             )
             ()
         ) as w

  val mutable v = value
  
  method get_value    = v
  method set_value vv = w#set_attribute "value" (string_of_t vv) ; v <- vv

  initializer
    w#obj >>> On_input_change.bind
      (let bgcol = ref (try w # get_background with _ -> "") in
       fun sv ->
         try
           self # set_value (t_of_string sv) ;
           w#set_background !bgcol ;
         with _ ->
           self # set_value v ;
           bgcol := (try w # get_background with _ -> "") ;
           LOption.cb_on_opted (w # set_background) parse_error_color ;
           LOption.cb_on_opted AXOJs.alert parse_error_message ;
      )

end



(***************)
(*** Folding ***)
(***************)
class block_foldable
  ?(folded = true)
  ?(persistent_as_container = false)
  (button     : AXOWidgets.generic_button   )
  (persistent : AXOWidgets.generic_container)
  (foldable   : AXOWidgets.generic_container)
  =
  let box =
    if persistent_as_container
    then persistent
    else new block_container
  in
object (self)

  inherit AXOWidgets.common
  method obj = box # obj
  inherit AXOWidgets.widget_plugin

  val mutable folded_ = folded

  method get_persistent = persistent
  method get_foldable   = foldable

  method private unfold =
    box#add_common (foldable :> AXOWidgets.common) ;
  method private fold =
    box#remove_common (foldable :> AXOWidgets.common) ;
    

  initializer
      box#add_common
        ?before:(try Some (List.hd box#get_content) with Failure "hd" -> None)
        (button :> AXOWidgets.common)
    ;
    if persistent_as_container
    then ()
    else box#add_common (persistent :> AXOWidgets.common)
    ;
    if folded then () else self#unfold ;
    button # add_click_action
      (fun () ->
         (if folded_
          then self#unfold
          else self#fold
         ) ;
         folded_ <- not folded_ ;
      ) ;

end
class dynamic_block_foldable
  ?(persistent_as_container = false)
  (button     : AXOWidgets.generic_button   )
  (persistent : AXOWidgets.generic_container)
  (foldable   : unit -> AXOWidgets.generic_container)
  =
  let box =
    if persistent_as_container
    then persistent
    else new block_container
  in
object (self)

  method obj = box # obj
  inherit AXOWidgets.widget_plugin

  val mutable folded_ = true
  val mutable current_foldable = None

  method get_persistent = persistent
  method get_last_foldable = current_foldable

  method private unfold =
    current_foldable <- Some (foldable ()) ;
    box#add_common (LOption.unopt current_foldable :> AXOWidgets.common) ;
  method private fold =
    box#remove_common (LOption.unopt current_foldable :> AXOWidgets.common) ;
    

  initializer
      box#add_common
        ?before:(try Some (List.hd box#get_content) with Failure "hd" -> None)
        (button :> AXOWidgets.common)
    ;
    if persistent_as_container
    then ()
    else box#add_common (persistent :> AXOWidgets.common)
    ;
    button # add_click_action
      (fun () ->
         (if folded_
          then self#unfold
          else self#fold
         ) ;
         folded_ <- not folded_ ;
      ) ;

end



(*************)
(*** trees ***)
(*************)
let foldable_tree ?(depth = (-1)) ?persistent_as_container
  tree elements container
  =
  let rec aux t l container d = (* the core function *)
    let folded = d > depth in
    let (but,com,kid) = elements t l folded
    (* elements produces a (button,
     *                      container (* with node's content *)
     *                      container (* for node's children *),
     *                     ) tuple*)
    in
      List.iter
        (fun { LTree.content = t ; LTree.children = l } ->
             kid#add_common ?before:None
             ((aux t l kid (succ d)) :> AXOWidgets.common ))
        l ;
      new block_foldable ~folded ?persistent_as_container but com kid
  in aux (LTree.get_content tree) (LTree.get_children tree) container 0


(************)
(*** Mask ***)
(************)
class mask =
object (self)
  inherit block_widget
  initializer
    self#set_attribute "style"
         "position: fixed; right: 0px; top: 0px; width: 100%; \
          height: 100%; background-color: black; opacity: .5;"
end




(************)
(*** link ***)
(************)
(*TODO: make a link_widget with multiple inheritance *)
class link ?href txt = (*TODO: add set_text and get_text method *)
object (self)

  inherit AXOWidgets.common_wrap
    (AXOHtml.High.a ?href ~children:[AXOHtml.Low.string txt] ())

  method set_href href = self#obj >>> AXOJs.Node.set_attribute "href" href
  method get_href      = self#obj >>> AXOJs.Node.get_attribute "href"

end
class link_widget ?href txt = (*TODO: add set_text and get_text method *)
object (self)

  inherit AXOWidgets.common_wrap
    (AXOHtml.High.a ?href ~children:[AXOHtml.Low.string txt] ())
  inherit AXOWidgets.widget_plugin

  method set_href href = self#obj >>> AXOJs.Node.set_attribute "href" href
  method get_href      = self#obj >>> AXOJs.Node.get_attribute "href"

end
class img_link ?href ~src ~alt =
object (self)

  inherit AXOWidgets.widget_wrap
    (AXOHtml.High.a ?href
       ~children:[AXOHtml.High.img ~src ~alt ()]
       ()
    )

  method set_href href = self#obj >>> AXOJs.Node.set_attribute "href" href
  method get_href      = self#obj >>> AXOJs.Node.get_attribute "href"

end

(*************)
(*** popup ***)
(*************)
class popup ?(background = "white") cont =
  let m = new mask in
  let x = new inline_text_widget_button "CLOSE" in
object (self) (*TODO: restrict access to the container part of self by forcing type in .mli *)

  inherit block_widget_container as c
  val mutable showed = false

  method show =
    if showed
    then ()
    else (
      ignore (m#auto_set_z_index) ;
      AXOWidgets.body#add_common ( m :> AXOWidgets.common ) ;
      ignore (c#auto_set_z_index) ;
      AXOWidgets.body#add_common ( self :> AXOWidgets.common ) ;
      showed <- not showed ;
    )
  method hide =
    if showed
    then (
      AXOWidgets.body#remove_common ( self :> AXOWidgets.common ) ;
      AXOWidgets.body#remove_common ( m :> AXOWidgets.common ) ;
      showed <- not showed ;
    )
    else ()

  initializer
    x#set_position AXOWidgets.Absolute ;
    x#set_anti_x 0 ;
    x#add_click_action (fun () -> self#hide) ;
    c#set_background background ;
    c#set_style_property "padding" "2px" ;
    c#add_common ( x        :> AXOWidgets.common ) ;
    c#add_common ( (new br) :> AXOWidgets.common ) ;
    c#add_common ( cont     :> AXOWidgets.common ) ;

end


(******************)
(*** ul, ol, li ***)
(******************)
class ul_container =
object
  inherit AXOWidgets.common_wrap (AXOHtml.Low.ul ())
  inherit AXOWidgets.container_plugin
end
class ul_widget_container =
object
  inherit AXOWidgets.common_wrap (AXOHtml.Low.ul ())
  inherit AXOWidgets.container_plugin
  inherit AXOWidgets.widget_plugin
end
class li_container =
object
  inherit AXOWidgets.common_wrap (AXOHtml.Low.li ())
  inherit AXOWidgets.container_plugin
end
class li_widget_container =
object
  inherit AXOWidgets.common_wrap (AXOHtml.Low.li ())
  inherit AXOWidgets.container_plugin
  inherit AXOWidgets.widget_plugin
end


(***************)
(*** movable ***)
(***************)
module Movable_move =
  AXOEvents.Make
    (struct
       type v = int * int
       let name = "onmousemove"
       let name_modifier = Some "_for_movable_"
       let destruct obj =
         (obj >>> get "clientX" >>> as_int,
          obj >>> get "clientY" >>> as_int)
       let default_value = None
     end)

module Movable_down =
  AXOEvents.Make
    (struct
       type v = int * int
       let name = "onmousedown"
       let name_modifier = Some "_for_movable_"
       let destruct obj =
         (obj >>> get "clientX" >>> as_int,
          obj >>> get "clientY" >>> as_int)
       let default_value = None
     end)

module Movable_up =
  AXOEvents.Make
    (struct
       type v = int * int
       let name = "onmouseup"
       let name_modifier = Some "_for_movable_"
       let destruct obj =
         (obj >>> get "clientX" >>> as_int,
          obj >>> get "clientY" >>> as_int)
       let default_value = None
     end)
class movable handle content_ =
object

  inherit block_widget_container as container

  (* temporary values to remember the coordinates of the mouse when the move
  * is initiated. *)
  val mutable mx = 0
  val mutable my = 0

  initializer
    (* First we make sure the container and the handle are ok *)
    handle#obj >>> get "style" >>> set "cursor" (JSOO.string "move") ;
    container#add_common (handle   :> AXOWidgets.common) ;
    container#add_common (content_ :> AXOWidgets.common) ;
    container#set_position AXOWidgets.Absolute ;
    container#set_style_property "border" "1px" ;
    (* Then we define the handlers *)
    let rec move_handler (x,y) =
      let vx = x - mx and vy = y - my in
      mx <- x ; my <- y ;
      container#move_x vx ;
      container#move_y vy ;
    and up_handler _ =
      AXOJs.Node.window >>> Movable_move.unbind move_handler ;
      AXOJs.Node.window >>> Movable_up.unbind up_handler ;
    in
    (* Then we bind the mousedown event *)
    handle#obj >>> Movable_down.bind
      (fun (sx,sy) ->
         mx <- sx ; my <- sy ;
         container#auto_set_z_index >>> ignore ;
         AXOJs.Node.window >>> Movable_move.bind move_handler ;
         AXOJs.Node.window >>> Movable_up.bind up_handler
      )

end



(*****************)
(*** Drop down ***)
(*****************)
(*TODO : drop down menu *)
