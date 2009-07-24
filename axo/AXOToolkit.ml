(* This module agregates usual constructions. If code can be factored, the
* common part should be place in the Widgets module *)

open JSOO
open AXOLang

(* Some improved/new widgets *)

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

(********************)
(*** text widgets ***)
(********************)
class virtual generic_text = (* interface *)
object
  inherit AXOWidgets.common
  method virtual get_text : string
  (** get the text content *)

  method virtual set_text : string -> unit
  (** set the content to a new one *)

end
class virtual text_plugin txt = (* plugin *)
object (self)

  inherit AXOWidgets.common
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
  inherit AXOWidgets.common_wrap obj_
  inherit AXOWidgets.widget_plugin
  inherit text_plugin txt
end

class inline_text txt =
object inherit text_wrap  txt (AXOHtml.Low.span ()) end

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

(***************************************)
(*** Container with widget abilities ***)
(***************************************)
class widget_container_wrap obj_ =
object
  inherit AXOWidgets.common_wrap obj_
  inherit AXOWidgets.widget_plugin
  inherit AXOWidgets.container_plugin
end
class block_widget_container = widget_container_wrap (AXOHtml.Low.div ())

(*************************)
(*** Some more buttons ***)
(*************************)
class inline_text_widget_button ?(activated = true) txt =
object
  inherit AXOWidgets.common_wrap (AXOHtml.Low.span ())
  inherit AXOWidgets.widget_plugin
  inherit AXOWidgets.button_plugin activated
  inherit text_plugin txt
end
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
  inherit AXOWidgets.button_plugin activated as b
  inherit text_plugin (cycle ()) as t

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
  let b = new block_container in
object (self)

  inherit AXOWidgets.common
  method obj = b # obj

  val mutable folded_ = folded

  method get_persistent = persistent
  method get_foldable   = foldable

  method private unfold =
    (if persistent_as_container then persistent else foldable)#add_common
      (foldable :> AXOWidgets.common) ;
  method private fold =
    (if persistent_as_container then persistent else foldable)#remove_common
      (foldable :> AXOWidgets.common) ;
    

  initializer
    b # add_common (persistent :> AXOWidgets.common) ;
    (if persistent_as_container
     then persistent#add_common
            ~before:(List.hd persistent#get_content)
            (button :> AXOWidgets.common)
     else b#add_common (button     :> AXOWidgets.common)
    ) ;
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



(*************)
(*** trees ***)
(*************)
let foldable_tree ?(depth = (-1)) ?persistent_as_container tree elements container =
  let rec aux t l container d = (* the core function *)
    let folded = d > depth in
    let (but,com,kid) = elements t l folded
    (* elements produces a (button,
     *                      container (*filled with node's content*)
     *                      container (*filled with node's children*),
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




(**********)
(*** BR ***)
(**********)
class br = object inherit AXOWidgets.common_wrap (AXOHtml.Low.br ()) end

(************)
(*** link ***)
(************)
(*TODO: make a link_widget with multiple inheritance *)
(*TODO: make an image_link *)
class link ?href txt = (*TODO: add set_text and get_text method *)
object (self)

  inherit AXOWidgets.common_wrap
    (AXOHtml.High.a ?href ~children:[AXOHtml.Low.string txt] ())

  method set_href href = self#obj >>> AXOJs.Node.set_attribute "href" href
  method get_href      = self#obj >>> AXOJs.Node.get_attribute "href"

end

(*************)
(*** popup ***)
(*************)
class popup ?(background = "white") ?(place = fun _ -> None) content =
  let c = new block_widget_container in
  let m = new mask in
  let x = new inline_text_widget_button "CLOSE" in
object (self)

  val mutable showed = false

  method show =
    if showed
    then ()
    else (
      ignore (m#auto_set_z_index) ;
      AXOWidgets.body#add_common ( m :> AXOWidgets.common ) ;
      ignore (c#auto_set_z_index) ;
      (match place () with
         | None -> c#set_position AXOStyle.Fixed ; c#set_x 10 ; c#set_y 10 ;
         | Some (p, x, y) ->    c#set_position p ; c#set_x x  ; c#set_y y  ;
      ) ;
      AXOWidgets.body#add_common ( c :> AXOWidgets.common ) ;
      showed <- not showed ;
    )
  method hide =
    if showed
    then (
      AXOWidgets.body#remove_common ( c :> AXOWidgets.common ) ;
      AXOWidgets.body#remove_common ( m :> AXOWidgets.common ) ;
      showed <- not showed ;
    )
    else ()

  initializer
    x#set_position AXOStyle.Absolute ;
    x#set_anti_x 0 ;
    x#add_click_action (fun () -> self#hide) ;
    c#set_background background ;
    c#add_common ( x        :> AXOWidgets.common ) ;
    c#add_common ( (new br) :> AXOWidgets.common ) ;
    c#add_common ( content  :> AXOWidgets.common ) ;

end


(******************)
(*** ul, ol, li ***)
(******************)
class ul_container =
object
  inherit AXOWidgets.common_wrap (AXOHtml.Low.ul ())
  inherit AXOWidgets.container_plugin
end
class li_container =
object
  inherit AXOWidgets.common_wrap (AXOHtml.Low.li ())
  inherit AXOWidgets.container_plugin
end



(*****************)
(*** Drop down ***)
(*****************)
(*TODO : drop down menu *)
