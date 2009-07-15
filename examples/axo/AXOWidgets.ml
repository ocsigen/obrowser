(*This module allow easy creation of graphical widgets*)

open JSOO
open AXOLang

(* Phantom types *)
type generic = [ `Inline | `Block ]



(* Simple widgets *)

class virtual [ 'a ] generic_widget =
object

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

(* block widget *)
class block_widget =
object ( self )

  inherit [ [< `Block ] ] generic_widget
  val obj = AXOHtml.Low.div ()

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
  method get_attribute n   = obj >>> AXOJs.Node.get_attribute n

  method set_position p = (obj >>> AXOStyle.style) # set_position p

end


(* text widgets *)
class virtual [ 'a ] text_widget obj txt =
object ( self )

  inherit [ 'a ] generic_widget
  val mutable text = txt
  val obj = AXOHtml.Low.span ~children:[ AXOJs.Node.text txt] ()

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
  method get_attribute n   = obj >>> AXOJs.Node.get_attribute n

  method get_text            : string = text
  method set_text (t:string) : unit   = text <- t ;
    obj >>> AXOJs.Node.replace_all (AXOJs.Node.text text)

  method set_position p = (obj >>> AXOStyle.style) # set_position p

end
class inline_text_widget text =
object inherit [ [< `Inline ] ] text_widget (AXOHtml.Low.span ()) text end
class block_text_widget text =
object inherit [ [< `Block ] ] text_widget (AXOHtml.Low.div ()) text end

(* button widgets *)
module Button_click =
  AXOEvents.Make (
    struct
       type v = unit
       let name = "onclick"
       let destruct = fun _ -> ()
       let default_value = Some ()
     end)

class virtual [ 'a ] generic_button =
object

  method virtual add_click_action    : (unit -> unit) -> unit
  method virtual remove_click_action : (unit -> unit) -> unit
  method virtual clear_click_action  : unit

  method virtual deactivate : unit
  method virtual activate   : unit

end


(* prepared buttons *)
class inline_text_button ?(activated = true) text =
object
  inherit inline_text_widget text
  inherit [ [< `Inline ] ] generic_button
  
  val mutable actions = []
  val mutable activated_ = activated

  method add_click_action f =
    actions <- f :: (List.filter ((!=) f) actions) ;
    if activated_ then obj >>> Button_click.bind f
  method remove_click_action f =
    actions <- List.filter ((!=) f) actions ;
    if activated_ then obj >>> Button_click.unbind f
  method clear_click_action =
    actions <- [] ;
    if activated_ then obj >>> Button_click.clear ()

  method deactivate =
    if activated_ then obj >>> Button_click.clear () else ()
  method activate =
    if activated_
    then ()
    else List.iter (fun f -> obj >>> Button_click.bind f) actions

end

class block_text_button ?(activated = true) text =
object
  inherit block_text_widget text
  inherit [ [< `Block ] ] generic_button
  
  val mutable actions = []
  val mutable activated_ = activated

  method add_click_action f =
    actions <- f :: (List.filter ((!=) f) actions) ;
    if activated_ then obj >>> Button_click.bind f
  method remove_click_action f =
    actions <- List.filter ((!=) f) actions ;
    if activated_ then obj >>> Button_click.unbind f
  method clear_click_action =
    actions <- [] ;
    if activated_ then obj >>> Button_click.clear ()

  method deactivate =
    if activated_ then obj >>> Button_click.clear () else ()
  method activate =
    if activated_
    then ()
    else List.iter (fun f -> obj >>> Button_click.bind f) actions

end

class [ 'a ] inline_text_cyclic_button ?(activated = true) hd tl =
  let q = Queue.create () in
object (self)

  inherit inline_text_button ~activated (fst hd) as b

  val mutable state : (string * 'a) = hd
  method private cycle =
    state <- Queue.pop q ; q >>> Queue.push state ; b#set_text (fst state) ;
  method get_state = snd state

  method activate =
    if activated_
    then ()
    else
      (List.iter (fun f -> obj >>> Button_click.bind f) actions ;
       obj >>> Button_click.bind (fun () -> self#cycle))

  initializer 
      q >>> Queue.push hd ;
      List.iter (fun e -> q >>> Queue.push e) tl ;
                 
end


(* TODO !!!*)
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

class shadow container = (*TODO : make exception proof !*)
object (self)

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
  method add_obj ?before o = match before with
    | None ->   w#get_obj >>> AXOJs.Node.append o ;
    | Some b -> w#get_obj >>> AXOJs.Node.insert_before o b ;
  method add_widget ?before (wi : widget) =
    let before = LOption.apply_on_opted
                   (fun w -> w#get_obj)
                   (before : widget option)
    in
    self#add_obj ?before wi#get_obj
  method remove_obj o = w#get_obj >>> AXOJs.Node.remove o
  method remove_widget (wi : widget) = self#remove_obj wi#get_obj

  method iter (f : JSOO.obj -> unit)     = w#get_obj >>> AXOJs.Node.iter f
  method iter_rec (f : JSOO.obj -> unit) = w#get_obj >>> AXOJs.Node.iter_rec f


end
let widget_of_container c = (c : container :> widget)

class div_container = container (AXOHtml.Low.div ())
class span_container = container (AXOHtml.Low.span ())
class p_container = container (AXOHtml.Low.p ())

(* trees *)

(*The widget uses a 3-tuple with elements being :
 * 'a : whatever info you want to keep...
 * container : the main container with the displayed DOM part
 * container : the place where children's main containers are *)
class ['a] tree (box : JSOO.obj) (t : ('a * container * container) LTree.tree) =
object (self)

  inherit container box as cont
  val mutable tree  = t
  method get_tree   = tree
  method set_tree t = tree <- t

  initializer
    cont # add_widget
      ((fun (_,main,_) -> widget_of_container main) (LTree.get_content t))

end

class ['b] foldable_tree
 (t : 'a LTree.tree)
 (renderer : 'a -> 'a LTree.tree list -> 'b * widget)
 (leaf_button_maker : unit -> button)
 (node_button_maker : unit -> bool cyclic_button)
  =
  let t_ =
    LTree.map
      (fun c l ->
         let main_c = new container (AXOHtml.Low.li ()) in
         let kids_c = new container (AXOHtml.Low.ul ()) in
         let (node, wid) = renderer c l in
         let but =
           if l = []
           then widget_of_button (leaf_button_maker ())
           else
             (let b = node_button_maker () in
                b#add_click_action
                  (fun () ->
                     if b#get_state
                     then main_c#add_widget (widget_of_container kids_c)
                     else main_c#remove_widget (widget_of_container kids_c)
                  ) ;
                widget_of_cyclic_button b
             )
         in
           main_c#add_widget but ;
           main_c#add_widget wid ;
           ((node, main_c, kids_c),l)
      )
      t
  in
object

  inherit ['b] tree (AXOHtml.Low.ul ()) t_

  initializer
    let before = None in (*for typing*)
    LTree.iter
      (fun (_,_,k) l ->
         List.iter
           (fun { LTree.content = (_,m,_) ; } ->
              k#add_widget ?before (widget_of_container m))
           l
      )
      t_

end


module DnD_up =
  AXOEvents.Make (
    struct
      type v = unit
      let name = "onmouseup"
      let destruct _ = ()
      let default_value = None
    end
)
module DnD_down =
  AXOEvents.Make (
    struct
      type v = unit
      let name = "onmousedown"
      let destruct _ = ()
      let default_value = None
    end
)
class ['b] draggable_tree
  (t : 'a LTree.tree)
  (renderer : 'a -> 'a LTree.tree list ->
     (  (('c * widget * widget) as 'b)
      * widget ))
  (dnd_callback : 'a LTree.tree -> 'a LTree.tree -> 'a LTree.tree -> unit)
  =
  let t_ =
    LTree.map
      (fun c l ->
         let main_c = new container (AXOHtml.Low.li ()) in
         let kids_c = new container (AXOHtml.Low.ul ()) in
         let ((node, dragg, drop), wid) = renderer c l in
           main_c#add_widget wid;
           (((node, dragg, drop), main_c, kids_c), l)
      )
      t
  in
  let shadow =
    let o = AXOHtml.Low.div
              ~attrs:[("style","background: gray; opacity: .3;")]
              ()
    in
    let s = new shadow AXOJs.body o in
      s # set_height 10 ; s # set_width 30 ; s
  in

object

  inherit ['b] tree (AXOHtml.Low.ul ()) t_ as base

  initializer
    let before = None in (*for typing*)
    LTree.iter
      (fun (_,_,k) l ->
         List.iter
           (fun { LTree.content = (_,m,_) ; } ->
              k#add_widget ?before (widget_of_container m))
           l
      )
      t_ ;

    let rec mup_handler downed downed_c uped uped_c =
      fun _ ->
      shadow#deactivate ;
      (try LTree.iter
             (fun ((_,_,drop),_,_) _ -> drop#get_obj >>> DnD_up.clear ())
             base#get_tree
       with exc ->
         AXOJs.debug (Printexc.to_string exc)) ;
      let dnode = LTree.node downed downed_c in
      let unode = LTree.node uped uped_c in
      if uped <> downed && not (LTree.is_in_lineage unode dnode)
      then (
        let da = LTree.map (fun ((a,_,_),_,_) l -> (a,l)) dnode in
        let ua = LTree.map (fun ((a,_,_),_,_) l -> (a,l)) unode in
          (* acting as told *)
        (try
           (try
              dnd_callback
                (LTree.map (fun ((a,_,_),_,_) l -> (a,l)) base#get_tree) da ua ;
            with exc ->
              AXOJs.alert
                ("drag and drop action failed : " ^ Printexc.to_string exc) ;
              raise Interrupted
           ) ;
           (try
              base#set_tree (LTree.move base#get_tree dnode unode) ;
            with exc ->
              AXOJs.alert
                "drag and drop action performed, tree manipulation error.\
                 You may want to reload the page." ;
              raise Interrupted
           ) ;
           (try
              let before = None in (*for typing*)
              (fun (_,m,_) (_,_,k) ->
                 k#add_widget ?before (widget_of_container m))
                downed uped
            with exc ->
              AXOJs.alert
                "drag and drop action performed, DOM manipulation error. \
                 Drag and drop not supported anymore." ;
              LTree.iter
                (fun ((_,d,_),_,_) _ -> d#get_obj >>> DnD_down.clear ())
                base#get_tree ;
           )
         with Interrupted -> ()
        )
      )

  and mdown_handler downed downed_c = fun _ ->
    shadow#activate ;

    LTree.iter
      (fun uped uped_c -> match uped with ((_,_,drop),_,_) ->
        drop#get_obj >>> DnD_up.bind
          (mup_handler downed downed_c uped uped_c);
      )
      base#get_tree ;
  in
    LTree.iter
      (fun r rc -> match r with ((_,dragg,_),_,_) ->
        dragg#get_obj >>> DnD_down.bind (mdown_handler r rc))
      base#get_tree ;


end
