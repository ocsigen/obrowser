(*This module is for Html/DOM manipulation*)

open JSOO
open AXOLang

(*[obj >>> set_attributes attrs] iters [AXOJs.Node.set_attribute] on attrs*)
let set_attributes attrs obj =
  List.iter (fun (n,v) -> obj >>> AXOJs.Node.set_attribute n v) attrs

(*[obj >>> append_children] iters [AXOJs.Node.append] on children*)
let append_children children obj =
  List.iter (fun c -> obj >>> AXOJs.Node.append c) children

(*[smart_create ~name ~attrs ~children ()] create a node with the attributes and
 * children allready set. *)
let smart_create ~name ?(attrs = []) ?(children = []) () =
  let obj = AXOJs.Node.element name in
  obj >>> set_attributes attrs ;
  obj >>> append_children children ;
  obj

(*[sort_children ~node ~comp ()] reorder children of [node] according to
 * [comp] *)
let sort_children ~node ?(comp = compare) () =
  let children = List.sort comp (AXOJs.Node.children node) in
  node >>> append_children children


module Low =
  (* Low level module : creating nodes and manually setting attributes and
   * children. TODO: make exhaustive the set of function *)
struct

  (*Note : frequent use of partial application on smart_create ! *)

  let div         = smart_create ~name:"div"
  let span        = smart_create ~name:"span"
  let p           = smart_create ~name:"p"
  let blockquote  = smart_create ~name:"blockquote"
  let q           = smart_create ~name:"q"
  let pre         = smart_create ~name:"pre"

  let br ()  = smart_create ~name:"br" ()
  let hr     = smart_create ~name:"hr"
  let string = AXOJs.Node.text

  let a = smart_create ~name:"a"

  let ul = smart_create ~name:"ul"
  let ol = smart_create ~name:"ol"
  let li = smart_create ~name:"li"

  let table    = smart_create ~name:"table"
  let caption  = smart_create ~name:"caption"
  let colgroup = smart_create ~name:"colgroup"
  let col      = smart_create ~name:"col"
  let thead    = smart_create ~name:"thead"
  let tbody    = smart_create ~name:"tbody"
  let tr       = smart_create ~name:"tr"
  let th       = smart_create ~name:"th"
  let td       = smart_create ~name:"td"
  let tfoot    = smart_create ~name:"tfoot"

  let h n  = smart_create ~name:("h" ^(string_of_int n))

  let form     = smart_create ~name:"form"
  let option   = smart_create ~name:"option"
  let optgroup = smart_create ~name:"optgroup"
  let select   = smart_create ~name:"select"
  let input    = smart_create ~name:"input"
  let textarea = smart_create ~name:"textarea"
  let button   = smart_create ~name:"button"
  let label    = smart_create ~name:"label"

  let em     = smart_create ~name:"em"
  let strong = smart_create ~name:"strong"
  let dfn    = smart_create ~name:"dfn"
  let code   = smart_create ~name:"code"
  let samp   = smart_create ~name:"samp"
  let kbd    = smart_create ~name:"kbd"
  let var    = smart_create ~name:"var"
  let cite   = smart_create ~name:"cite"

  let del = smart_create ~name:"del"
  let ins = smart_create ~name:"ins"
  let sub = smart_create ~name:"sub"
  let sup = smart_create ~name:"sup"

  let fieldset = smart_create ~name:"fieldset"
  let legend   = smart_create ~name:"legend"

  let img = smart_create ~name:"img"

end 

module High =
  (** High level module : creating nodes with "hints" on specific attributes
  * TODO: make function set exhaustive ; make hints set exhaustive *)
struct

  let set_opt_attr name value obj =
    match value with
      | None -> obj
      | Some v -> obj >>> AXOJs.Node.set_attribute name v ; obj
  let set_attr name value obj =
    obj >>> AXOJs.Node.set_attribute name value ; obj
  let set_opt_attrs attrs obj =
    List.fold_left (fun o (n,v) -> o >>> set_opt_attr n v) obj attrs
  let set_attrs attrs obj =
    List.fold_left (fun o (n,v) -> o >>> set_attr n v) obj attrs

  let a ?href ?name ?target ?attrs ?children () =
    ( Low.a ?attrs ?children () ) >>> set_opt_attrs
        [ "href",href ; "name",name ; "target",target ]
  let img ~src ~alt ?height ?width ?attrs () = 
    ( Low.img ?attrs () ) >>> set_attrs [ "src",src ; "alt",alt ]


  let ul ?attrs lis = Low.ul ?attrs ~children:lis ()
  let ol ?attrs lis = Low.ol ?attrs ~children:lis ()

  let tr ?align ?valign ?attrs tds =
    ( Low.tr ?attrs ~children:tds () ) >>> set_opt_attrs
        [ "align",align ; "valign",valign ]
  let tbody ?align ?valign ?attrs trs =
    ( Low.tbody ?attrs ~children:trs () ) >>> set_opt_attrs
        [ "align",align ; "valign",valign ]
  let col ?align ?valign ?span ?width ?attrs () =
    ( Low.col ?attrs () ) >>> set_opt_attrs
        [ "align",align ; "valign",valign ; "span",span ; "width",width ]
  let colgroup ?align ?valign ?span ?width ?attrs cols =
    ( Low.colgroup ?attrs ~children:cols () ) >>> set_opt_attrs
        [ "align",align ; "valign",valign ; "span",span ; "width",width ]
  let table ?attrs ?caption ?colgroup ?thead ~tbody ?tfoot () =
    Low.table ?attrs
      ~children:(
        LOption.optionnaly_add_to_list
          (LOption.optionnaly_add_to_list
             (LOption.optionnaly_add_to_list [ tbody ] thead)
             colgroup)
          caption
      )
      ()

  let option ?(attrs = []) ?value ?label ?(disabled = false) ?(selected = false)
             txt =
    Low.option
      ~attrs:(
        List.fold_left
          LOption.optionnaly_add_to_list
          attrs
          [ LOption.apply_on_opted (fun v -> ("value",v)) value ;
            LOption.apply_on_opted (fun l -> ("label",l)) label ;
           if disabled then Some ("disabled","disabled") else None ;
           if selected then Some ("selected","selected") else None ;
          ]
      )
      ~children:[Low.string txt]
      ()
  let select ?attrs to_option options =
    Low.select ?attrs ~children:( List.map to_option options ) ()

end



