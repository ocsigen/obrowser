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

  let div  (* ?attrs ?children () *) = smart_create ~name:"div"
  let span (* ?attrs ?children () *) = smart_create ~name:"span"
  let p    (* ?attrs ?children () *) = smart_create ~name:"p"

  let br () = smart_create ~name:"br" ()
  let string = AXOJs.Node.text

  let ul (* ?attrs ?children () *) = smart_create ~name:"ul"
  let ol (* ?attrs ?children () *) = smart_create ~name:"ol"
  let li (* ?attrs ?children () *) = smart_create ~name:"li"

  let table    (* ?attrs ?chidren () *) = smart_create ~name:"table"
  let colgroup (* ?attrs ?chidren () *) = smart_create ~name:"colgroup"
  let col      (* ?attrs ?chidren () *) = smart_create ~name:"col"
  let thead    (* ?attrs ?chidren () *) = smart_create ~name:"thead"
  let tbody    (* ?attrs ?chidren () *) = smart_create ~name:"tbody"
  let tr (* ?attrs ?chidren () *) = smart_create ~name:"tr"
  let th (* ?attrs ?chidren () *) = smart_create ~name:"th"
  let td (* ?attrs ?chidren () *) = smart_create ~name:"td"

  let h n (* ?attrs ?chidren () *) = smart_create ~name:("h" ^(string_of_int n))

  let option (* ?attrs ?children () *) = smart_create ~name:"option"
  let select (* ?attrs ?children () *) = smart_create ~name:"select"
  let input  (* ?attrs ?children () *) = smart_create ~name:"input"
  
end 

module High =
  (** High level module : creating nodes with "hints" on specific attributes
  * TODO: make function set exhaustive ; make hints set exhaustive *)
struct

  let a ?href ?attrs ?children () =
    let obj = smart_create ~name:"a" ?attrs ?children () in
      (match href with
         | None -> ()
         | Some v -> obj >>> AXOJs.Node.set_attribute "href" v ) ;
      obj

  let img ?src ?alt ?attrs () = 
    let obj = smart_create ~name:"img" ?attrs () in
      (match src with
         | None -> ()
         | Some v -> obj >>> AXOJs.Node.set_attribute "src" v ) ;
      (match alt with
         | None -> ()
         | Some v -> obj >>> AXOJs.Node.set_attribute "alt" v ) ;
      obj


  let ul ?attrs lis = Low.ul ?attrs ~children:lis ()
  let ol ?attrs lis = Low.ol ?attrs ~children:lis ()

  let tr ?attrs tds        = Low.tr ?attrs ~children:tds ()
  let tbody ?attrs trs     = Low.tbody ?attrs ~children:trs ()
  let colgroup ?attrs cols = Low.colgroup ?attrs ~children:cols ()
  let table ?attrs ?(colgroup) ?(thead) ~tbody () =
    Low.table ?attrs
      ~children:(
        LOption.optionnaly_add_to_list
          (LOption.optionnaly_add_to_list [ tbody ] thead)
          colgroup
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
      ~children:[AXOJs.Node.text txt]
      ()
  let select ?attrs to_option options =
    Low.select ?attrs ~children:( List.map to_option options ) ()

end



