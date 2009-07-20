(*This module is for Html/DOM manipulation*)

open JSOO
open AXOLang

let set_attributes attrs obj =
  List.iter (fun (n,v) -> obj >>> AXOJs.Node.set_attribute n v) attrs
let append_children children obj =
  List.iter (fun c -> obj >>> AXOJs.Node.append c) children

let smart_create ~name ?(attrs = []) ?(children = []) () =
  let obj = AXOJs.Node.element name in
  obj >>> set_attributes attrs ;
  obj >>> append_children children ;
  obj

let sort_children ~node ?(comp = compare) () =
  let children = List.sort comp (AXOJs.Node.children node) in
  List.iter (fun c -> node >>> AXOJs.Node.remove c) children ;
  node >>> append_children children


module Low =
struct

  let div  (* ?attrs ?children () *) = smart_create ~name:"div"
  let span (* ?attrs ?children () *) = smart_create ~name:"span"
  let p    (* ?attrs ?children () *) = smart_create ~name:"p"

  let br () = smart_create ~name:"br" ()

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
struct

  let a ?href ?attrs ?children () =
    let obj = smart_create ~name:"a" ?attrs ?children () in
      begin
        match href with
          | None -> ()
          | Some v -> obj >>> AXOJs.Node.set_attribute "href" v
      end ;
      obj

  let img ?src ?alt ?attrs () = 
    let obj = smart_create ~name:"img" ?attrs () in
      begin
        match src with
          | None -> ()
          | Some v -> obj >>> AXOJs.Node.set_attribute "src" v ;
      end ;
      begin
        match alt with
          | None -> ()
          | Some v -> obj >>> AXOJs.Node.set_attribute "alt" v ;
      end ;
      obj



  let ul ?attrs lis =
    Low.ul ?attrs ~children:lis ()
  let ol ?attrs lis =
    Low.ol ?attrs ~children:lis ()

  let tr ?attrs to_td tds =
    Low.tr ?attrs ~children:(List.map to_td tds) ()
  let tbody ?attrs to_tr trs =
    Low.tbody ?attrs ~children:(List.map to_tr trs) ()
  let colgroup ?attrs to_col cols =
    Low.colgroup ?attrs ~children:(List.map to_col cols) ()
  let table ?attrs ?(colgroup) ?(thead) ~tbody () =
    Low.table ?attrs
      ~children:(match thead, colgroup with
                   | Some h, Some c -> tbody :: h :: c :: []
                   | Some h, None   -> tbody :: h      :: []
                   | None  , Some c -> tbody      :: c :: []
                   | None,   None   -> tbody           :: [])

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
  let select ?attrs to_option options = (* text, (val,lbl,disabled,selected) *)
    Low.select ?attrs ~children:( List.map to_option options ) ()

end



