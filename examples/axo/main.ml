
open AXOLang


let body = AXOJs.Node.document >>> AXOJs.Node.get_element_by_id "body"

let test_tree =
  LTree.node "test"
   [ LTree.node "Un"
      [ LTree.node "toto" [] ;
        LTree.node "fait" [] ;
        LTree.node "la"   [] ;
        LTree.node "cuisine" [] ;
      ] ;
     LTree.node "2"
      [ LTree.node "sdjrhg" [] ;
        LTree.node "fadsrgr" [] ;
        LTree.node "cdsfssdise" [] ;
      ] ;
     LTree.node "3"
      [] ;
     LTree.node "Cinq"
      [ LTree.node "sdjrhg" [] ;
        LTree.node "fgr" [] ;
        LTree.node "cdsfssdise"
         [ LTree.node "haha"
            [ LTree.node "hihi"
                [ LTree.node "hoho"
                    [ LTree.node "huhu" []
                    ] ;
                ] ;
            ] ;
         ];
      ] ;
   ]


let renderer x _ = (x, new AXOWidgets.span_text_widget x)
let leaf_button_maker () = new AXOWidgets.text_button ~activated:false "x "
let node_button_maker () =
  new AXOWidgets.cyclic_button (AXOHtml.Low.span ())
    ((AXOHtml.Low.span
        ~attrs:[("style","cursor:crosshair")]
        ~children:[AXOJs.Node.text "> "]
        ()),
     true)
    [((AXOHtml.Low.span
        ~attrs:[("style","cursor:crosshair")]
        ~children:[AXOJs.Node.text "v "]
        ()),
      false)]

let ftree =
  new AXOWidgets.foldable_tree
    test_tree
    renderer
    leaf_button_maker
    node_button_maker

let _ = body >>> AXOJs.Node.append (ftree#get_obj)





let container = AXOHtml.Low.div ()
let parse (_, m) = AXOHtml.Low.div ~children:[AXOJs.Node.text m] ()
let url = "./index.html"
let args = let c = ref 0 in fun () -> incr c ; [("value", string_of_int !c)]
let dl = AXOToolkit.dynamic_list ~container ~parse ~url ~args ()
let _ = body >>> AXOJs.Node.append dl

