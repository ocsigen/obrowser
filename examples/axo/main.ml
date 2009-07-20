
open AXOLang


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

let content = new AXOWidgets.block_container

let s = new AXOToolkit.select
let _ = List.map
          (fun t -> s # add_option t)
          ["tata" ; "tete" ; "tyty" ; "tutu" ; "titi" ; "toto" ]
let _ = content # add_widget (s :> AXOWidgets.generic_widget)

let i = new AXOToolkit.typed_text_input string_of_int int_of_string 0
let _ = i#set_attribute "size" "4"
let _ = content # add_widget (i :> AXOWidgets.generic_widget)

let ii = Js.Html.input string_of_int int_of_string 0 10 true (fun _ -> ())
let _ = content # add_widget ((new AXOWidgets.widget_wrap ii.Js.Html.node) :> AXOWidgets.generic_widget)

let f =
  AXOToolkit.foldable ~folded:false
    ~button:(new AXOWidgets.inline_text_button "Expand/collapse")
    (content :> AXOWidgets.generic_widget)

let _ =
  AXOWidgets.body # add_widget (f :> AXOWidgets.generic_widget)
