
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
        LTree.node "fadsrgr"
         [ LTree.node "la"
            [ LTree.node "li"
                [ LTree.node "lo"
                    [ LTree.node "lu" []
                    ] ;
                ] ;
            ] ;
         ];
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

let content = new AXOToolkit.block_container

let s = new AXOToolkit.select
let _ = List.map
          (fun t -> s # add_option t)
          ["tata" ; "tete" ; "tyty" ; "tutu" ; "titi" ; "toto" ]
let _ = content # add_common (s :> AXOWidgets.common)

let i = new AXOToolkit.typed_text_input
          ~parse_error_style:"background-color: red; "
          string_of_int int_of_string 0
let _ = i#set_attribute "size" "4"
let _ = content # add_common (i :> AXOWidgets.common)


let f = new AXOToolkit.block_foldable ~folded:false
          ((new AXOToolkit.cyclic_block_text_button "Fold" ["Unfold"])
                                                   :> AXOWidgets.generic_button)
          ((new AXOToolkit.inline_container) :> AXOWidgets.generic_container)
          (content :> AXOWidgets.generic_container)

let _ =
  AXOWidgets.body # add_common (f :> AXOWidgets.common)
